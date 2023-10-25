# Function to update table GeoKoder in KHELSA.mdb, using the geo-database in orgdata/Raadataloypa.
# Meant for being used in an RMD-file where parameters and encoding are set. That is where the "headings" come from.
# The function writes the table 'GeoKoder_ny', which can be checked before renaming it to become the sharp table.
# stbj okt-23

#```{r setup}
library(orgdata)
library(haven)
library(tidyverse)
library(DBI)
library(odbc)

# CAVE: The order of loading makes orgdata::read_file() and read_log() 'masked' by the readr:: functions with the same names.
# When using these functions, be specific about which package.

#```{r parametre}
GeoKoderUpdate <- function(aar = NewGeoYear,
                           DBroot = DBrootpath,
                           KH_db = KHELSAfile,
                           GEO_db = GEO_dbfile
){

#aar    <- 2024     #The new Geo-year
#DBroot <- "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING"
#KH_db  <- "KHELSA.mdb"   #SKARP!
#GEO_db <- "raw-khelse/geo-koder.accdb" 

dbdriver	<- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ="

#```{r connection using DBI}
dbname	<- paste(DBroot, GEO_db, sep = "/")
kilde <- DBI::dbConnect(odbc::odbc(),
                        .connection_string = paste0(dbdriver, dbname)
                        #, encoding = "latin1")
)

dbname	<- paste(DBroot, KH_db, sep = "/")  
target 	<- DBI::dbConnect(odbc::odbc(),
                          .connection_string = paste0(dbdriver, dbname)
                          #, encoding = "latin1")
)

# Stop if the target database is write-protected.
# NB: This is NOT a test of whether I have Exclusive access. 
stopifnot(!dbIsReadOnly(target))

#```{r GeoKoder prepping new input}
# Get data
DATA <- DBI::dbGetQuery(kilde, paste("SELECT * FROM tblGeo WHERE validTo = ", "\'", aar, "\'", sep = ""))
# Here _validTo_ is read as Character. Must change into Integer to match GeoKoder when appending.
# I could also have used the _same_ function for reading both DATA and old_GeoKoder ...
DATA <- DATA %>%
    mutate(across(
        .cols = 'validTo',
        .fns = ~ as.integer(.x)))

old_GeoKoder <- DBI::dbReadTable(target, "GeoKoder")    

# Select only valid codes that must be in the target table GeoKoder
newtab <- DATA %>% 
    filter(level != "grunnkrets")

# Recode from string geonivaa (level) to one-letter codes (GEOniv)
newtab <- newtab %>%
    mutate(GEOniv = dplyr::case_when(level == "fylke" ~ "F",
                                     level == "kommune" ~ "K",
                                     level == "bydel" ~ "B",
                                     level == "grunnkrets" ~ "G"))

# Find which codes must be added
# Including GEOniv in 'by' to include the recode of "54" between 2019 and 2020. 
newtab <- dplyr::anti_join(newtab, old_GeoKoder, by = c("code" = "GEO" , "GEOniv") )

# Clean the table structure to match the target
newtab <- newtab %>%
    dplyr::select(-grunnkrets, -kommune, -fylke, -bydel, -batch) %>% #remove columns
    dplyr::rename(NAVN = name) %>%                                   #new column name
    dplyr::rename(FRA = validTo) %>%                                 #ditto, new code is valid starting now
    dplyr::mutate(TIL = 9999)                                        #new column containing last year the code is valid

# Make string geo codes
# Add leading zero if the code has 3-5-7 digits
newtab <- newtab %>%
    mutate(GEO = as.character(code)) %>%      # Make new chr-variable GEO. Must delete 'code' later.
    mutate(GEO = dplyr::if_else(stringr::str_length(GEO) %in% c("1", "3", "5", "7"),    #condition
                                paste("0", GEO, sep = ""),                         #if True
                                GEO))                                              #if False


# Add duplicates with six-digit "sone" code in GEO, and GEOniv == "S", for all K and B
soner <- newtab %>%                                 # Extract K og B, and rebuild the copy into S
    filter(GEOniv %in% c("K", "B")) %>%
    mutate(GEO = dplyr::if_else(stringr::str_length(GEO) == 4,    #condition
                                paste(GEO, "00", sep = ""),       #if True
                                GEO))   %>%                       #if False
    mutate(GEOniv = "S")

newtab <- newtab %>%                                # Append new S-codes
    dplyr::bind_rows(soner)

# Make column TYP with O for normal codes and U for codes that shall not be present in cube files (mainly 99-codes).
# Patterns in stringr are interpreted as Regex. Write them simply as strings inside quotes.
newtab <- newtab %>%
    mutate(TYP = "O") %>%
    mutate(TYP = dplyr::case_when(stringr::str_detect(GEO, ".99$") ~ "U",
                                  str_detect(GEO, ".9900$") ~ "U",
                                  .default = TYP))                  #if none of the above is found

# Clean the dataset and prepare for append to the Access table
# We don't need the ID column, it will be created by Access when writing the table.
newtab <- newtab %>%
    dplyr::select(GEO, NAVN, FRA, TIL, GEOniv, TYP)        # This also gives the column order.

#```{r Read K- and F- source tables}
# Set the previous year as TIL for expired codes.
# To find the expired codes: Read 'geo-koder.accdb' (the source) tables 'kommuneYYYY' and 'fylkeYYYY' 
# for YYYY == <new geo-year>. 
# (there is no 'bydelYYYY', and we do not need to update the grunnkrets table of KHELSA).
# Extract only changeOccurred == YYYY. The sqlQuery does this.
# Expired codes are in oldCode. Must discard any empty oldCode, which are codes that did not change in the year YYYY.
# Among the expired codes, find if any are equal to their currentCode, and remove these.

# In KHELSA table GeoKoder (target) select only valid codes (TIL == 9999).
# The codes matching between these two lists, shall get TIL = YYYY.

# OBS: There are no 2020-tables in the source file GEO_koder.accdb, so "2020" is tricked into reading tables for 2021.

tabaar <- dplyr::if_else(aar == 2020,     #condition
                         2021,            #if True
                         aar)             #if False
kommYYYY <- paste("kommune", tabaar, sep = "")
fylkYYYY <- paste("fylke", tabaar, sep = "")

# Read the two source tables that give the expired codes, and select changes valid for the correct year.

kommtab <- DBI::dbGetQuery(kilde, 
                           paste("SELECT * FROM ", kommYYYY, " WHERE changeOccurred = ", "\'", aar, "\'", sep = ""))
fylktab <- DBI::dbGetQuery(kilde, 
                           paste("SELECT * FROM ", fylkYYYY, " WHERE changeOccurred = ", "\'", aar, "\'", sep = ""))


#```{r GeoKoder clean the old table}
utgaaende <- kommtab %>%
    dplyr::bind_rows(fylktab) %>%         # Append the two tables
    dplyr::filter(!is.na(oldCode)) %>%    # Keep only rows having an oldCode. 
    dplyr::filter(!(oldCode == currentCode)) # Remove any rows where oldCode == currentCode. E.g. "9999" etc. are like this.

utgaaende_K <- utgaaende %>% filter(stringr::str_length(oldCode) == 4) %>%
    select(oldCode)

# Now 'utgaaende' has a list (in oldCode) with all K- and F-codes in KHELSA that shall have their "Valid to" set to last year.
# The municipality codes (Kommunenummerne) are moved into a separate list 'utgaaende_K'.


# Read old table
# We have tables only for K and F (and Grunnkrets) in the norgeo database GEO_koder.accdb.
# KHELSA.mdb has 8-digit codes for Delbydel, which are tagged with GEOniv == "G" in order to be accepted by KHfunctions,
# and 6-digit Soner, and these must be kept. We cannot remove anything, only edit in existing rows.
# So I use a flag column.

old_GeoKoder <- DBI::dbReadTable(target, "GeoKoder")    # Geo is read as string
fjoraar <- aar - 1

# Flag the rows to be edited:
    # UNDERLIG: Med NA og "x" som flagg-verdier, ble alle TIL-verdier i 'False'-grenen satt til NA (altså slettet) 
    # i Bytte-årstall-kommandoen nedenfor. Med numerisk flagg ble TIL-verdiene for 'False' bevart, slik de skulle.
ryddetOld <- old_GeoKoder %>%
    dplyr::mutate(flagg = 999) %>%                                      
    dplyr::mutate(flagg = dplyr::if_else(GEO %in% utgaaende$oldCode,   # All expired codes from the source lists,
                                         1,
                                         flagg)) %>%
    mutate(flagg = if_else((GEOniv %in% c("B", "S") & substr(GEO, 1, 4) %in% utgaaende_K$oldCode), # bydeler and soner having 
                           1,                                                                      # expired kommunenummer.
                           flagg))

# And then change the year in flagged rows:
ryddetOld <- ryddetOld %>%
    mutate(TIL = if_else(flagg == 1,
                         fjoraar,
                         TIL))

# Tidy up, and append new input from the previous chunk
ryddetOld <- ryddetOld %>%
    select(GEO, NAVN, FRA, TIL, GEOniv, TYP) %>%
    dplyr::bind_rows(newtab) %>%
    arrange(GEO, GEOniv, TIL)       # Sorting the table


# Now 'ryddetOld' is ready to be written into Access as table GeoKoder.
# - The old table is preserved, exept that TIL-years are changed into the previous year for all expired codes.
# - New codes are appended.
#
# OBS: HELSEREGIONER is not included in this, so the change from 51-54 to 81-84 in 2020 is not included.
# But in the real (sharp) table, the codes 51-53 are already flagged with TIL == 2019, so they should not be affected anyway.


#```{r Write GeoKoder back using DBI}
dbname	<- paste(DBroot, KH_db, sep = "/") 
dbconn 	<- DBI::dbConnect(odbc::odbc(),
                          .connection_string = paste0(dbdriver, dbname),
                          encoding = "latin1")
tblname 	<- "GeoKoder_ny"

if(dbExistsTable(dbconn, tblname)) DBI::dbRemoveTable(dbconn, tblname)

dbWriteTable(conn = dbconn, name = tblname, value = ryddetOld, batch_rows = 1, row.names = "ID", overwrite = TRUE)
# https://github.com/r-dbi/odbc/issues/263   explains the batch_rows-parameter.

dbDisconnect(dbconn)
return(ryddetOld)
}