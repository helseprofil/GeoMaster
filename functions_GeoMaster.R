library(RODBC)
library(data.table)
library(orgdata)
library(collapse)

# Update to use correct files, default = production files
root <- "O:/Prosjekt/FHP/PRODUKSJON/STYRING"
khelsa <- "KHELSA.mdb"
geokoder <- "raw-khelse/geo-koder.accdb"
# Functions used to update geo tables

#' KnrHarmUpdate
#' 
#' Function updating the KnrHarm table in KHELSA
#'
#' @param year current year
#' @param basepath path to root folder
#' @param khelsapath relative file path to khelsa database
#' @param geokoderpath relative file path to geokoder database
#' @param write option to overwrite table in khelsa
#'
#' @examples
#' KnrHarmUpdate(year = 2024, basepath = root, khelsapath = "KHELSA.mdb", geokoderpath = "raw-khelse/geo-koder.accdb", write = FALSE)
KnrHarmUpdate <- function(year = 2024,
                          basepath = root,
                          khelsapath = khelsa,
                          geokoderpath = geokoder,
                          write = FALSE){
    
    # Connect to databases
    cat("\n Connecting to databases")
    .KHELSA <- RODBC::odbcConnectAccess2007(paste0(basepath, khelsapath))
    .GEOtables <- RODBC::odbcConnectAccess2007(paste0(basepath, geokoderpath))
    
    # Read and format original tables
    cat("\n Read, format, and combine original tables")
    KnrHarm <- addleading0(
        setDT(sqlQuery(.KHELSA, "SELECT * FROM KnrHarm"))
    )
    
    kommunefylke <- addleading0(
        data.table::rbindlist(list(setDT(sqlQuery(.GEOtables, paste0("SELECT oldCode, currentCode, changeOccurred FROM kommune", year))),
                                   setDT(sqlQuery(.GEOtables, paste0("SELECT oldCode, currentCode, changeOccurred FROM fylke", year)))
        )
        )
    )
    setnames(kommunefylke, 
             c("oldCode", "currentCode", "changeOccurred"),
             c("GEO", "GEO_omk", "HARMstd"))
    
    tblGeo <- addleading0(
        setDT(sqlQuery(.GEOtables, paste0("SELECT * FROM tblGeo WHERE validTo = '", year, "' AND level <> 'grunnkrets'")))
    )
    
    # Join the existing KnrHarm and orgdata tables
    comb <- rbindlist(list(KnrHarm, kommunefylke))
    
    # Remove rows where GEO == GEO_omk (which doesn't make sense)
    # Also removes rows where GEO = NA
    comb <- comb[GEO != GEO_omk]
    
    # Remove rows where GEO_omk reappears in GEO due to future recoding
    # e.g. 02 (Akershus) -> 30 (Viken) -> 32 (Akershus)
    cat("\n\n Extracting rows with final recoding (no future recoding)")
    validrecode <- comb[!GEO_omk %in% GEO][order(GEO)]
    
    ### Check invalid recode, including the current recoding of GEO and GEO_omk
    cat("\n For rows with future subsequent recoding, identify the correct current code\n\n")
    invalidrecode <- comb[GEO_omk %in% GEO]
    invalidrecode[validrecode, correct_omk := i.GEO_omk, on = .(GEO = GEO)]
    invalidrecode[validrecode, subsequent_omk := i.GEO_omk, on = .(GEO_omk = GEO)]
    
    ### check if all GEO-codes in invalidrecode does have a correct_omk
    missingrecode <- invalidrecode[is.na(correct_omk)]
    
    if(nrow(missingrecode) > 0){
        
        message(" - The following rows do not have a valid GEO_omk, and missing correct_omk")
        
        print(missingrecode)
        
        addrows <- data.table::data.table(GEO = missingrecode$GEO,
                                          GEO_omk = missingrecode$subsequent_omk,
                                          HARMstd = missingrecode$HARMstd)
        
        validrecode <- data.table::rbindlist(list(validrecode, addrows))
        
        message(" - The following rows are added to final table, where GEO_omk is replaced with subsequent_omk")
        print(addrows)
        
    }
    
    # Only keep unique combinations of GEO and GEO_omk, remove orig column
    out <- unique(validrecode, by = c("GEO", "GEO_omk"))
    
    # Quality control
    
    cat("\n--\nQuality control\n--\n\n")
    
    ### Check whether any rows from the original KnrHarm table is removed and not properly replaced
    KnrHarmRemoved <- KnrHarm[!GEO %in% validrecode$GEO & GEO != GEO_omk]
    
    if(nrow(KnrHarmRemoved) > 0){
        message(" - The following rows are removed from the original KnrHarm table, and not properly replaced")
        KnrHarmRemoved
    } else {
        message(" - All original rows in KnrHarm are kept or properly updated")
    }
    
    ### Check for any missing values in GEO or GEO_omk
    missinggeo <- out[is.na(GEO) | is.na(GEO_omk)]
    if(nrow(missinggeo) > 0){
        message(" - The following rows contain missing values for GEO or GEO_omk")
        missinggeo
    } else {
        message(" - No missing values for GEO and GEO_omk in output table")
    }
    
    # Check that all values in GEO_omk are valid
    # validcodes are fetched from tblGeo, where validTo = year
    validcodes <- tblGeo[validTo == year, (code)]
    if(nrow(out[!GEO_omk %in% validcodes]) > 1){
        message(" - The following rows contain invalid values in GEO_omk")
        print(out[!GEO_omk %in% validcodes])
    } else {
        message(" - All values in GEO_omk are valid for ", year)
    }


    # Write to Access    
    if(write){
        
        # Ask for confirmation before writing
        opts <- c("Overwrite", "Cancel")
        answer <- utils::menu(choices = opts, 
                              title = paste0("Whoops!! You are now replacing the table KnrHarm in:\n\n", 
                                             basepath, khelsapath, 
                                             "\n\nPlease confirm or cancel:"))
    
       if(opts[answer] == "Overwrite"){
           cat("\nUpdating the KnrHarm table in KHELSA...\n")
           RODBC::sqlSave(channel = .KHELSA, 
                          dat = out, 
                          tablename = "KnrHarm", 
                          append = FALSE, 
                          rownames = FALSE, 
                          safer = FALSE)
           cat(paste0("\nDONE! New table written to:\n", basepath, khelsapath, "\n\n"))
           } else {
               cat(paste0("\nYou cancelled, and the table was not overwritten! Puh!\n"))
               }
    }
    
    RODBC::odbcClose(.KHELSA)
    RODBC::odbcClose(.GEOtables)

    return(out)
}


#' GeoKoderUpdate
#' 
#' Function updating the KnrHarm table in KHELSA
#'
#' @param year current year
#' @param basepath root folder
#' @param khelsapath relative path to khelsa database
#' @param geokoderpath relative path to geokoder database
#' @param write option to overwrite the table in khelsa database
#'
#' @examples  GeoKoderUpdate(year = 2024, basepath = root, khelsapath = "KHELSA.mdb", geokoderpath = "raw-khelse/geo-koder.accdb", write = FALSE)
GeoKoderUpdate <- function(year = 2024,
                           basepath = root,
                           khelsapath = khelsa,
                           geokoderpath = geokoder,
                           write = FALSE){
    
    # Connect to databases
    cat("\n Connecting to databases")
    .KHELSA <- RODBC::odbcConnectAccess2007(file.path(basepath, khelsapath))
    .GEOtables <- RODBC::odbcConnectAccess2007(file.path(basepath, geokoderpath))
    
    on.exit(RODBC::odbcClose(.KHELSA), add = T)
    on.exit(RODBC::odbcClose(.GEOtables), add =T)
    
    # Read and format original tables
    cat("\n Read, format, and combine original tables")
    GeoKoder <- addleading0(setDT(sqlQuery(.KHELSA, "SELECT * FROM GeoKoder WHERE GEOniv <> 'S'")))
    
    tblGeo <- addleading0(
        setDT(sqlQuery(.GEOtables, paste0("SELECT [code], [name], [validTo], [level] FROM tblGeo WHERE validTo = '", year, "' AND level NOT IN ('grunnkrets', 'okonomisk')")))
    )
    
    ## Change column names of tblGeo to comply with GeoKoder
    setnames(tblGeo, 
             old = c("code", "level", "name"),
             new = c("GEO", "GEOniv", "NAVN"),
             skip_absent = T)
    
    ## Change GEOniv to F/K/B, add ID and TYP columns
    tblGeo[, `:=` (GEOniv = fcase(GEOniv == "fylke", "F",
                                  GEOniv == "kommune", "K",
                                  GEOniv == "bydel", "B",
                                  GEOniv == "levekaar", "V"),
                   ID = 1,
                   TYP = fcase(grepl("99$", GEO), "U",
                               default = "O"))]
    
    # Identify rows with expired GEO-codes, and set TIL = year - 1
    # Exception for 99, 9999, and 999999
    GeoKoder[(!grepl("99$", GEO) & !GEO %in% tblGeo$GEO & GEOniv %in% c("F", "K", "B") & TIL == 9999) | (GEOniv == "G" & TIL == 9999),
             TIL := year - 1]    
    
    # Identify rows in tblGeo not existing in GeoKoder, based on GEOniv + GEO
    geoexist <- GeoKoder[, .(GEOniv, GEO)][, let(exist = 1)]
    newrows <- collapse::join(tblGeo, geoexist, on = c("GEOniv", "GEO"), how = "left")
    newrows <- newrows[is.na(exist)][, let(exist = NULL)]
    
    ## Set FRA = validTo and TIL = 9999
    ## Change column order to comply with GeoKoder
    setnames(newrows, 
             old = "validTo",
             new = "FRA")
    newrows[, TIL := 9999]
    setcolorder(newrows, names(GeoKoder))
    
    # Add new codes to list
    comb <- data.table::rbindlist(list(GeoKoder, newrows))
    
    # generate rows with GEOniv = S
    sone <- comb[GEOniv %in% c("B", "K") & GEO != "999999"]
    sone[, GEOniv := "S"]
    sone[nchar(GEO) == 4, GEO := paste0(GEO, "00")]
    
    # Generate final output, and add ID column
    out <- data.table::rbindlist(list(comb, sone))[order(GEO)]
    out[, ID := .I]
    
    # Quality control
    
    cat("\n--\nQuality control\n--\n\n")
    
    ## Check that all values of GEO (F, K, B) with TYP == "O" are valid codes for current year according to tblGeo
    allvalid <- out[TIL == 9999 & TYP == "O" & GEOniv %in% c("F", "K", "B") & !GEO %in% tblGeo$GEO]
    if(nrow(allvalid) > 0){
        message(" - The following rows contain invalid GEO codes for ", year)
        allvalid
    } else {
        message(" - All GEO codes with TYP = 'O' and TIL == 9999 are valid for ", year)
    }
    
    ## Check that all valid GEO-codes are included in GeoKoder with TIL == 9999
    validincluded <- tblGeo$GEO[!tblGeo$GEO %in% out[TIL == "9999", GEO]]
    if(length(validincluded) > 0){
        message(" - The following valid GEO codes for ", year, " are not included in GeoKoder, or have TIL != 9999")
        validincluded
    } else {
        message(" - All valid GEO codes for ", year, " are included in GeoKoder")
    }
    
    # Write to Access    
    if(write){
        
        # Ask for confirmation before writing
        opts <- c("Overwrite", "Cancel")
        answer <- utils::menu(choices = opts, 
                              title = paste0("Whoops!! You are now replacing the table GeoKoder in:\n\n", 
                                             basepath, khelsapath, 
                                             "\n\nPlease confirm or cancel:"))
        
        if(opts[answer] == "Overwrite"){
            cat("\nUpdating the GeoKoder table in KHELSA...\n")
            RODBC::sqlSave(channel = .KHELSA, 
                           dat = out, 
                           tablename = "GeoKoder", 
                           append = FALSE, 
                           rownames = FALSE, 
                           safer = FALSE)
            cat(paste0("\nDONE! New table written to:\n", basepath, khelsapath, "\n\n"))
        } else {
            cat(paste0("\nYou cancelled, and the table was not overwritten! Puh!\n"))
        }
    }
    
   
    return(out)
}

# Helper function to convert GEO columns to character and add leading 0
addleading0 <- function(data){
    
    allcols <- c("GEO", "GEO_omk", "oldCode", "currentCode", "code", "grunnkrets", "kommune", "fylke", "bydel")
    cols <- names(data)[names(data) %in% allcols]
    data[, (cols) := lapply(.SD, as.character), .SDcols = cols]
    for(i in 1:length(cols)){
        data[get(cols[i]) != 0 & nchar(get(cols[i])) %in% c(1,3,5,7), (cols[i]) := paste0("0", get(cols[i]))]
    }
    data[]
}
