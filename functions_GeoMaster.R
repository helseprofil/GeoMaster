# Required packages

library(RODBC)
library(data.table)
library(orgdata)

# Functions used to update geo tables

#' KnrHarmUpdate
#' 
#' Function updating the KnrHarm table in KHELSA
#'
#' @param year current year
#' @param khelsa Name of database file path, root folder = STYRING
#' @param geokoder Name of database file path, root folder = STYRING
#'
#' @examples
#' KnrHarmUpdate(year = 2024, basepath = , khelsapath = "KHELSA.mdb", geokoderpath = "raw-khelse/geo-koder.accdb", write = FALSE)
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

# Helper function to convert GEO columns to character and add leading 0
addleading0 <- function(data){
    
    allcols <- c("GEO", "GEO_omk", "oldCode", "currentCode", "code", "grunnkrets", "kommune", "fylke", "bydel")
    cols <- names(data)[names(data) %in% allcols]
    data[, (cols) := lapply(.SD, as.character), .SDcols = cols]
    for(i in 1:length(cols)){
        data[get(cols) != 0 & nchar(get(cols[i])) %in% c(1,3,5,7), (cols[i]) := paste0("0", get(cols[i]))]
    }
    data[]
}
