# Functions used to update geo tables

addleading0 <- function(data, cols){
    
    data[, (cols) := lapply(.SD, as.character), .SDcols = cols]
    
    for(i in 1:length(cols)){
        data[nchar(get(cols[i])) %in% c(1,3,5), (cols[i]) := paste0("0", get(cols[i]))]
    }
    
    data[]
}
