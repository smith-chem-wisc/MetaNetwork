fetchMart <- function(dataSetIndex){
  mart <- useMart(biomart = "ENSEMBL_MART_ENSEMBL",
                  dataset = dataSetIndex)
  
  if(is.null(mart) == FALSE){
    message("Successfully retrieved database")}else{
      stop("Error in database retrieval")
    }
  return(mart)
}