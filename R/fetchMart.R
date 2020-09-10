fetchMart <- function(species){
  ensembl <- useMart("ensembl")
  if(species == "Human"){
    ensembl <- useDataset("hsapiens_gene_ensembl", mart = ensembl)
    mart <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  } else if(species == "Mouse"){
    ensembl <- useDataset("mmusculus_gene_ensembl", mart = ensembl)
    mart <- useMart("ensembl", dataset = "mmusculus_gene_ensembl")
  }
  if(is.null(mart) == FALSE){
    message("Successfully retrieved database")}else{
      stop("Error in database retrieval")
    }
  return(mart)
}
