## Convert Uniprot Accessions
convertAccessions <- function(wgcnaResults, 
                              convertTo = "external_gene_name", 
                              filterBy = "uniprot_gn_id",
                              mart){
  sheetNumber <- length(wgcnaResults)
  if(sheetNumber == 1){
    UniprotAcessions <- wgcnaResults[,1]
    }else{
      UniprotAcessions <- list()
      for(i in seq_len(sheetNumber)){
        UniprotAcessions[[i]] <- wgcnaResults[[i]][,1]
      }
    }
  ModuleNames <- names(wgcnaResults)
  names(UniprotAcessions) <- ModuleNames
  
  ConvertedGeneSymbols <- list()
  for(i in seq_len(sheetNumber)){
    ConvertedGeneSymbols[[i]] <- getBM(attributes = convertTo, 
                                       filters = filterBy, mart = mart, 
                                       values = UniprotAcessions[[i]])$external_gene_name
  }
  return(ConvertedGeneSymbols)
}

