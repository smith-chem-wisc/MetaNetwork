## queryTOM
queryTOM <- function(matrix, proteinAccession, allData, interactors){
  if(length(proteinAccession) == 1){
    proteinTOMquery <- which(colnames(allData) == proteinAccession)
    interactorsQuery <- which(colnames(allData) %in% interactors)
    results <- c()
    for(i in 1:length(interactorsQuery)){
      results[i] <- matrix[proteinTOMquery, interactorsQuery[i]]    
    }
    results <- cbind(interactors, results)
    return(results)
  }else{
    proteinTOMquery <- which(colnames(allData) %in% proteinAccession)
    interactorsquery <- which(colnames(allData) %in% interactors)
    matrixResults <- matrix(nrow = proteinTOMquery, 
                            ncol = interactorsquery)
    matrixResults <- matrix[proteinTOMquery, interactorsquery]
    colnames(matrixResults) <- proteinAccession
    rownames(matrixResults) <- interactors  
    return(matrixResults)
  } 
}