## Merge the close modules
mergeModuleEigenproteins <- function(exprData, moduleColors, cutHeight){

  allDataModuleEigenProteins <- moduleEigengenes(expr = exprData,
                                                 colors = moduleColors)
  preMergeModuleEigenProteins <- allDataModuleEigenProteins$eigengenes
  MEDiss <- 1 - cor(preMergeModuleEigenProteins)
  METree <- hclust(as.dist(MEDiss),
                   method = "average")
  mergedClust <- mergeCloseModules(exprData,
                                   moduleColors,
                                   cutHeight = cutHeight,
                                   verbose = 3)
  mergedClust$METree <- METree
  mergedClust$MEDiss <- MEDiss
  return(mergedClust)
}
