mergeAndWriteWGCNAWorkbook <- function(selectedDatabase,
                                     allData,
                                     dataList, ...){
  allData <- as_tibble(allData)
  dataList <- lapply(dataList, as_tibble)
  message("Pulling gene symbols from Uniprot Database...")
  userInputDatabaseSelectedColumns <- tibble(selectedDatabase$Entry,
                                             selectedDatabase$`Gene names`,
                                             selectedDatabase$`Protein names`)
  colnames(userInputDatabaseSelectedColumns) <- c(names(allData)[1],
                                                  "gene name",
                                                  "protein name")
  dat.resMerged <- left_join(allData,
                             userInputDatabaseSelectedColumns,
                             by = names(allData)[1])
  message("Error does not occur in Line 16")
  list.cluster.datMerged <- list()
  for(i in seq_along(dataList)){
    list.cluster.datMerged[[i]] <- left_join(dataList[[i]],
                                             userInputDatabaseSelectedColumns,
                                             by = names(allData[1]))
  }
  message("Saving workbook...")
  names(list.cluster.datMerged) <- names(dataList)
  createResultsWGCNAExcelWorkbook(dat.resMerged, list.cluster.datMerged, ...)
  message("WGCNA workbook saved")
}

