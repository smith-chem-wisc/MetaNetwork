createResultsWGCNAExcelWorkbook <- function(fullDataSet, modulesList, 
                                            filePath = "Results/ResultsWGCNA.xlsx",
                                            ...){
  wb = createWorkbook()
  
  addWorksheet(wb, paste("WGCNA_workbook"))
  writeData(wb, sheet = 1, fullDataSet)
  
  # write modules only tabs
  for(ii in 1:length(modulesList)) {
    addWorksheet(wb, names(modulesList)[ii])
    writeData(wb, names(modulesList)[ii], modulesList[[ii]])
  }
  
  saveWorkbook(wb, file = filePath, overwrite=TRUE)
}
