createAllezEnrichmentXLSX <- function(geneUniverse, AllezPvalues){
  wb = createWorkbook()

  addWorksheet(wb, paste("GeneUniverse"))
  writeData(wb, sheet = 1, geneUniverse)

  # write modules only tabs
  for(ii in 1:length(AllezPvalues)) {
    addWorksheet(wb, names(AllezPvalues)[[ii]])
    writeData(wb, names(AllezPvalues)[[ii]], AllezPvalues[[ii]][[1]])
  }
  saveWorkbook(wb, file = "Results/AllezEnrichment.xlsx", overwrite=TRUE)
  message("Allez enrichment workbook saved successfully")
}
