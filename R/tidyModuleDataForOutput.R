## Tidy the data
tidyModuleDataForOutput <- function(dataInput, moduleColors, groupsFile){
  col.length <- length(colnames(dataInput)) - 1
  
  dfAddedColumn <- add_column(dataInput, "Experiment" = rownames(dataInput))
  dataFile_tidy <- gather(dfAddedColumn, key = "Gene", value = "Expression", 1:col.length)
  
  genes_colors.df <- data.frame(colnames(dataInput), moduleColors)
  colnames(genes_colors.df) <- c("Gene", "moduleColor")
  dataColTidy <- left_join(dataFile_tidy, genes_colors.df, "Gene")
  dataColTidy$moduleColor <- as.factor(dataColTidy$moduleColor)
  colnames(groupsFile) <- c("Experiment", "SampleID")
  groupsFile$Experiment <- str_replace_all(groupsFile$Experiment, 
                                           pattern = " ", 
                                           replacement = ".")
  
  allDataFinal <- left_join(dataColTidy, groupsFile, "Experiment")
  message("Module data is now tidy")
  return(allDataFinal)
}
