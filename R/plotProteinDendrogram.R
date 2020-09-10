plotProteinDendrogram <- function(proteinDendrogram,
  dynamicColors,
  mergedColors){

  pdf("Results/DendroColorMergedClust.pdf")
  plotDendroAndColors(proteinDendrogram, mergedColors,
                      "Merged dynamic", rowText = mergedColors,
                      dendroLabels = FALSE, hang = 0.03,
                      addGuide = TRUE, guideHang = 0.05, 
                      rowTextAlignment = "left", 
                      addTextGuide = TRUE)
  dev.off()
  message("Protein dendrogram generated")
}
