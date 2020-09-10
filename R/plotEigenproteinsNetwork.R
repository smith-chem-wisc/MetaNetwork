plotEigenproteinsNetwork <- function(moduleEigenproteins,
                              fileName = "Results/DendrogramEigenproteins.pdf",
                              widthInches = 10){
  pdf(file = fileName, width = widthInches)
  plotEigengeneNetworks(moduleEigenproteins,
    "EigenproteinNetwork",
    marHeatmap = c(3,4,2,2),
    marDendro = c(3,4,2,5),
    plotDendrograms = TRUE,
    xLabelsAngle = 90,
    heatmapColors=blueWhiteRed(50))
  dev.off()
  message("Eigenproteins dendrogram exported")
}
