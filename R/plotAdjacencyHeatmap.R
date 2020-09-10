plotAdjacencyHeatmap <- function(moduleEigenproteins,
                                 fileName = "Results/Eigenprotein_adjacency heatmap.pdf",
                                 widthInches = 10){
  MET <- orderMEs(MEs = moduleEigenproteins)


  pdf(file = fileName, width = widthInches)
  par(cex = 1.0)
  plotEigengeneNetworks(MET, "Eigenprotein Dendrogram",
                        marDendro = c(0,4,2,0),
                        plotHeatmaps = FALSE)

  plotEigengeneNetworks(MET,
                        "Eigenprotein adjacency heatmap",
                        marDendro = c(3,4,2,2),
                        xLabelsAngle = 90)
  dev.off()
  message("Adjacency heatmap created")
}
