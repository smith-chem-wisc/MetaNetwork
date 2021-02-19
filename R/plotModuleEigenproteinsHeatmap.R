plotmoduleEigenproteinsHeatmap <- function(moduleEigenproteins, 
                                       fileName = "Results/ModuleEigenproteinsHeatmap.pdf", 
                                       widthInches = 15){
  modulesHeatmap <- heatmap3(moduleEigenproteins, 
                             distfun = function(x) dist(x, method="euclidean"),
                             main = "Module Eigenproteins",
                             cexRow = 0.6, cexCol = 0.6)
  pdf(file = fileName, width = widthInches)
  heatmap3(moduleEigenproteins, distfun = function(x) dist(x, method="euclidean"),
           main = "Module Eigenproteins",
           cexRow = 0.6, cexCol = 0.6)
  dev.off()  
  message("Heatmap of eigenproteins successfully created")
}

