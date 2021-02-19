plotTOMpdf <- function(TOMData,
                    proteinDendro,
                    moduleColors,
                    fileName = "Results/NetworkHeatmap.pdf", 
                    height=12, 
                    width=12){
  message("Use only for small TOMs!")
  message("Begin generating TOM plot.")
  message("Please be patient. This may take a while")
  
  dev.new()
  pdf(file = fileName, height, width)
  TOMplot(TOMData, proteinDendro, moduleColors,
          main = "Network heatmap plot, all proteins",
          col = hcl.colors(n = 55, 
                           palette = "viridis", 
                           alpha = 1, rev = FALSE), 
          ... = list(labRow = TRUE,
                     colRow = TRUE)) #rev(heat.colors()))
  dev.off()
  message("TOM plot successfully generated")
}
