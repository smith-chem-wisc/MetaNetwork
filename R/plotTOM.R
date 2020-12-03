plotTOM <- function(TOMData,
                   proteinDendro,
                   moduleColors,
                   fileName = "Results/NetworkHeatmap.png", ...){
  message("Begin generating TOM plot.")
  message("Please be patient. This may take a while")

  dev.new()
  png(filename = fileName)
  TOMplot(TOMData, proteinDendro, moduleColors,
    main = "Network heatmap plot, all proteins",
    col = hcl.colors(n = 55, palette = "viridis", alpha = 1, rev = FALSE), ...) #rev(heat.colors()))
  dev.off()
  message("TOM plot successfully generated")
}
