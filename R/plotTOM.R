plotTOM <- function(TOMData,
                   proteinDendro,
                   moduleColors,
                   fileName = "Results/NetworkHeatmap.png"){
  message("Begin generating TOM plot.")
  message("Please be patient. This may take a while")

  dev.new()
  png(filename = fileName)
  TOMplot(TOMData, proteinDendro, moduleColors,
    main = "Network heatmap plot, all proteins",
    col = rev(heat.colors(999)))
  dev.off()
  message("TOM plot successfully generated")
}
