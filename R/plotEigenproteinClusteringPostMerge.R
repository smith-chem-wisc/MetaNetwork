plotEigenproteinClusteringPostMerge <- function(preMergeDendroData,
                                                postMergeDendroData,
                                                cutHeight,
                                                fileName = "Results/ModuleEigenproteinMergeDendrogram.pdf",
                                                widthInches = 10){

  pdf(file = fileName, width = widthInches)
  par(mfrow = c(2,1))
  par(cex = 0.6)
  plot(preMergeDendroData,
    main = "Clustering of module eigenproteins, pre-merging",
    xlab = "",
    sub = "")
  abline(h = cutHeight, col = "red")
  plot(postMergeDendroData,
    main = "Clustering of module eigenproteins, post-merging",
    xlab = "", sub = "")
  dev.off()
  message("Eigenproteins dendrograms pre- and post-merge cretaed")
}
