plotScaleFreeTopology <- function(fileName = "Results/ScaleFreeTopology.pdf",
                                 powersOutput,
                                 powers,
                                 cex1 = 0.9,
                                 scaleFreeThreshold,
                                 widthInches = 10){
  pdf(fileName, width = widthInches)
    par(mfrow = c(1,2))
    # Scale-free topology fit index as a function of the soft-thresholding power
    plot(powersOutput$fitIndices[,1], -sign(powersOutput$fitIndices[,3])*powersOutput$fitIndices[,2],
         xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
         main = paste("Scale independence"))
    text(powersOutput$fitIndices[,1], -sign(powersOutput$fitIndices[,3])*powersOutput$fitIndices[,2],
         labels=powers,cex=cex1,col="red")
    # this adds the red line corresponding to R^2. Default = 0.85
    abline(h=scaleFreeThreshold,col="red")
    # Mean connectivity as a function of the soft-thresholding power
    plot(powersOutput$fitIndices[,1], powersOutput$fitIndices[,5],
         xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
         main = paste("Mean connectivity"))
    text(powersOutput$fitIndices[,1], powersOutput$fitIndices[,5], labels=powers, cex=cex1,col="red")
  dev.off()
  message("Scale free topology plot successfully cretaed")
}
