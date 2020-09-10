set.seed(54321)
data <- cbind(rnorm(n = 100, mean = 20, sd = 4), rnorm(n = 100, mean = 20, sd = 4))
Rcutoff <- 20
pdfFunctionTesting <- function(data, scaleFreeThreshold, fileName = "pdfPlot.pdf",...){
  graphics.off()
  pdf(file = fileName)
  plot(data)
  abline(h = scaleFreeThreshold, col="red")
  dev.off()
}
pdfFunctionTesting(data, scaleFreeThreshold = Rcutoff)
