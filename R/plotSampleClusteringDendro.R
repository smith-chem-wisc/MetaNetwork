plotSampleClusteringDendro <- function(dendro, fontSize = 15,
                                       fileName = "SampleClustering.pdf"){
sampleClusteringQC  <- ggdendrogram(dendro, rotate = TRUE)+
  ggtitle("Sample Clustering to Detect Outliers")+
  xlab("Sample")+
  ylab("Height")+
  coord_flip()
  theme_bw()

  theme_bw(base_size = fontSize)
ggsave(filename = fileName, path = "Results", plot = last_plot())
message("Sample dendrogram created")
}
