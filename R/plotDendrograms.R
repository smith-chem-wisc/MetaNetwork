plotDendrograms <- function(x){
  extracted_dendro_data <- dendro_data(x)
  
  ggplot()+
    geom_segment(data = extracted_dendro_data$segments, 
                 aes(x = x, y = y, xend = xend, yend = yend))+
    geom_text(data = extracted_dendro_data$labels, 
              aes(x = x, y = y, label = label), 
              size = 3, vjust = 0, angle = 90)
  
}