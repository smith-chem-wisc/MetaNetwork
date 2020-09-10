getTop10HubProteins <- function(df, file.name){
  ##get the module color:
  mod.color <- df$moduleColors[1]
  ## Get the number of unique proteins
  protein.count <- length(unique(df$accession))
  #get the column with the
  #get the GO terms from GOTerms, using the df.
  top10df <- df[1:10,]
  ggplot(data = top10df, mapping = aes_string(x = colnames(top10df)[1], y = colnames(top10df)[ncol(top10df)] ))+
    geom_col()+
    ggtitle(paste("Top KMEs for", mod.color, "module. ", sep = " "))+
    theme_classic()
  ggsave(filename = file.name, last_plot())
}