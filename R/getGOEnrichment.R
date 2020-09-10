getGOEnrichment <- function(df, fullGOTermsList, plot = TRUE, file.name){
  
  #get the module color for plotting
  modcolor <- names(df)
  #
  fisher.vec <- vector()
  for(i in seq_along(df$`GO:ID`)){
    C_a <- df$`GO:ID`[[i]]
    index.val <- which(fullGOTermsList$`GO:ID` %in% C_a)
    A <- df$n[i]
    B <- sum(df$n)
    C <- fullGOTermsList$n[index.val] - A
    D <- sum(fullGOTermsList$n) - B
    fisher.tib <- tibble(rbind(A,C), rbind(B,D))
    inter1 <- fisher.test(fisher.tib)
    fisher.vec[i] <- inter1$p.value
  }
  
  
  GOterms.pvalue <- df %>%
    add_column(fisher.vec)%>%
    arrange(fisher.vec)
  top.20.pvalues <- GOterms.pvalue[1:20,]
  
  if(plot == TRUE){
    p <- ggplot(data = top.20.pvalues, mapping = aes(x = -log10(fisher.vec), y = reorder(GOTermName, -fisher.vec)))+
      geom_col()+
      geom_vline(xintercept = -log10(0.05/length(df$`GO:ID`)), col = "red")+
      scale_y_discrete()+
      ggtitle(file.name)+
      ylab("Top 20 GO Terms")+
      xlab("-log10(p-value)")+
      theme_bw()
    ggsave(file.name, plot = last_plot())
  }
}