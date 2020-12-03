plotZscores <- function(x, fill, module){
  ZscorePlot <- ggplot(data = x[1:10,], mapping = aes(x = z.score, y = reorder(str_wrap(Term, width = 30), z.score)))+
    geom_bar(stat = "identity", fill = fill, col = "black")+
    scale_fill_manual(fill, aesthetics = "fill")+
    ggtitle(module, subtitle = "Top 10 Enriched GO Terms")+
    ylab("Gene Ontology Term")+
    xlab("Z-score")+
    theme_bw(base_size = 14)+
    theme(legend.position = "none")
  return(ZscorePlot)
}