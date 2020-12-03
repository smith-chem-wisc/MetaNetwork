ModuleEigenproteinsWithSignif <- function(EigenproteinsTall, 
                                          position1, increment){
  
  my_comparisons <- list(c(1,2), c(3,4))
  my_comparisons_unpaired1 <- list(c(1,3))
  my_comparisons_unpaired2 <- list(c(2,4))  
  
  EigenproteinsFin <- EigenproteinsTall%>%
    group_by(SampleGroup , `Module Eigenprotein`)%>%
    summarize(mean = mean(Value))%>%
    mutate(moduleColor = str_sub(`Module Eigenprotein`, 3))
  position2 <- position1 + increment
  position3 <- position2 + increment
  
  MEplots <- ggplot()+
    scale_fill_manual(values = unique(EigenproteinsFin$moduleColor), aesthetics = "fill")+
    geom_bar(data = EigenproteinsFin, 
             mapping = aes(x = `SampleGroup`, y = mean, fill = EigenproteinsFin$moduleColor), stat = "identity", 
             col = "black")+
    geom_point(shape = 21, data = EigenproteinsTall, mapping = aes(x = `SampleGroup`, y = Value),
               fill = "white", position = position_jitter(width = 0.2))+
    geom_signif(comparisons = my_comparisons, data = EigenproteinsTall, 
                mapping = aes(x = as.factor(`SampleGroup`), y = Value), 
                test = "t.test", 
                y_position = position1, 
                test.args = list(alternative = "two.sided",
                                 paired = TRUE))+
    geom_signif(comparisons = my_comparisons_unpaired1, data = EigenproteinsTall, 
                mapping = aes(x = as.factor(`SampleGroup`), y = Value), 
                test = "t.test", 
                test.args = list(alternative = "two.sided", 
                                 paired = FALSE), 
                y_position = position2
                )+
    geom_signif(comparisons = my_comparisons_unpaired2, data = EigenproteinsTall, 
                mapping = aes(x = as.factor(`SampleGroup`), y = Value), 
                test = "t.test", 
                test.args = list(alternative = "two.sided", 
                                 paired = FALSE), 
                y_position = position3
                )+
    facet_wrap(~`Module Eigenprotein`)+
    ggtitle("Module Eigenproteins vs. Experimental Groups")+
    ylab("Module Eigenprotein")+
    xlab("Experimental group")+
    geom_hline(yintercept = 0)+
    theme_bw(base_size = 14)+
    theme(legend.position = "none", axis.text.x = element_text(angle = 90))
  return(MEplots)
}