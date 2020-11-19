## Plotly volcano plot ##
# Transforms expression data and returns an interactive volcano plot. 

## Creates the generic volcano plot function called by PlotDifferentialExpression
VolcanoPlotter <- function(dataSet, 
                            foldChangeThreshold = 2, 
                            p.valueThreshold = 0.01, 
                            comparison1, 
                            comparison2){
  ggplot(dataSet, mapping = aes(x = dataSet$FoldChange, 
                                      y = -log10(dataSet$p.adj)))+
    geom_point(aes(color = moduleColors, label = dataSet$accession))+
    scale_color_identity(guide = "legend")+
    geom_hline(yintercept = -log10(p.valueThreshold))+
    geom_vline(xintercept = -log2(foldChangeThreshold))+
    geom_vline(xintercept = log2(foldChangeThreshold))+
    xlab("log2(Fold-change)")+
    ylab("-log10(corrected p-value)")+
    ggtitle(paste0(comparison1, " vs ", comparison2))+
    theme_dark()
}

## Automatically processes the data and plots the data
PlotDifferentialExpression <- function(dataSet1,
                                   dataSet2, 
                                   dataSet1.name, 
                                   dataSet2.name, 
                                   dataType = "log2Transformed", 
                                   accessionAndColors, 
                                   accessionAndProteinNames,
                                   plot = TRUE,
                                   plotlyInteractive = TRUE){
  if(is.character(dataSet1.name) & is.character(dataSet2.name) != TRUE){
    dataSet1.name <- as.character(dataSet1.name)
    dataSet2.name <- as.character(dataSet2.name)
    warning("Data set names coerced to character")
  }
  # Converts to tidy data format
  Set1Tall <- reshape::melt(dataSet1) %>%
    add_column(group = dataSet1.name)
  Set2Tall <- reshape::melt(dataSet2) %>%
    add_column(group = dataSet2.name)
  
  #_bySample is used for calculating the fold changes; _byAccession is used for 
  # calculating the p-values
  Set1vSet2_bySample <- merge(Set1Tall, Set2Tall, all = T)
  Set1vSet2_byAccession <- merge(Set1Tall, Set2Tall, by = "accession")
  
  # Calculates the mean
  Set1vSet2_mean <- Set1vSet2_bySample %>%
    group_by(accession, group) %>%
    summarise(mean = mean(value))%>%
    spread(key = group, value = mean)
  # Calculates the p-values and adjusts them using Benjamini-Hochberg
  Set1vSet2_pvalue <- Set1vSet2_byAccession %>%
    group_by(accession) %>%
    summarise(ttest = t.test(value.x, value.y)$p.value)%>%
    mutate(p.adj = p.adjust(ttest, method = "BH"))

  # specifies which type of data it is. It's typically better for the data to be 
  # log2 transformed and then normalized to have a normal distribution, but this might not 
  # always be the case. This portion of the function exists as a way to quickly add
  # new functionality in the future. 
  if(dataType == "log2Transformed"){ 
    Set1vSet2_foldChange <- tibble(Set1vSet2_mean[1], Set1vSet2_mean[3] - Set1vSet2_mean[2])
    colnames(Set1vSet2_foldChange) <- c("accession", "FoldChange")
  }

  Set1vsSet2_final <- left_join(Set1vSet2_foldChange,
                                Set1vSet2_pvalue,
                                by = "accession")%>%
    left_join(., accessionAndColors, by = "accession")%>%
    left_join(., accessionAndProteinNames, by = "accession")
  # plot == FALSE is useful if you are doing downstream analysis outside of MetaNetwork
  if(plot == FALSE){
    return(Set1vsSet2_final)
  }
  # VolcanoPlotter uses ggplot2, which is then turned into an interactive HTML 
  # widget if plotlyInteractive == TRUE. Currently set up as an if-else statement
  # because we could add more plot output features in the future
  prePlotlyVolcano <- VolcanoPlotter(Set1vsSet2_final, comparison1 = dataSet1.name,
                 comparison2 = dataSet2.name)
  if(plotlyInteractive == TRUE){
    PlotlyVolcano <- plotly::ggplotly(prePlotlyVolcano)
    return(PlotlyVolcano)
  }else{
    return(prePlotlyVolcano)
  }
}

