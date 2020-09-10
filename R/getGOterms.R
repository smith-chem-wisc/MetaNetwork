getGOterms <- function(df, GOTerms, plot = TRUE, file.name){
  ##get the module color:
  mod.color <- df$moduleColors[1]
  ## Get the number of unique proteins
  protein.count <- length(unique(df$accession))
  #get the GO terms from GOTerms, using the df.
  cluster.uniprot <- df$accession
  index <- which(GOTerms$`Input Accession` %in% cluster.uniprot, useNames = TRUE, arr.ind = TRUE)
  
  GO.listModule <- GOTerms$`Input Accession`[index]
  GO.listID <- GOTerms$`GO:ID`[index]
  GO.listTerms <- GOTerms$`GO Term Name`[index]
  GO.listAspect <- GOTerms$Aspect[index]
  GO.df <- tibble(GO.listModule, GO.listID, GO.listTerms, GO.listAspect)
  ##Filtering the GO terms for P-type GO terms only
  GO.count <- GO.df%>%
    group_by(`GO.listID`)%>%
    filter(GO.listAspect == "F")%>% ## for both process and function: GO.listAspect == "F"|GO.listAspect == "P
    count(`GO.listTerms`)%>%
    arrange(-n)
  ##Plotting function
  if(plot == TRUE){
    p <- ggplot(data = GO.count[1:20,], mapping = aes(x = n, y = reorder(GO.listTerms, -n)))+
      geom_col()+
      ggtitle(paste("GO Terms for", mod.color, "module. ", protein.count, "proteins", sep = " "))+
      theme_classic()
    ggsave(filename = file.name, last_plot())
  }else{return(GO.count)}
}