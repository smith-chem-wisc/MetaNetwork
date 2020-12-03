## Adapted from the enrich_shiny application found at https://github.com/lengning/Enrich_shiny
enrichAllez <- function(GeneSymbols, 
                        GeneUniverse, 
                        SpeciesLibrary, 
                        idtype = "SYMBOL", 
                        alter = TRUE, 
                        Lowersetsize = 5, 
                        Uppsersetsize = 500, 
                        outprefix, 
                        ...){
  Scores <- rep(0, length(GeneUniverse))
  names(Scores) <- as.vector(GeneUniverse)
  Scores[base::intersect(names(Scores), GeneSymbols)] <- 1
  allezOutput <- allez(scores = Scores, 
                       lib = SpeciesLibrary, 
                       idtype = "SYMBOL", library.loc = GeneSymbols)
  allezOutput$setscores <- rownames_to_column(data.frame(allezOutput$setscores), 
                                              var = "GO_Term")
  allezOutput$setscores <- addPvaluesToAllezOutput(outputAllez = allezOutput$setscores)
  
 if(alter == TRUE){
   
   CategoryMatrix <- allezOutput$setscores$GO_Term
   AuxMatrix <- allezOutput$aux$set.data
   gInData <- names(which(Scores == 1))
   AuxMatrix <- AuxMatrix[which(AuxMatrix$symbol %in% gInData),]
   
   MaxInCategory <- pmin(length(CategoryMatrix))
   
   GenesInCategories <- lapply(1:MaxInCategory, function(x) {
     useg <- AuxMatrix[which(AuxMatrix$go_id == CategoryMatrix[x]), "symbol"]
     numOL <- length(useg)
     gcats <- paste0(useg, collapse = ", ")
     return(list(gcats, numOL))
     })
   NumInCats <- unlist(sapply(GenesInCategories, function(x) x[2]))
   if(length(CategoryMatrix) > 1000){
     NumberInCats <- c(NumInCats, rep(0, length(CategoryMatrix) - MaxInCategory))
   }
   names(NumInCats) <- CategoryMatrix
   GenesInCategories <- unlist(sapply(GenesInCategories, function(x) x[1]))
     if(length(CategoryMatrix > 1000)) {c(GenesInCategories, 
                            rep("",length(CategoryMatrix) - MaxInCategory))}
   names(GenesInCategories) <- CategoryMatrix
   
   allezOutput$setscores$num.overlap <- NumInCats
   allezOutput$setscores$Genes <- GenesInCategories
     
   
 }
  
  message("Allez enrichment completed")
  if(length(allezOutput$setscores) == 0) message("Warning: output is blank")
  return(allezOutput)
}