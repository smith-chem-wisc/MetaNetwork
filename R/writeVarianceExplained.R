## Variance explained by eigenproteins
writeVarianceExplained <- function(datExpr, 
         colors, 
         MEs){
  varianceExplained <- propVarExplained(datExpr, 
                   colors, 
                   MEs, 
                   corFnc = "cor", 
                   corOptions = "use = 'p'")
  write.csv(x = varianceExplained, file = "Results/varianceExplained.csv")
  message("Variance explained file successfully written")
}
