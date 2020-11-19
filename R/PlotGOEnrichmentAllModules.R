PlotGOEnrichmentAllModules <- function(allezModules){
  AllModulesPlots <- list()
  for(i in 1:length(allezModules)){
    AllModulesPlots[[i]] <- plotZscores(allezModules[[i]],
                                        fill = names(allezModules[i]), 
                                        module = names(allezModules[i])) 
  }
  return(AllModulesPlots)
}