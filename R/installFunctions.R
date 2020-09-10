installFunctions <- function(){
 Rfiles <- list.files(path = "./R")
 RfilesPaths <- paste("R/", Rfiles, sep = "")
 for(i in 1:length(RfilesPaths)){
   source(RfilesPaths[i])
 }
 message("Successfully loaded endogenous functions")
}
