## Install and load required packages for the first time
installAndLoadRequiredPackages <- function(){
  packages.vec <- c("shiny", "devtools", "heatmap3", "WGCNA", "readxl", 
                    "openxlsx", "BiocManager", "tidyverse", 
                    "allez", "ggdendro", "biomaRt", "shinyFiles")
  packages.to.install <- packages.vec[!packages.vec %in% installed.packages()[,"Package"]]
  if(length(packages.to.install)>0){
    for(i in 1:length(packages.to.install)){
      if(packages.to.install[i] == "allez"){
        library(devtools)
        install_github(repo = "wiscstatman/allez", dependencies = TRUE)
      }else if(packages.to.install[i] == "WGCNA"){
        BiocManager::install("WGCNA")
      }else if(packages.to.install[i] == "biomaRt"){
        BiocManager::install("biomaRt")
      }
      install.packages(packages.to.install[i])
    }
  }
  lapply(packages.vec, library, character.only = TRUE)
  message("Packages are installed and loaded")
}
