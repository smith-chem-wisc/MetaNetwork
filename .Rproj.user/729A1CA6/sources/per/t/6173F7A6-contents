## test comment
AnimalsAndDatabases <- tibble::tibble(readRDS(file = "./AppData/AnimalsAndDatabases.rds"))

server <- shinyServer(function(input, output) {
  
  # Generate the file preview ----
  output$preview <- renderTable({
    req(input$dataFile)
    req(input$groupsFile)
    req(input$dataFile)

    datafile.tibb <- read.csv(input$dataFile$datapath)
    groups.tibb <- read.csv(input$groupsFile$datapath)
    head(datafile.tibb)
  })
  # Action button reactive statement to activate the WGCNA workflow ----

  observeEvent(input$action, {
    # Action button for tab 1 ----
    # require a file input in order to do the workflow. ----
    req(input$dataFile)
    req(input$groupsFile)
    req(input$databaseFile)

    RCutoff <- as.numeric(input$rcutoff)
    MCutHeight <- as.numeric(input$mcutheight)
    PowerUpper <- as.numeric(input$powerupper)
    minModuleSize <- as.numeric(input$minmodsize)
    IgnoreCols <- as.numeric(input$ignorecols)
    softPower <- 12 # sets default power parameter to 12
    AdvancedOptionsEnabled <- input$advancedOptions
    path <- getwd()  
      if(input$threads == ""){
        enableWGCNAThreads()
      }else{
        threads <- as.numeric(input$threads)
        enableWGCNAThreads(threads)
        }
      
      if(input$overridePowerSelection != ""){
        softPower <- as.numeric(input$overridePowerSelection)
        message(print(softPower))
      }
    
    ## WGCNA workflow based on WGCNA tutorials available at https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/
    ## and adopted from the workflow written by Wu et al, which can be found in the supplementary information.  
    ## Citation: Jemma X. Wu, Dana Pascovici, Yunqi Wu, Adam K. Walker, and Mehdi Mirzaei
    ## Journal of Proteome Research 2020 19 (7), 2898-2906. 
    
    # Load the data files ----
    allDataFile <- read.csv(file = input$dataFile$datapath,
                            fileEncoding = "UTF-8-BOM")
    groupsFile <- read.csv(file = input$groupsFile$datapath,
                           fileEncoding = "UTF-8-BOM")

    # Transpose data so that genes are columns and samples are rows ----
    allDataFile_t <- as.data.frame(t(allDataFile[,-c(1:IgnoreCols)]) )
    # column names and row names are now switched.
    rownames(allDataFile_t) <- colnames(allDataFile)[-c(1:IgnoreCols)]
    colnames(allDataFile_t) <- allDataFile[,1]

    #WGCNA Workflow starts here ----
    # Choose a set of soft-thresholding powers
    powers <- c(c(1:10), seq(from = 12, to=PowerUpper, by=2))
    sft <- pickSoftThreshold(allDataFile_t,
                             powerVector = powers,
                             RsquaredCut = RCutoff,
                             verbose = 5)
    
    if(input$automaticPowerSelection == TRUE){
      softPower <- sft$powerEstimate
    }
    
    adjacency <- adjacency(allDataFile_t, power = softPower, type="signed")
    TOM <- TOMsimilarity(adjacency)
    dissTOM <- 1-TOM
    message("dissimilarity TOM matrix successfully created")
    # Clustering using TOM-based dissimilarity
    proTree <- hclust(as.dist(dissTOM), method = "average")

    message("Creating Results folder")
    dir.create(file.path(path, "Results"))

    message("Identifying modules")
    # Module identification using dynamic tree cut
    dynamicMods <- cutreeDynamic(dendro = proTree, distM = dissTOM,
                                 deepSplit = 2, pamRespectsDendro = FALSE,
                                 minClusterSize = minModuleSize)
    dynamicColors <- labels2colors(dynamicMods)
    # Merge clusters
    # Calculate the module eigenproteins
    #The threshold for the merge. Default is 0.25, corresponding to a correlation of 0.75
    message("Merging modules")
    message(print(MCutHeight))
    mergedClust <- tryCatch({mergeModuleEigenproteins(allDataFile_t, 
                                                     moduleColors = dynamicColors,
                                                     cutHeight = MCutHeight)}, 
                              error=function(cond){
                                message("MCutHeight too low!")
                                MCutHeight <- MCutHeight + 0.1
                                mergeModuleEigenproteins(allDataFile_t, 
                                                         moduleColors = dynamicColors, 
                                                         cutHeight = MCutHeight)
                              },
                              warning=function(cond){
                                message("MCutHeight too low!")
                                MCutHeight <- MCutHeight + 0.1
                                mergeModuleEigenproteins(allDataFile_t, 
                                                         moduleColors = dynamicColors, 
                                                         cutHeight = MCutHeight)
                            }
                            )
    mergedColors <- mergedClust$colors
    mergedMEs <- mergedClust$newMEs
    moduleColors <- mergedColors
    METree <- mergedClust$METree
    MEDiss <- mergedClust$MEDiss
    MEs <- mergedMEs
    rownames(MEs) <- rownames(allDataFile_t)

    # reorder MEs by color names of modules
    MEs <- MEs[,order(colnames(MEs))]
    #Create the EigenGeneNetworks Plot
    DendrogramEigenproteins <- plotEigengeneNetworks(MEs, "Eigenprotein Network",
                                                     marHeatmap = c(3,4,2,2),
                                                     marDendro = c(3,4,2,5),
                                                     plotDendrograms = TRUE,
                                                     xLabelsAngle = 90,
                                                     heatmapColors=blueWhiteRed(50))
    # Plot module profiles with eigenproteins overlaid
    WGCNAClusterID <- moduleColors

    ##tidy data format for ease of plotting

    tidyDataFinal <- tidyModuleDataForOutput(dataInput = allDataFile_t, moduleColors = moduleColors,
                                             groupsFile = groupsFile)

    #Create the TOM Plot
    plotTOM <- dissTOM^7
    diag(plotTOM) = NA # Makes a nicer plot
    # Get KME - module membership - correlation between proteins and eigenproteins ----
    kmes <- signedKME(allDataFile_t, MEs)

    # separate results by modules, order by kME, hub proteins on top, in preparation for
    dat.res <- data.frame(allDataFile, moduleColors , kmes)

    list.cluster.dat <- lapply(levels(as.factor(moduleColors)),
                               function(x) {dtemp = dat.res[dat.res$moduleColors == x,];
                               dtemp[order(dtemp[,paste0('kME',x)==colnames(dtemp)], decreasing=TRUE),
                                     -setdiff(grep("^kME", colnames(dtemp)), which(paste0('kME',x)==colnames(dtemp)))]} )

    names(list.cluster.dat) <- 	levels(as.factor(moduleColors))

    #Calculate the module eigenproteins
    allDataModuleEigenProteins <- moduleEigengenes(expr =  allDataFile_t, colors = moduleColors)
    #Quality control on the sample clustering
    sampleTree <- hclust(dist(allDataFile_t), method = "average")

    #Get the Eigenproteins for each module ready to print
    EigenProteins <- tibble(rownames(allDataModuleEigenProteins$eigengenes),
                            allDataModuleEigenProteins$eigengenes)
    EigenProteinsColumnNumber <- length(colnames(EigenProteins))

    EigenProteins_tidy <- EigenProteins%>%
      gather(key = "Samples", value = "ModuleEigenproteins", 2:EigenProteinsColumnNumber)
    colnames(EigenProteins_tidy) <- c("Experiment", "Module", "ModuleEigenprotein")
    EigenProteinsFin <- left_join(EigenProteins_tidy, tibble(groupsFile), "Experiment")

    ## Output the files
    message("Starting output to Results folder")
    # Write the files
    # module memberships
    userInputDatabase <- read_tsv(input$databaseFile$datapath)

    mergeAndWriteWGCNAWorkbook(selectedDatabase = userInputDatabase,
                               allData = dat.res,
                               dataList = list.cluster.dat)
    message("ResultsWGCNA.xlsx written...")
    #Eigenproteins
    write.csv(EigenProteinsFin, file = "Results/Eigenproteins_by_sample.csv")
    EigenproteinsWide <- spread(data = EigenProteinsFin,
                                key = `Module`,
                                value = ModuleEigenprotein)
    write.csv(EigenProteins, file = "Results/EigenproteinsWide.csv")
    message("Eigenproteins written...")
    # Proportion of variance explained
    writeVarianceExplained(datExpr = allDataFile_t,
                           colors = mergedColors,
                           MEs = mergedMEs)
    message("Variance explained written...")

    # Create the .pdfs
    EigenProteinsFin <- as_tibble(EigenProteinsFin)
    dir.create(file.path("Results", "EigenproteinsBySample"))
    for(i in 1:length(unique(EigenProteinsFin[[2]]))){
      plotModuleEigenproteinsBySample(MEPs = EigenProteinsFin,
                                      module = unique(EigenProteinsFin[[2]])[i],
                                      filename = paste("Results/",
                                                       "EigenproteinsBySample/",
                                                       unique(EigenProteinsFin[[2]])[i], ".pdf", sep = ""))
    }

    # Sample clustering quality control plot
    plotSampleClusteringDendro(sampleTree)

    # Scale free topology plot ----
    plotScaleFreeTopology(powersOutput = sft,
                          powers = powers,
                          scaleFreeThreshold = RCutoff)

    # Plot the pre-merge and the merged module eigenprotein clustering
    plotEigenproteinClusteringPostMerge(preMergeDendroData = METree,
                                        postMergeDendroData = mergedClust$dendro,
                                        cutHeight = MCutHeight)

    # Plot the dendrogram following cluster merging ----
    plotProteinDendrogram(proTree,
                          dynamicColors = dynamicColors,
                          mergedColors = mergedColors)

    #Eigeneproteins dendrogram
    plotEigenproteinsNetwork(moduleEigenproteins = MEs)

    #Heatmap of the Module Eigenproteins
    plotmoduleEigenproteinsHeatmap(moduleEigenproteins = MEs)

    # Plot network heatmap
    plotTOM(TOMData = plotTOM,
            proteinDendro = proTree,
            moduleColors = moduleColors)
    #need to add the adjacency heatmap
    plotAdjacencyHeatmap(moduleEigenproteins = MEs)

    #Output code
    output$workflowOutput <- renderText({
      print("WGCNA workflow completed. Results exported to folder.")
      #Grid arrange the plots in preparation for output
    ## use the renderImage function to display the output plots.
    })
  }) 

  observeEvent(input$action2, {
    #require GO File to be uploaded----
    req(input$WGCNAResults)
    ## Get the database based on user choice
    organism <- input$organismID 
    ## Load the WGCNA Results File
    wgcnaResults <- read_excel_allsheets(input$WGCNAResults$datapath)
    sheetNumber <- length(wgcnaResults)
    organismID <- input$organismID
    organismIndex <- which(AnimalsAndDatabases[[1]] == organismID)
    installed.packages(AnimalsAndDatabases[[4]][organismIndex])
    library(AnimalsAndDatabases[[4]][organismIndex], character.only = TRUE)
    ## ModuleNames2 stands for "module names starting at the second page of the WGCNA workbook"
    ModuleNames2 <- names(wgcnaResults)[2:length(wgcnaResults)]    
    
    library(biomaRt) ## Biomart is loaded here because it can be a little slow 
    ## to load, so I just load it when needed
    
    ## Get the organism specific biomart for accession conversion to gene symbols
    mart <- fetchMart(AnimalsAndDatabases[[2]][organismIndex])
    
    ## Convert the Uniprot Acessions to Gene Symbols for each sample
    ConvertedGeneSymbols <- convertAccessions(wgcnaResults = wgcnaResults, mart = mart)

    geneUniverse <- unique(ConvertedGeneSymbols[[1]]) 
    ConvertedGeneSymbolsWithoutUniverse <- ConvertedGeneSymbols[2:length(ConvertedGeneSymbols)]
    names(ConvertedGeneSymbolsWithoutUniverse) <- ModuleNames2
  
    ## AllezEnriched function is adapted from the enrich_shiny application that can be found as a standalone, R shiny-based application 
    ## at https://github.com/lengning/Enrich_shiny. The Enrich_shiny application uses Allez for GO enrichment analysis. 
    ## The allez package can be found at https://github.com/wiscstatman/allez. 
    AllezEnriched <- list()
    for(i in 1:length(ConvertedGeneSymbolsWithoutUniverse)){
      AllezEnriched[[i]] <- enrichAllez(ConvertedGeneSymbolsWithoutUniverse[[i]],
                                        GeneUniverse = geneUniverse,
                                        SpeciesLibrary = AnimalsAndDatabases[[3]][organismIndex]) #species library
      # is defined in the control flow in lines 462-473
    }
    names(AllezEnriched) <- ModuleNames2

    ## Create the workbook with the Allez sheets
    path <- getwd()
    dir.create(file.path(path, "Results"))
    createAllezEnrichmentXLSX(geneUniverse, AllezEnriched)

    message("Allez Enrichment workflow completed")
  })
}) #this one is the server's end
