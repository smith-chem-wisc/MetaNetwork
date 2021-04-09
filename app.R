## Single file app script

## Libraries ####
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(BiocManager)) install.packages("BiocManager", dependencies = TRUE)
if(!require(devtools)) install.packages("devtools")
if(!require(WGCNA)) BiocManager::install("WGCNA")
if(!require(shiny)) install.packages("shiny", dependencies = TRUE)
if(!require(openxlsx)) install.packages("openxlsx", dependencies = TRUE)
if(!require(pheatmap)) install.packages("pheatmap", dependencies = TRUE)
if(!require(plotly)) install.packages("plotly", dependencies = TRUE)
if(!require(shinythemes)) install.packages("shinythemes", dependencies = TRUE)
if(!require(shinyWidgets)) install.packages("shinyWidgets", dependencies = TRUE)
if(!require(gprofiler2)) install.packages("gprofiler2", dependencies = TRUE)
if(!require(withr)) install.packages("withr")
if(!require(zip)) devtools::install_github("zip")
if(!require(rmarkdown)) install.packages("rmarkdown")

library(BiocManager)
options(repos = BiocManager::repositories())
options("repos")
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(openxlsx)
library(pheatmap)
library(plotly)
library(gprofiler2)
library(WGCNA)
library(withr)
library(zip)
library(rmarkdown)
## Classes ####
### Classesx
  ## Classes not S4 need to be set as S4 classes using the setClass function: ####
  setClass("hclust")
  setClass("ggplot")
  setClass("gg")
  setClass("htmlwidget")
  setClass("plotly")
  setClass("pheatmap")




  setOldClass("ggplot")
  setOldClass("gg")
  setOldClass(c("gg", "ggplot")) ## apparently you have to use setOldClass sequentially
  ## This language is incredibly stupid, no offense to Hadley


  ## Class WGCNA Parameters ####
  setClass(Class = "WGCNAParameters",
           slots = c(
             power = "numeric",
             networkType = "character",
             replaceMissingAdjacencies = "logical",
             TOMtype = "character",
             minModuleSize = "numeric",
             mergeCutHeight = "numeric",
             pamRespectsDendro = "logical",
             deepSplit = "numeric",
             checkMissingData = "logical",
             maxBlockSize = "numeric",
             nPreclusteringCenters = "numeric",
             verbose = "numeric"
           ))

  ## Class Raw Data ####
  setClass(Class = "RawData",
           slots = c(
             path = "character",
             data = "tbl_df",
             NonDataColumns = "numeric"),
           contains = class(tibble::tibble())
  )

  ## Protein Dendrogram ####
  setClass("ProteinDendrogram",
           slots = c(
             FileName = "character",
             Plot = "ANY"
           ), contains = "recordedplot")
  ### Class Definition CleanData ####
  setClass(Class = "CleanData",
           slots = c(Data = "list",
                     NonDataColumns = "numeric",
                     Samples = "character",
                     Identifiers = "character"),
           contains = class(tibble::tibble()))

  ## PairedComparisons(not in use yet) ####
  ## TODO: Implement statistical analysis into module eigenproteins analysis
  setClass("PairedComparisons",
           slots = c(
             Comparisons = "list"
           ))

  ## Traits (not used yet) ####
  ### Class Definition Traits
  ### TODO: Implement traits analysis
  setClass("Traits",
           slots = c(
             Traits_df = "list"
           ),
           contains = "tbl_df")
  BuildTraitsFromPath <- function(path){
    traits_tib <- readr::read_csv(path)
    traits_output <- new("Traits")
    traits_output@Traits_df <- traits_tib
    traits_output
  }

  ## EigenproteinNetwork ####
  setClass("EigenproteinNetwork",
           slots = c(
             FileName = "character",
             Plot = "ANY"
           ), contains = "recordedplot")


  ### Class Definition ExperimentalGroups ####
  setClass(Class = "ExperimentalGroups",
           slots = c(
             path = "character",
             ExperimentalDataFrame = "list",
             ExperimentalGroups = "character",
             Sample = "character",
             SignificanceTesting = "logical",
             SigTestType = "character",
             SigTestComparisons = "PairedComparisons"
           ))
  ## Class Definition UniprotDatabase ####
  setClass("UniprotDatabase",
           slots = c(
             Path = "character",
             Database = "tbl_df"
           ),
           contains = class(tibble::tibble())
  )

  ### Class Definition WGCNAHclust ####
  setClass(Class = "WGCNAHclust",
           slots = c(
             hclust = "hclust"
           )
  )

  ## Class Definition WGCNAResults
  setClass(Class = "WGCNAResults",
           slots = c(
             colors = "character",
             unmergedColors = "character",
             MEs = "list",
             goodSamples = "logical",
             goodGenes = "logical",
             dendrogram = "WGCNAHclust",
             TOMFiles = "character",
             blockGenes = "list",
             blocks = "numeric",
             blocksOrder = "ANY",
             MEsOK = "logical",
             ExperimentalGroups = "character",
             TOMatrix = "matrix"
           ),
           contains = c("CleanData", class(tibble::tibble()))
  )

  ### Class Definition ModuleEigenproteinsTidy ####
  setClass("ModuleEigenproteinsTidy",
           slots = c(
             ModuleEigenproteins = "list"
           ), contains = "CleanData")


  ## Class Definition ModuleMembership ####
  setClass("ModuleMembership",
           slots = list(
             Modules = "list"
           ),
           contains = c(class(tibble::tibble()))
  )

  ## gProfilerParameters2 ####
  setClass("gProfilerParameters2",
           slots = c(
             organism = "character",
             ordered_query = "logical",
             multi_query = "logical",
             significant = "logical",
             exclude_iea = "logical",
             measure_underrepresentation = "logical",
             evcodes = "logical",
             user_threshold = "numeric",
             correction_method = "character",
             domain_scope = "character",
             custom_bg = "ANY",
             numeric_ns = "character",
             sources = "ANY",
             as_short_link = "logical"
           ))

  ## GostPlotParameters ####
  setClass("GostPlotParameters", slots = c(
    capped = "logical",
    interactive = "logical"
  ))

  ## EigenproteinHeatmap ####
  setClass("EigenproteinHeatmap",
           slots = c(
             FileName = "character",
             Plot = "pheatmap"
           ), contains = "pheatmap")
  ### Class Definition gProfilerResults ####
  setClass("gProfilerResults",
           slots = c(
             Parameters = "gProfilerParameters2",
             EnrichmentResults = "list"
           ),
           contains = c(class(tibble::tibble()))
  )

  ### Class Definition gProfilerPlot ####
  setClass("gProfilerPlot",
           slots = c(
             Parameters = "GostPlotParameters",
             Plot = "plotly"),
           contains = class(plotly::plotly_empty()))


  ### Class Definition GostPlotCollection ####
  setClass("GostPlotCollection",
           slots = c(
             Plots = "list",
             Names = "character"
           ),
           contains = class(plotly::plotly_empty()))

  ### Class Definition gProfilerResults ####
  setClass("gProfilerResults",
           slots = c(
             Parameters = "gProfilerParameters2",
             EnrichmentResults = "list"
           ),
           contains = c(class(tibble::tibble()))
  )

  ### Class Definition SampleClustering ####
  setClass("SampleClustering",
           slots = c(
             FileName = "character",
             Plot = "ggplot"
           ), contains = "ggplot")

  ### PowerDeterminationParameters Class ####
  setClass("PowerDeterminationParameters",
           slots = c(
             R_squared_cutoff = "numeric",
             powerVector = "numeric",
             networkType = "character",
             moreNetworkConcepts = "logical",
             UserDefinedPower = "logical",
             OverridePower = "numeric",
             AutomaticPowerSelection = "logical"
           ))

  ### Class Definition ScaleFreeTopology ####
  setClass("ScaleFreeTopology",
           slots = c(
             FileName = "character",
             Plot = "ANY",
             Parameters = "PowerDeterminationParameters",
             powerEstimate = "numeric",
             fitIndices = "list"
           ), contains = "recordedplot")


  ### Class Definition Eigenpr
  ### Class Definition ModuleEigenproteinDiagnostics ####
  setClass("ModuleEigenproteinDiagnostics",
           slots = c(
             FileName = "character",
             CorrDendrogram = "ANY",
             AdjacencyHeatmap = "ANY",
             CorrHeatmap = "ANY"
           ), contains = "recordedplot")
  ### Class Definition ModuleEigenproteinPlots ####
  setClass("ModuleEigenproteinPlots",
           slots = c(
             Violin = "list",
             Boxplot = "list",
             Density = "list",
             Names = "character"
           ), contains = "ggplot")
  ## TOMPlot ####
  setClass("TOMPlot",
           slots = c(
             FileName = "character",
             Plot = "recordedplot"
           ))
  ### Class Definition PlotsOutput ####
  setClass("PlotsOutput",
           slots = c(
             PathToOutput = "character",
             gProfilerPlots = "GostPlotCollection",
             SampleClustering = "SampleClustering",
             EigenproteinNetwork = "EigenproteinNetwork",
             ProteinDendrogram = "ProteinDendrogram",
             EigenproteinDiagnostics = "ModuleEigenproteinDiagnostics",
             ModuleEigenproteinPlots = "ModuleEigenproteinPlots",
             TOMPlot = "TOMPlot",
             SFTPlot = "ScaleFreeTopology",
             EigenproteinHeatmap = "EigenproteinHeatmap"
           ))


  ### Class Definition DataOutput ####
  setClass("DataOutput",
           slots = c(
             VarianceExplained = "tbl_df",
             ModuleEigenproteinsTidy = "tbl_df",
             ModuleEigenproteinsWide = "tbl_df",
             ModuleMembership = "list",
             TOMatrix = "matrix",
             gProfilerResults = "list"
           ))

  ## Class ServerOutput ####
  setClass("ServerOutput",
           slots = c(
             Plots = "PlotsOutput",
             Data = "DataOutput"
           ))



  setClassUnion("missingOrNULL", c("missing", "NULL"))
  #setOldClass(Classes = c("gg", "ggplot"))
  setClassUnion("characterOrNULL", c("character", "missing", "NULL"))
  setClassUnion("GostPlotCollectionOrNULL", c("GostPlotCollection","missing","NULL"))
  setClassUnion("SampleClusteringOrNULL", c("SampleClustering","missing","NULL"))
  setClassUnion("ProteinClusteringOrNULL", c("ProteinDendrogram","missing","NULL"))
  setClassUnion("ModuleEigenproteinDiagnosticsOrNULL",
                c("ModuleEigenproteinDiagnostics","missing","NULL"))
  setClassUnion("ModuleEigenproteinPlotsOrNULL",
                c("ModuleEigenproteinPlots","missing","NULL"))
  setClassUnion("ComparisonsOrNULL", c("missing", "NULL", "PairedComparisons"))

## Functions ####
  ## Functions
#
#   plotSampleClusteringDendro <- function(dendro, fontSize = 15,
#                                          fileName = "SampleClustering.pdf"){
#     ggdendro::ggdendrogram(dendro, rotate = TRUE) +
#       ggplot2::ggtitle("Sample Clustering to Detect Outliers") +
#       ggplot2::xlab("Sample") +
#       ggplot2::ylab("Height") +
#       ggplot2::coord_flip() +
#       ggplot2::theme_bw(base_size = fontSize)
#
#     ggplot2::ggsave(filename = fileName,
#                     path = "Results",
#                     plot = ggplot2::last_plot())
#   }

  ## Class Constructor: WGCNAParameters ####
  WGCNAParameters <- function(power,
                              networkType = "signed",
                              replaceMissingAdjacencies = TRUE,
                              TOMtype = "signed",
                              minModuleSize = 20,
                              mergeCutHeight = 0.25,
                              pamRespectsDendro = FALSE,
                              deepSplit = 2,
                              checkMissingData = TRUE,
                              maxBlockSize = 30000,
                              nPreclusteringCenters = 10,
                              verbose = 5){
    if(!is.numeric(power)){
      warning("power coerced to numeric")
      power <- as.numeric(power)
    }
    if(!is.character(networkType) | !(networkType %in% c("signed", "unsigned"))){
      stop("Invalid networkType parameter!")
    }
    if(!is.numeric(minModuleSize)){
      warning("minModuleSize coerced to numeric")
      minModuleSize <- as.numeric(minModuleSize)
    }
    if(!is.numeric(mergeCutHeight)){
      warning("mergeCutHeight coerced to numeric")
      mergeCutHeight <- as.numeric(mergeCutHeight)
    }

    new("WGCNAParameters",
        power = power,
        networkType = networkType,
        replaceMissingAdjacencies = replaceMissingAdjacencies,
        TOMtype = TOMtype,
        minModuleSize = minModuleSize,
        mergeCutHeight = mergeCutHeight,
        pamRespectsDendro = pamRespectsDendro,
        deepSplit = deepSplit,
        checkMissingData = checkMissingData,
        maxBlockSize = maxBlockSize,
        nPreclusteringCenters = nPreclusteringCenters,
        verbose = verbose)
  }

  ## Class Constructor: WGCNA Parameters ####
  RawData <- function(path, data = NULL, non_data_columns = NULL){
    if(!is.character(path)) stop("No valid data path given")
    data <- tibble::as_tibble(data)
    non_data_columns <- as.double(non_data_columns)

    new("RawData", path = path, data = data, NonDataColumns = non_data_columns)
  }

  ## Class Constructor: PairedComparisons
  BuildPairedComparisonsFromPath <- function(path){
    comparisons_tibb <- readr::read_csv(path)
    comp_output <- new("PairedComparisons")
    comp_output@Comparisons <- lapply(
      split(comparisons_tibb, seq(nrow(comparisons_tibb))), unlist)
    comp_output
  }

  ## Class Constructor: Traits ####
  BuildTraitsFromPath <- function(path){
    traits_tib <- readr::read_csv(path)
    traits_output <- new("Traits")
    traits_output@Traits_df <- traits_tib
    traits_output
  }

  ## Class Constructor: ExperimentalGroups ####
  ExperimentalGroups <- function(path,
                                 ExperimentalGroups = NA,
                                 Sample = NA){
    if(!is.character(path)) stop("No valid experimental path given")

    new("ExperimentalGroups", path = path, ExperimentalGroups = NA, Sample = NA)
  }

  ## Class Constructor: UniprotDatabase
  CreateUniprotDatabase <- function(path){
    UniprotDatabaseObject <- new("UniprotDatabase")
    uploadedDatabase <- readr::read_tsv(path)
    UniprotDatabaseObject@Database <- tibble::tibble(Accession = uploadedDatabase$Entry,
                                                     Gene = uploadedDatabase$`Gene names`,
                                                     Protein = uploadedDatabase$`Protein names`)
    UniprotDatabaseObject
  }

  ## Class Constructor: WGCNAHclust
  WGCNAHclust <- function(results){

    new("WGCNAHclust", hclust = results[[6]][[1]])
  }

  ## WGCNAResults Constructor
  WGCNAResults <- function(results, CleanData, ExperimentalGroups){

    WGCNA_results_length <- length(results)
    WGCNAResultsObject <- new("WGCNAResults",
                              Samples = CleanData@Samples,
                              Identifiers = CleanData@Identifiers,
                              ExperimentalGroups =
                                ExperimentalGroups@ExperimentalGroups)
    WGCNAResultsObject@Data <- CleanData@Data

    if(WGCNA_results_length == 10){
      WGCNAResultsObject@colors = results[[1]]
      WGCNAResultsObject@unmergedColors = results[[2]]
      WGCNAResultsObject@MEs = results[[3]]
      WGCNAResultsObject@goodSamples = results[[4]]
      WGCNAResultsObject@goodGenes = results[[5]]
      WGCNAResultsObject@dendrogram = WGCNAHclust(results)
      WGCNAResultsObject@TOMFiles = ifelse(is.null(results[[7]]),
                                           yes = "NA", no = results[[7]])
      WGCNAResultsObject@blockGenes = results[[8]]
      WGCNAResultsObject@blocks = results[[9]]
      WGCNAResultsObject@MEsOK = results[[10]]
      WGCNAResultsObject@blocksOrder = NA
    }else if(WGCNA_results_length == 11){

      WGCNAResultsObject@colors = results[[1]]
      WGCNAResultsObject@unmergedColors = results[[2]]
      WGCNAResultsObject@MEs = results[[3]]
      WGCNAResultsObject@goodSamples = results[[4]]
      WGCNAResultsObject@goodGenes = results[[5]]
      WGCNAResultsObject@dendrogram = WGCNAHclust(results)
      WGCNAResultsObject@TOMFiles = ifelse(is.null(results[[7]]),
                                           yes = "NA", no = results[[7]])
      WGCNAResultsObject@blockGenes = results[[8]]
      WGCNAResultsObject@blocks = results[[9]]
      WGCNAResultsObject@blocksOrder = results[[10]]
      WGCNAResultsObject@ME = results[[11]]
    }
    WGCNAResultsObject
  }

  ## Class Constructor: ModuleEigenproteinsTidy
  ModuleEigenproteinsTidy <- function(WGCNAResults,
                                      ExperimentalGroups){
    ME_object <- new("ModuleEigenproteinsTidy")
    BuildModuleEigenproteinsTidy(ME_object,
                                 WGCNAResults = WGCNAResults,
                                 ExperimentalGroups = ExperimentalGroups)
  }

  ## Class Constructor: gProfilerParameters
  gProfilerParameters2 <- function(organism,
                                   OrderedQuery = FALSE,
                                   MultiQuery = FALSE,
                                   Significant = FALSE,
                                   ExcludeIEA = FALSE,
                                   MeasureUnderrepresentation = FALSE,
                                   EvCodes = FALSE,
                                   UserThreshold = 0.05,
                                   CorrectionMethod = "g_SCS",
                                   DomainScope = "annotated",
                                   CustomBkg = NULL,
                                   NumericNS = "",
                                   Sources = NULL,
                                   AsShortLink = FALSE){
    new("gProfilerParameters2",
        organism = organism,
        ordered_query = OrderedQuery,
        multi_query = MultiQuery,
        significant = Significant,
        exclude_iea = ExcludeIEA,
        measure_underrepresentation = MeasureUnderrepresentation,
        evcodes = EvCodes,
        user_threshold = UserThreshold,
        correction_method = CorrectionMethod,
        domain_scope = DomainScope,
        custom_bg = CustomBkg,
        numeric_ns = NumericNS,
        sources = Sources,
        as_short_link = AsShortLink
    )
  }

  ## Class Constructor GostPlotParameters
  GostPlotParameters <- function(capped = TRUE,
                                 interactive = TRUE){
    new("GostPlotParameters",
        capped = capped,
        interactive = interactive)
  }

  ## Class Constructor PowerDeterminationParameters
  PowerDeterminationParameters <- function(R_squared_cutoff,
                                           powerVector,
                                           networkType = "signed",
                                           moreNetworkConcepts = FALSE,
                                           UserDefinedPower = FALSE,
                                           OverridePower = 12,
                                           AutomaticPowerSelection = TRUE
  ){
    new("PowerDeterminationParameters",
        R_squared_cutoff = R_squared_cutoff,
        powerVector = powerVector,
        networkType = networkType,
        moreNetworkConcepts = moreNetworkConcepts,
        UserDefinedPower = UserDefinedPower,
        OverridePower = OverridePower,
        AutomaticPowerSelection = AutomaticPowerSelection)
  }
  ## ModuleEigenprotein Plots ####
  ModuleEigenproteinPlots <- function(METidy,
                                      comparisonsObject){
    MEPlotsOutput <- new("ModuleEigenproteinPlots")
    MEnames <- unique(METidy@ModuleEigenproteins$Module)

    bp_list <- BoxPlot_IndividualModule_by_Experiment(METidy,
                                                      comparisonsObject)
    names(bp_list) <- MEnames
    bp_list[["All"]] <- BoxPlot_ModuleFacetWrapped_by_Experiment(METidy,
                                                                 comparisonsObject)

    vp_list <- ViolinPlot_IndividualModule_by_Experiment(METidy,
                                                         comparisonsObject)
    names(vp_list) <- MEnames
    vp_list[["All"]] <- ViolinPlot_ModuleFacetWrapped_by_Experiment(METidy,
                                                                    comparisonsObject)

    dens_list <- DensityPlot_IndividualModule_by_Experiment(METidy)
    names(dens_list) <- MEnames
    dens_list[["All"]] <- DensityPlot_ModuleFacetWrapped_by_Experiment(METidy)

    MEPlotsOutput@Violin <- vp_list
    MEPlotsOutput@Boxplot <- bp_list
    MEPlotsOutput@Density <- dens_list

    MEPlotsOutput
  }

  ## Generate FileNames:
## Generics ####
  ### S4 Generics
  setGeneric("BuildAllPlots",
             function(METidy,
                      WGCNAResults,
                      CleanData,
                      WGCNAParameters,
                      SFTObject,
                      GostplotParameters,
                      gProfilerResults,
                      comparisonsObject) standardGeneric("BuildAllPlots"))


  ## Accessors for ServerOutput Objects ####
  setGeneric("fetch_gostplots",
             function(plotsOutput) standardGeneric("fetch_gostplots"))
  setGeneric("fetch_SampleClustering",
             function(plotsOutput) standardGeneric("fetch_SampleClustering"))
  setGeneric("fetch_ProteinDendrogram",
             function(plotsOutput) standardGeneric("fetch_ProteinDendrogram"))
  setGeneric("fetch_EigenproteinDiagnostics",
             function(plotsOutput) standardGeneric("fetch_EigenproteinDiagnostics"))
  setGeneric("fetch_ModuleEigenproteinPlots",
             function(plotsOutput) standardGeneric("fetch_ModuleEigenproteinPlots"))
  setGeneric("fetch_TOMPlot",
             function(plotsOutput) standardGeneric("fetch_TOMPlot"))
  setGeneric("fetch_SFTPlot",
             function(plotsOutput) standardGeneric("fetch_SFTPlot"))
  setGeneric("fetch_MEPlots_names",
             function(x) standardGeneric("fetch_MEPlots_names"))
  setGeneric("fetch_gProfiler_names",
             function(x) standardGeneric("fetch_gProfiler_names"))
  setGeneric("transpose_tibble",
             function(x, var, val) standardGeneric("transpose_tibble"))
  setGeneric("fetch_MEplot_by_index",
             function(ServerOutput, PlotType, index)
               standardGeneric("fetch_MEplot_by_index"))
  setGeneric("fetch_ME_violin", function(ServerOutput) standardGeneric("fetch_ME_violin"))
  setGeneric("fetch_ME_boxplot", function(ServerOutput) standardGeneric("fetch_ME_boxplot"))
  setGeneric("fetch_ME_density", function(ServerOutput) standardGeneric("fetch_ME_density"))
  setGeneric("fetch_MEplot_by_index", function(ggplot_list) standardGeneric("fetch_MEplot_by_index"))

  setGeneric("fetch_violin_plots", function(MEPlots) standardGeneric("fetch_violin_plots"))
  setGeneric("fetch_boxplots", function(MEPlots) standardGeneric("fetch_boxplots"))
  setGeneric("fetch_density_plots", function(MEPlots) standardGeneric("fetch_density_plots"))
  ### get specific_MEPlot ####
  setGeneric("get_specific_MEPlot",
             function(parent, slot, plot_name)
               standardGeneric("get_specific_MEPlot"))
  ### get_specific_gostplot ####
  setGeneric("get_specific_gostplot",
             function(parent, plot_name)
               standardGeneric("get_specific_gostplot"))
  ## accessors used for get_specific_diagnostic_plot ####
  setGeneric("fetch_Protein_Dendrogram", function(parent) standardGeneric("fetch_Protein_Dendrogram"))
  setGeneric("fetch_correlation_dendrogram", function(parent) standardGeneric("fetch_correlation_dendrogram"))
  setGeneric("fetch_adjacency_heatmap", function(parent) standardGeneric("fetch_adjacency_heatmap"))
  setGeneric("fetch_correlation_heatmap", function(parent) standardGeneric("fetch_correlation_heatmap"))
  setGeneric("fetch_Eigenprotein_heatmap", function(parent) standardGeneric("fetch_Eigenprotein_heatmap"))
  setGeneric("fetch_SFT_plot", function(parent) standardGeneric("fetch_SFT_plot"))
  setGeneric("fetch_TOM_plot", function(parent) standardGeneric("fetch_TOM_plot"))
  setGeneric("fetch_SampleClustering_dendro", function(parent) standardGeneric("fetch_SampleClustering_dendro"))
  setGeneric("get_specific_diagnostic_plot", function(parent, plot) standardGeneric("get_specific_diagnostic_plot"))

  ## Transpose tibble ####
  setGeneric("transpose_tibble",
             function(x, var, val) standardGeneric("transpose_tibble"))

  ## Find_non_data_column_indexes ####
  setGeneric("find_nondata_column_indexes",
             function(object) standardGeneric("find_nondata_column_indexes"))

  ## ReadDataFromPath ####
  setGeneric("ReadDataFromPath",
             function(object) standardGeneric("ReadDataFromPath"))

  ## RawData object accessors ####
  setGeneric("path", function(x) standardGeneric("path"))
  setGeneric("path<-", function(x, value) standardGeneric("path<-"))
  setGeneric("data", function(x) standardGeneric("data"))
  setGeneric("data<-", function(x, value) standardGeneric("data<-"))
  setGeneric("NonDataColumns", function(x) standardGeneric("NonDataColumns"))
  setGeneric("NonDataColumns<-", function(x, value) standardGeneric("NonDataColumns<-"))

  ## CleanData object accessors ####
  setGeneric("Data",
             function(x) standardGeneric("Data"))

  setGeneric("Data<-",
             function(x, value) standardGeneric("Data<-"))
  setGeneric("Samples", function(x) standardGeneric("Samples"))
  setGeneric("Identifiers<-", function(x, value) standardGeneric("Identifiers<-"))
  setGeneric("Identifiers", function(x) standardGeneric("Identifiers"))
  setGeneric("Samples<-", function(x, value) standardGeneric("Samples<-"))
  setGeneric("Samples", function(x) standardGeneric("Samples"))

  ## BuildCleanDataFromRaw ####
  setGeneric("BuildCleanDataFromRawData",
             function(x,y, ...) standardGeneric("BuildCleanDataFromRawData"))

  ## RunBlockwiseModules ####
  setGeneric("RunBlockwiseModules",
             function(parameters, cleaned_data){
               standardGeneric("RunBlockwiseModules")
             })

  ## BuildExperimentalGroupsFromPath ####
  setGeneric("BuildExperimentalGroupsFromPath",
             function(x) standardGeneric("BuildExperimentalGroupsFromPath"))

  ## plot for WGCNAHClust ####
  setGeneric("plot")

  ## plotDendroAndColors ####
  setGeneric("plotDendroAndColors",
             function(dendro, colors, ...) standardGeneric("plotDendroAndColors"))

  ## BuildModuleEigenproteinsTidy ####
  setGeneric("BuildModuleEigenproteinsTidy",
             function(ModuleEigenproteinsTidy,
                      WGCNAResults,
                      ExperimentalGroups)
               standardGeneric("BuildModuleEigenproteinsTidy"))

  ## BuildModuleMembership ####
  setGeneric("BuildModuleMembership",
             function(ModuleMembershipObject,
                      WGCNAResultsObject,
                      UniprotDatabaseObject,
                      RawDataObject) standardGeneric("BuildModuleMembership"))

  ## RunGProfilerEnrichment ####
  setGeneric("RunGProfilerEnrichment",
             function(ModuleMembershipObject,
                      gProfilerParametersObject, ...) standardGeneric("RunGProfilerEnrichment"))

  ## gost ####
  setGeneric("gost", function(gProfilerParameters, ModuleMembership) standardGeneric("gost"))

  ## gostplot ####
  setGeneric("gostplot", function(GostPlotParameters,
                                  gProfilerResults) standardGeneric("gostplot"))
  ## gostplot ####
  setGeneric("gostplot", function(GostPlotParameters,
                                  gProfilerResults) standardGeneric("gostplot"))

  ## plotSamleClusteringDendro ####
  setGeneric("plotSampleClusteringDendro",
             function(CleanData) standardGeneric("plotSampleClusteringDendro"))

  ## PickSoftThreshold ####
  setGeneric("PickSoftThreshold", function(PowerObject,
                                           CleanData){standardGeneric("PickSoftThreshold")})

  ## ScaleFreeTopologyPlot ####
  setGeneric("ScaleFreeTopologyPlot",
             function(SFTObject) standardGeneric("ScaleFreeTopologyPlot"))
  ## plotEigenproteinNetwork####
  setGeneric("plotEigenproteinNetwork", function(Results) standardGeneric("plotEigenproteinNetwork"))
  ## PlotProteinDendrogram ####
  setGeneric("PlotProteinDendrogram", function(Results) standardGeneric("PlotProteinDendrogram"))
  ## PlotEigenproteinHeatmap ####
  setGeneric("PlotEigenproteinHeatmap",
             function(WGCNAResults) standardGeneric("PlotEigenproteinHeatmap"))
  ## PlotModuleEigenproteinDiagnostics ####
  setGeneric("PlotModuleEigenproteinDiagnostics",
             function(WGCNAResults) standardGeneric("PlotModuleEigenproteinDiagnostics"))

  ## ModuleEigenprotein Plotting generics ####

  ## Facet Wrapped
  setGeneric("BoxPlot_ModuleFacetWrapped_by_Experiment",
             function(METidy, comparisons) standardGeneric("BoxPlot_ModuleFacetWrapped_by_Experiment"))
  setGeneric("ViolinPlot_ModuleFacetWrapped_by_Experiment",
             function(METidy, comparisons) standardGeneric("ViolinPlot_ModuleFacetWrapped_by_Experiment"))
  setGeneric("DensityPlot_ModuleFacetWrapped_by_Experiment",
             function(METidy) standardGeneric("DensityPlot_ModuleFacetWrapped_by_Experiment"))

  ## By Module
  setGeneric("BoxPlot_IndividualModule_by_Experiment",
             function(METidy, comparisons)
               standardGeneric("BoxPlot_IndividualModule_by_Experiment"))
  setGeneric("ViolinPlot_IndividualModule_by_Experiment",
             function(METidy, comparisons)
               standardGeneric("ViolinPlot_IndividualModule_by_Experiment"))
  setGeneric("DensityPlot_IndividualModule_by_Experiment",
             function(METidy, comparisons)
               standardGeneric("DensityPlot_IndividualModule_by_Experiment"))
  ## CreatePlotsOutput ####
  setGeneric("CreatePlotsOutput", function(PathToOutput,
                                           gProfilerPlots,
                                           SampleClustering,
                                           ProteinDendrogram,
                                           EigenproteinDiagnostics,
                                           ModuleEigenproteinPlots,
                                           TOMPlot,
                                           SFTPlot,
                                           EigenproteinHeatmap) standardGeneric("CreatePlotsOutput"))
  ## Accessors for BuildDataOutput ####
  setGeneric("BuildDataOutput", function(METidy,
                                         WGCNAResults,
                                         gProfiler,
                                         CleanData,
                                         MM_object) standardGeneric("BuildDataOutput"))
  setGeneric("fetch_MEs_wide", function(WGCNAResults) standardGeneric("fetch_MEs_wide"))
  setGeneric("fetch_MEs_tidy", function(METidy) standardGeneric("fetch_MEs_tidy"))
  setGeneric("fetch_gProfiler_results", function(gProfilerResults) standardGeneric("fetch_gProfiler_results"))
  setGeneric("fetch_gProfiler_parameters", function(gProfilerResults) standardGeneric("fetch_gProfiler_parameters"))
  setGeneric("fetch_Module_Membership", function(MM_object) standardGeneric("fetch_Module_Membership"))
  setGeneric("calc_prop_var", function(WGCNAResults, CleanData) standardGeneric("calc_prop_var"))
  ## plotTOM ####
  setGeneric("plotTOM",
             function(CleanData, WGCNAResults, WGCNAParameters) standardGeneric("plotTOM"))

  ## PropVarExplained ####
  setGeneric("PropVarExplained",
             function(CleanData, WGCNAResults) standardGeneric("PropVarExplained"))
  ## CreateServerOutput ####
  setGeneric("CreateServerOutput",
             function(PlotsOutput, DataOutput)
               standardGeneric("CreateServerOutput"))


  setGeneric("fetch_specific_gProfiler_result",
             function(parent, plot_name) standardGeneric("fetch_specific_gProfiler_result"))
  ## PlotProteinDendrogram ####
  setGeneric("PlotProteinDendrogram", function(Results) standardGeneric("PlotProteinDendrogram"))

  setGeneric("plotEigenproteinNetwork", function(Results) standardGeneric("plotEigenproteinNetwork"))
  setGeneric("PlotEigenproteinHeatmap",
             function(WGCNAResults) standardGeneric("PlotEigenproteinHeatmap"))

  ## GenerateFileNames ####
  setGeneric("GenerateFileNames", function(ServerOutput, temp_dir) standardGeneric("GenerateFileNames"))
  setGeneric("WritePlots", function(ServerOutput, list_of_paths) standardGeneric("WritePlots"))
  setMethod("WritePlots", signature("ServerOutput", "list"),
            function(ServerOutput, list_of_paths){
              ## gProfilerPlots
              gost_plots <- fetch_gostplots(ServerOutput)
              withr::with_dir(new = dirname(list_of_paths[[1]][1]),
                              code =               for(i in 1:length(gost_plots)){
                                htmlwidgets::saveWidget(gost_plots[[i]]@Plot,
                                                        file = list_of_paths[[1]][i],
                                                        selfcontained = TRUE)
                              })

              violin_plots <- fetch_violin_plots(ServerOutput)
              boxplots <- fetch_boxplots(ServerOutput)
              density_plots <- fetch_density_plots(ServerOutput)

              ## Boxplots
              for(i in 1:length(violin_plots)){
                ggsave(filename = list_of_paths[[2]][i], plot = boxplots[[i]])
              }
              ## Violin Plots
              for(i in 1:length(violin_plots)){
                ggsave(filename = list_of_paths[[3]][i], plot = violin_plots[[i]], device = "pdf")
              }

              ## Density Plots
              for(i in 1:length(violin_plots)){
                ggsave(filename = list_of_paths[[4]][i], plot = density_plots[[i]], device = "pdf")
              }

              ## Diagnostics
              for(i in 1:3){
                pdf(file = list_of_paths[[5]][i], width = 8, height = 6)
                switch(i,
                       {replayPlot(fetch_correlation_dendrogram(ServerOutput))},
                       {replayPlot(fetch_adjacency_heatmap(ServerOutput))},
                       {replayPlot(fetch_correlation_heatmap(ServerOutput))})
                dev.off()
              }

              ## SFT Plot
              pdf(file = list_of_paths[[6]], width = 8, height = 6)
              replayPlot(fetch_SFT_plot(ServerOutput))
              dev.off()

              ## Eigenprotein Heatmap
              pdf(file = list_of_paths[[7]], width = 12, height = 9)
              grid::grid.newpage()
              grid::grid.draw(fetch_Eigenprotein_heatmap(ServerOutput)$gtable)
              dev.off()

              ## TOMPlot
              png(filename = list_of_paths[[8]], width = 1200, height = 1200)
              replayPlot(fetch_TOM_plot(ServerOutput))
              dev.off()
            })

  ## Methods ####

  ## GenerateFileNames ####
  setMethod("GenerateFileNames", signature("ServerOutput", "character"),
            function(ServerOutput, temp_dir){

              ## You can't have spaces in temporary directories !!!
              gProfiler_names <- paste(file.path(temp_dir,
                                           "gProfiler_Plots",
                                           fetch_gProfiler_names(ServerOutput)),
                                       ".html",
                                       sep = "")
              MEPlots_names_boxplot <- file.path(temp_dir,
                                                 "Module_Eigenprotein_Plots",
                                                 "boxplots",
                                                 paste(fetch_MEPlots_names(ServerOutput),
                                                       "pdf",
                                                       sep = ".")
                                                 )
              MEPlots_names_violin <- file.path(temp_dir,
                                                "Module_Eigenprotein_Plots",
                                                "violin",
                                                paste(fetch_MEPlots_names(ServerOutput),
                                                      "pdf",
                                                      sep = ".")
                                                )
              MEPlots_names_density <- file.path(temp_dir,
                                                 "Module_Eigenprotein_Plots",
                                                 "density",
                                                 paste(fetch_MEPlots_names(ServerOutput),
                                                       "pdf",
                                                       sep = ".")
              )
              EPDiagnostics <- c("Correlation_Dendrogram",
                                 "Adjacency_Heatmap",
                                 "Correlation_Heatmap")
              EP_Names <- file.path(temp_dir,
                                    "Eigenprotein_Diagnostics",
                                    paste(EPDiagnostics,
                                          "pdf",
                                          sep = "."))
              SFT_names <- file.path(temp_dir, "Scale_Free_Topology_Plot.pdf")
              EP_heatmap <- file.path(temp_dir, "Eigenprotein_Heatmap.pdf")
              TOMPlot_names <- file.path(temp_dir, "TOMPlot.png")

              list_of_paths <- list("gProfiler_paths" = gProfiler_names,
                                    "MEPlots_paths_boxplot" = MEPlots_names_boxplot,
                                    "MEPlots_paths_violin" = MEPlots_names_violin,
                                    "MEPlots_paths_density" = MEPlots_names_density,
                                    "EP_Names" = EP_Names,
                                    "SFT_Names" = SFT_names,
                                    "EP_heatmap" = EP_heatmap,
                                    "TOMPlot_names" = TOMPlot_names)
              return(list_of_paths)
            })
  ## Generate DataFileNames ####
  setGeneric("DataFileNames", function(ServerOutput, temp_dir) standardGeneric("DataFileNames"))
  setMethod("DataFileNames", signature("ServerOutput", "character"),
            function(ServerOutput, temp_dir){
              csv_names <- c("Variance_Explained",
                                  "ModuleEigenproteins_tidy",
                                  "ModuleEigenproteinsWide")
              excel_names <- c("WGCNAResults_Module_Membership",
                               "gProfiler_Enrichment_Results")

              csv_filepaths <- file.path(temp_dir, paste(csv_names, "csv", sep = "."))
              excel_filepaths <- file.path(temp_dir, paste(excel_names, ".xlsx", sep = "."))

              list_of_paths <- list("variance_explained" = csv_filepaths[1],
                                    "ME_tidy" = csv_filepaths[2],
                                    "ME_wide" = csv_filepaths[3],
                                    "WGCNAResults" = excel_filepaths[1],
                                    "gProfiler" = excel_filepaths[2])
              return(list_of_paths)

            })
  ## WriteData ####
  setGeneric("WriteData", function(ServerOutput, list_of_paths) standardGeneric("WriteData"))
  setMethod("WriteData", signature("ServerOutput", "list"),
            function(ServerOutput, list_of_paths){
              ## CSVs
              write.csv(ServerOutput@Data@VarianceExplained, row.names = FALSE,
                        file = list_of_paths[[1]])
              write.csv(ServerOutput@Data@ModuleEigenproteinsTidy,
                        file = list_of_paths[[2]])
              write.csv(ServerOutput@Data@ModuleEigenproteinsWide,
                        file = list_of_paths[[3]])
              ## Excel sheets
              ## WGCNA Results
              sheetNames <- names(ServerOutput@Data@ModuleMembership)
              wb <- openxlsx::createWorkbook()
              for(i in 1:length(sheetNames)) {
                addWorksheet(wb, sheetNames[i])
                writeData(wb,
                          sheetNames[i],
                          ServerOutput@Data@ModuleMembership[[i]],
                          withFilter = TRUE)
              }
              openxlsx::saveWorkbook(wb,
                                     file = list_of_paths[[4]],
                                     overwrite = TRUE)
              ## gProfiler Results
              sheetNames <- names(ServerOutput@Data@gProfilerResults)
              wb <- openxlsx::createWorkbook()
              for(i in 1:length(sheetNames)) {
                addWorksheet(wb, sheetNames[i])
                writeData(wb,
                          sheetNames[i],
                          ServerOutput@Data@gProfilerResults[[i]]$result,
                          withFilter = TRUE)
              }
              openxlsx::saveWorkbook(wb,
                                     file = list_of_paths[[5]],
                                     overwrite = TRUE)

            })
## WriteResultsToTempFolder
  setGeneric("WriteResultsToTempFolder",
             function(ServerOutput, path) standardGeneric("WriteResultsToTempFolder"))
  setMethod("WriteResultsToTempFolder", signature("ServerOutput", "character"),
            function(ServerOutput, path){
              file_names_plots <- GenerateFileNames(ServerOutput, path)
              file_names_data <- DataFileNames(ServerOutput, path)

              WritePlots(ServerOutput, file_names_plots)
              WriteData(ServerOutput, file_names_data)
            })

  ## BuildAllPlots ####
  setMethod("BuildAllPlots", signature(METidy = "ModuleEigenproteinsTidy",
                                       WGCNAResults = "WGCNAResults",
                                       CleanData = "CleanData",
                                       WGCNAParameters = "WGCNAParameters",
                                       SFTObject = "ScaleFreeTopology",
                                       GostplotParameters = "GostPlotParameters",
                                       gProfilerResults = "gProfilerResults",
                                       comparisonsObject = "missingOrNULL"),
            function(METidy, WGCNAResults, CleanData, WGCNAParameters,
                     SFTObject, GostplotParameters, gProfilerResults,
                     comparisonsObject){
              ## Protein Dendrogram
              protein_dendrogram <- PlotProteinDendrogram(WGCNAResults)

              ## Scale Free Topology
              sft_plot <- ScaleFreeTopologyPlot(SFTObject = SFTObject)

              ## Eigenprotein Diagnostics
              EP_diagnostics <- PlotModuleEigenproteinDiagnostics(WGCNAResults)

              ## SampleClustering
              sample_clustering <- plotSampleClusteringDendro(CleanData)

              ## ModuleEigenproteinPlots
              ME_plots <- ModuleEigenproteinPlots(METidy,
                                                  comparisonsObject = comparisonsObject)

              ## gProfiler Plots
              gost_plots <- gostplot(GostPlotParameters = GostplotParameters,
                                     gProfilerResults = gProfilerResults)

              ## TOMPlot
              tom_plot <- plotTOM(CleanData, WGCNAResults, WGCNAParameters)

              eigenprotein_heatmap <- PlotEigenproteinHeatmap(WGCNAResults)

              plots_output <- CreatePlotsOutput(PathToOutput = NULL,
                                                gProfilerPlots = gost_plots,
                                                SampleClustering = sample_clustering,
                                                ProteinDendrogram = protein_dendrogram,
                                                EigenproteinDiagnostics = EP_diagnostics,
                                                ModuleEigenproteinPlots = ME_plots,
                                                TOMPlot = tom_plot,
                                                SFTPlot = sft_plot,
                                                EigenproteinHeatmap = eigenprotein_heatmap
              )
              plots_output
            })

  ## Accessor methods for ServerOutput Objects ####
  ##
  setMethod("fetch_gostplots", signature(plotsOutput = "ServerOutput"),
            function(plotsOutput){
              plotsOutput@Plots@gProfilerPlots@Plots
            })

  setMethod("fetch_SampleClustering", signature(plotsOutput = "PlotsOutput"),
            function(plotsOutput){
              plotsOutput@SampleClustering@Plot
            })

  setMethod("fetch_ProteinDendrogram", signature(plotsOutput = "PlotsOutput"),
            function(plotsOutput){
              plotsOutput@ProteinDendrogram@Plot
            })

  setMethod("fetch_EigenproteinDiagnostics", signature(plotsOutput = "PlotsOutput"),
            function(plotsOutput){
              plotsOutput@EigenproteinDiagnostics
            })

  setMethod("fetch_ModuleEigenproteinPlots", signature(plotsOutput = "ServerOutput"),
            function(plotsOutput){
              plotsOutput@Plots@ModuleEigenproteinPlots
            })

  setMethod("fetch_TOMPlot", signature(plotsOutput = "PlotsOutput"),
            function(plotsOutput){
              plotsOutput@TOMPlot
            })

  setMethod("fetch_SFTPlot", signature(plotsOutput = "PlotsOutput"),
            function(plotsOutput){
              plotsOutput@SFTPlot
            })

  setMethod("fetch_MEPlots_names", signature("ServerOutput"),
            definition = function(x){
              names(x@Plots@ModuleEigenproteinPlots@Violin)
            })

  setMethod("fetch_gProfiler_names", signature("ServerOutput"),
            definition = function(x){
              names(x@Plots@gProfilerPlots@Plots)
            })

  ## Transpose Tibble ####
  setMethod("transpose_tibble",
            signature(x = "list",
                      var = "character",
                      val = "character"),
            definition = function(x, var = "var", val = "val"){
              x %>%
                tidyr::gather(var, val, 2:ncol(x)) %>%
                tidyr::spread(names(x)[1], "val")
            })

  setMethod("transpose_tibble",
            signature(x = "tbl_df",
                      var = "character",
                      val = "character"),
            definition = function(x, var = "var", val = "val"){
              x %>%
                tidyr::gather(var, val, 2:ncol(x)) %>%
                tidyr::spread(names(x)[1], "val")
            })
  setMethod("transpose_tibble",
            signature(x = "tbl_df",
                      var = "missing",
                      val = "missing"),
            definition = function(x){
              x %>%
                tidyr::gather(var = "var", val = "val", 2:ncol(x)) %>%
                tidyr::spread(names(x)[1], "val")
            })

  ## Accessors for getting specific slots of ModuleEigenproteinPlots Objects ####
  setMethod("fetch_violin_plots", signature(MEPlots = "ServerOutput"),
            definition = function(MEPlots){
              MEPlots@Plots@ModuleEigenproteinPlots@Violin
            })
  setMethod("fetch_boxplots", signature(MEPlots = "ServerOutput"),
            definition = function(MEPlots){
              MEPlots@Plots@ModuleEigenproteinPlots@Boxplot
            })
  setMethod("fetch_boxplots", signature(MEPlots = "ModuleEigenproteinPlots"),
            definition = function(MEPlots){
              MEPlots@Boxplot
            })
  setMethod("fetch_density_plots", signature(MEPlots = "ServerOutput"),
            definition = function(MEPlots){
              MEPlots@Plots@ModuleEigenproteinPlots@Density
            })

  ## get_specific_MEPlot ####
  setMethod("get_specific_MEPlot", signature(parent = "ServerOutput",
                                             slot = "character",
                                             plot_name = "character"),
            definition = function(parent, slot, plot_name){

              MEPlots <- fetch_ModuleEigenproteinPlots(parent)
              selected_subset <- switch(slot,
                                        "Violin Plot" = {fetch_violin_plots(MEPlots)},
                                        "Boxplot" = {fetch_boxplots(MEPlots)},
                                        "Density Plot" = {fetch_density_plots(MEPlots)})
              selected_subset[[plot_name]]
            })

  ## get_specific_gostplot ####
  setMethod("get_specific_gostplot", signature(parent = "ServerOutput",
                                               plot_name = "character"),
            definition = function(parent, plot_name){
              fetch_gostplots(parent)[[plot_name]]@Plot
            })

  ## Find_nondata_column_indexes ####
  setMethod("find_nondata_column_indexes", signature(object = "RawData"),
            function(object){
              results <- unname(which(sapply(object@data, class) == "character"))
              object@NonDataColumns <- as.numeric(results)
              object
            })
  setMethod("find_nondata_column_indexes", signature(object = "list"),
            function(object){
              results <- unname(which(sapply(object@data, class) == "character"))
              object@NonDataColumns <- results
              object
            })

  ## ReadDataFromPath ####
  setMethod("ReadDataFromPath", signature("RawData"), function(object){
    path <- object@path
    object@data <- readr::read_csv(path,
                                   locale = readr::locale(encoding = "UTF-8"))
    find_nondata_column_indexes(object)
  })

  ## ReadDataFromPath
  setMethod("ReadDataFromPath", "ExperimentalGroups", function(object){
    path <- object@path
    object@data <- readr::read_csv(path,
                                   locale = readr::locale(encoding = "UTF-8"))
    object
  })

  ## RawData Accessors ####

  setMethod("path", "RawData", function(x) x@path)
  setMethod("path<-",
            signature(x = "RawData", value = "character"),
            function(x, value){
              x@path <- value
              validObject(x)
              x
            })
  setMethod("data", "RawData", function(x) x@data)
  setMethod("data<-",
            signature("RawData", "list"),
            function(x, value){
              x@data <- value
              validObject(x)
              x
            })
  setMethod("NonDataColumns", "RawData", function(x) x@NonDataColumns)
  setMethod("NonDataColumns<-", "RawData", function(x, value){
    x@NonDataColumns <- value
    validObject(x)
    x
  })

  ## CleanData object accessors ####
  setMethod("Data", "CleanData",
            definition = function(x) x@Data)
  setMethod("Data<-", "CleanData",
            function(x, value){
              x@Data <- value
              x
            })
  setMethod("NonDataColumns",
            "CleanData",
            definition = function(x) x@NonDataColumns)
  setMethod("NonDataColumns<-",
            "CleanData",
            definition = function(x, value){
              x@NonDataColumns <- value
              x
            })
  setMethod("Samples", "CleanData",
            definition = function(x) x@Samples)
  setMethod("Samples<-", "CleanData",
            definition = function(x, value){
              x@Samples <- value
              x
            })
  setMethod("Identifiers", "CleanData",
            definition = function(x) x@Identifiers)
  setMethod("Identifiers<-", "CleanData",
            definition = function(x,value){
              x@Identifiers
              x
            })

  ## Build Clean Data From Raw ####
  setMethod("BuildCleanDataFromRawData",
            signature(x = "RawData", y = "CleanData"),
            function(x, y){
              NonDataColumnsLength <- length(x@NonDataColumns)
              ColumnsToRemove <- c()

              if(NonDataColumnsLength > 1){
                ColumnsToRemove <- x@NonDataColumns[2:NonDataColumnsLength]
                y@Data <- transpose_tibble(x@data[,-ColumnsToRemove],
                                           var = "var",
                                           val = "val")
              }else{
                y@Data <- transpose_tibble(x@data,
                                           var = "var",
                                           val = "val")
              }
              y@NonDataColumns <- x@NonDataColumns
              y@Samples <- colnames(x@data[,-x@NonDataColumns])
              y@Identifiers <- x@data[[1]]
              validObject(y)
              y
            })

  ## RunBlockwiseModules ####
  setMethod("RunBlockwiseModules",
            signature(parameters = "WGCNAParameters",
                      cleaned_data = "CleanData"),
            definition = function(parameters, cleaned_data){
              results <- WGCNA::blockwiseModules(cleaned_data@Data[,-1],
                                                 power = parameters@power,
                                                 networkType = parameters@networkType,
                                                 replaceMissingAdjacencies = parameters@networkType,
                                                 TOMtype = parameters@TOMtype,
                                                 minModuleSize = parameters@minModuleSize,
                                                 mergeCutHeight = parameters@mergeCutHeight,
                                                 pamRespectsDendro = parameters@pamRespectsDendro,
                                                 deepSplit = parameters@deepSplit,
                                                 checkMissingData = parameters@checkMissingData,
                                                 maxBlockSize = parameters@maxBlockSize,
                                                 nPreclusteringCenters =
                                                   parameters@nPreclusteringCenters,
                                                 verbose = parameters@verbose
              )
              results
            })

  ## PairedComparison Object accessors
  setMethod("[[<-",
            signature = "PairedComparisons",
            definition = function(x,i,j,value){
              x@Comparisons[[i]] <- value
              return(x)
            })


  ## BuildExperimentalGroupsFromPath
  setMethod("BuildExperimentalGroupsFromPath",
            signature = "ExperimentalGroups", function(x){
              path <- x@path
              x@ExperimentalDataFrame <- readr::read_csv(path)
              x@ExperimentalGroups <- x@ExperimentalDataFrame[[2]]
              x@Sample <- x@ExperimentalDataFrame[[1]]
              x
            })

  ## WGCNAHclust plot ####
  setMethod("plot", "WGCNAHclust",
            definition = function(x){
              plot(x@hclust)
            })

  ## PlotDendroAndColors ####
  setMethod("plotDendroAndColors",
            signature(dendro = "WGCNAHclust", "WGCNAResults"),
            definition = function(dendro, colors){
              WGCNA::plotDendroAndColors(dendro = dendro@hclust,
                                         colors = colors@colors[colors@goodGenes],
                                         dendroLabels = FALSE)
            })
  setMethod("plotDendroAndColors",
            signature(dendro = "WGCNAHclust", "list"),
            definition = function(dendro, colors){
              WGCNA::plotDendroAndColors(dendro = dendro@hclust,
                                         colors = colors$colors[colors$goodGenes],
                                         dendroLabels = FALSE)
            })
  ## BulidModuleEigenproteinsTidy ####
  setMethod("BuildModuleEigenproteinsTidy",
            signature(ModuleEigenproteinsTidy = "ModuleEigenproteinsTidy",
                      WGCNAResults = "WGCNAResults",
                      ExperimentalGroups = "ExperimentalGroups"),
            definition = function(ModuleEigenproteinsTidy,
                                  WGCNAResults,
                                  ExperimentalGroups){
              ModuleEigenproteinsTidy@Data <- tibble::as_tibble(WGCNAResults@Data)

              untidy_ME_data <- WGCNAResults@MEs %>%
                tibble::add_column(WGCNAResults@Samples, .before = 1) %>%
                tibble::add_column(WGCNAResults@ExperimentalGroups, .before = 2)
              names(untidy_ME_data)[1:2] <- c("Sample", "ExperimentalGroup")
              tidy_ME_data <- tidyr::gather(untidy_ME_data,
                                            key = "Module",
                                            value = "Eigenprotein",
                                            3:ncol(untidy_ME_data))
              ModuleEigenproteinsTidy@ModuleEigenproteins <- tidy_ME_data
              ModuleEigenproteinsTidy
            })

  ## BuildModuleMembership
  setMethod("BuildModuleMembership",
            signature(ModuleMembershipObject = "ModuleMembership",
                      WGCNAResultsObject = "WGCNAResults",
                      UniprotDatabaseObject = "UniprotDatabase",
                      RawDataObject = "RawData"),
            definition = function(ModuleMembershipObject,
                                  WGCNAResultsObject,
                                  UniprotDatabaseObject,
                                  RawDataObject){
              WGCNA_All_Data <- RawDataObject@data
              WGCNA_for_KME_calc <- WGCNAResultsObject@Data[,-1]
              UniprotDatabase <- UniprotDatabaseObject@Database
              ## Get the signed correlation constant between protein i and module j
              kmes <- WGCNA::signedKME(WGCNA_for_KME_calc, WGCNAResultsObject@MEs)
              ## Get the colors for each protein
              ModuleColors <- WGCNAResultsObject@colors
              ## Combine into a single data frame
              all_results <- data.frame(WGCNA_All_Data, kmes, ModuleColors)

              ## The lapply and abstract function:
              ## (1) ModuleColors is converted into a factor, then lapply is called
              ## to iterate over the factors
              ## (2) The iteration uses each level of ModuleColors to pull the corresponding
              ## accesssions into its own sheet.
              ## (3) The results are ordered so that the highest kMEs are on top of the list.
              ## (4) Finally, the setdiff function is used to exclude all kME columns
              ## not associated with the current ModuleColor.
              all_results_list <- lapply(levels(as.factor(ModuleColors)),
                                         function(x) {dtemp = all_results[all_results$ModuleColors == x,]
                                         dtemp[order(dtemp[,paste0('kME',x) == colnames(dtemp)],
                                                     decreasing=TRUE),
                                               -setdiff(grep("^kME", colnames(dtemp)),
                                                        which(paste0('kME',x) == colnames(dtemp)))]
                                         })
              names(all_results_list) <- levels(as.factor(ModuleColors))
              all_results_list[["AllData"]] <- all_results
              all_data_and_modules <- lapply(all_results_list, tibble::tibble)
              All_Data_Output <- lapply(names(all_data_and_modules),
                                        function(x){
                                          dplyr::left_join(all_data_and_modules[[x]],
                                                           UniprotDatabase,
                                                           by = names(all_data_and_modules[[1]][1]))
                                        })
              names(All_Data_Output) <- names(all_data_and_modules)
              ModuleMembershipObject@Modules <- All_Data_Output
              ModuleMembershipObject
            })

  ## gost
  setMethod("gost", signature(gProfilerParameters = "gProfilerParameters2",
                              ModuleMembership = "ModuleMembership"),
            definition = function(gProfilerParameters,
                                  ModuleMembership){

              remove_all_data_element <- which(names(ModuleMembership@Modules) == "AllData")
              gprofiler_results <- new("gProfilerResults")
              gprofiler_results@Parameters <- gProfilerParameters
              gprofiler_results@EnrichmentResults <-
                lapply(ModuleMembership@Modules[-remove_all_data_element], function(x){
                  gprofiler2::gost(x[[1]],
                                   organism = gProfilerParameters@organism,
                                   ordered_query = gProfilerParameters@ordered_query,
                                   multi_query = gProfilerParameters@multi_query,
                                   significant = gProfilerParameters@significant,
                                   exclude_iea = gProfilerParameters@exclude_iea,
                                   measure_underrepresentation =
                                     gProfilerParameters@measure_underrepresentation,
                                   evcodes = gProfilerParameters@evcodes,
                                   user_threshold = gProfilerParameters@user_threshold,
                                   correction_method = gProfilerParameters@correction_method,
                                   domain_scope = gProfilerParameters@domain_scope,
                                   custom_bg = gProfilerParameters@custom_bg,
                                   numeric_ns = gProfilerParameters@numeric_ns,
                                   sources = gProfilerParameters@sources,
                                   as_short_link = gProfilerParameters@as_short_link)
                })
              gprofiler_results
            })
  ## gostplot ####
  setMethod("gostplot", signature(GostPlotParameters = "GostPlotParameters",
                                  gProfilerResults = "gProfilerResults"),

            definition = function(GostPlotParameters, gProfilerResults){

              PlotsOutput <- new("GostPlotCollection")
              modules <- names(gProfilerResults@EnrichmentResults)
              modules_length <- length(modules)
              plots <- new("GostPlotCollection")
              for(i in 1:modules_length){
                temp_gProfilerPlot <- new("gProfilerPlot")
                temp_gProfilerPlot@Parameters <- GostPlotParameters
                temp_gProfilerPlot@Plot <-
                  gprofiler2::gostplot(gostres = gProfilerResults@EnrichmentResults[[i]],
                                       capped = GostPlotParameters@capped,
                                       interactive = GostPlotParameters@interactive)
                plots[[names(gProfilerResults@EnrichmentResults)[i]]] <- temp_gProfilerPlot
              }

              plots
            })

  ## GostPlotCollection accessors ####
  setMethod("[[<-",
            signature = "GostPlotCollection",
            definition = function(x,i,j,value){
              x@Plots[[i]] <- value
              return(x)
            })

  ## plotSampleClusteringDendro ####
  setMethod("plotSampleClusteringDendro", signature("CleanData"),
            definition = function(CleanData){
              dendro_results <- new("SampleClustering")
              sample_tree <- fastcluster::hclust(dist(CleanData@Data[,-1]))
              sample_tree$labels <- CleanData@Samples
              dendro_output <- ggdendro::ggdendrogram(sample_tree, rotate = TRUE) +
                ggplot2::ggtitle("Sample Clustering to Detect Outliers") +
                ggplot2::xlab("Sample") +
                ggplot2::ylab("Height") +
                ggplot2::coord_flip() +
                ggplot2::theme_bw(base_size = 15)

              dendro_results@Plot <- dendro_output
              dendro_results
            })

  ## PickSoftThreshold ####
  check_soft_threshold_power_estimate <- function(object){
    if(!is.na(object$powerEstimate)){TRUE}else{FALSE}
  }
  setMethod("PickSoftThreshold",
            signature(PowerObject = "PowerDeterminationParameters",
                      CleanData = "CleanData"),
            definition = function(PowerObject, CleanData){
              ScaleFreeTopologyObject <- new("ScaleFreeTopology")
              sft <- WGCNA::pickSoftThreshold(CleanData@Data[,-1],
                                              RsquaredCut = PowerObject@R_squared_cutoff,
                                              powerVector = PowerObject@powerVector,
                                              verbose = 5)
              ## check_soft_threshold_power_estimate makes sure that sft is returning a non-NA
              ##  value for the power estimate.
              if (!check_soft_threshold_power_estimate(sft)){
                message("Power determination failed to reach scale-free topology threshold. \n
                        Defaulting to power selection based on the number of samples.")
                number_samples <- length(CleanData@Samples)
                if (PowerObject@networkType == "signed"){
                  if (number_samples < 20) sft$powerEstimate <- 18
                  if (number_samples >= 20 & number_samples < 30) sft$powerEstimate <- 16
                  if (number_samples >= 30 & number_samples < 40) sft$powerEstimate <- 14
                  if (number_samples >= 40) sft$powerEstimate <- 12
                }else if (PowerObject@networkType == "unsigned"){
                  if (number_samples < 20) sft$powerEstimate <- 9
                  if (number_samples >= 20 & number_samples < 30) sft$powerEstimate <- 8
                  if (number_samples >= 30 & number_samples < 40) sft$powerEstimate <- 7
                  if (number_samples >= 40) sft$powerEstimate <- 6
                }
              }

              ScaleFreeTopologyObject@fitIndices <- sft$fitIndices
              ScaleFreeTopologyObject@Parameters <- PowerObject
              ScaleFreeTopologyObject@powerEstimate <- sft$powerEstimate
              if(PowerObject@UserDefinedPower == TRUE){
                message(paste0("User power selected using power ", PowerObject@OverridePower))
                ScaleFreeTopologyObject@powerEstimate <- PowerObject@OverridePower
              }
              ScaleFreeTopologyObject
            })

  ## ScaleFreeTopologyPlot ####
  setMethod("ScaleFreeTopologyPlot",
            signature(SFTObject = "ScaleFreeTopology"),
            definition = function(SFTObject){
              par(mfrow = c(1,2))
              ## First Plot
              plot(SFTObject@fitIndices[,1],-sign(SFTObject@fitIndices[,3])*SFTObject@fitIndices[,2],
                   xlab = "Power",
                   ylab = "Scale Free Topology Model Fit, signed R^2",
                   type = "n",
                   main = paste("Scale Independence"))
              graphics::text(SFTObject@fitIndices[,1],
                             -sign(SFTObject@fitIndices[,3])*SFTObject@fitIndices[,2],
                             labels = SFTObject@Parameters@powerVector,
                             cex = 0.9,
                             col = "red")
              graphics::abline(h = SFTObject@Parameters@R_squared_cutoff, col = "red")

              ## Second plot
              plot(SFTObject@fitIndices[,1],
                   SFTObject@fitIndices[,5],
                   xlab = "Soft Threshold (power)",
                   ylab = "Mean Connectivity",
                   type = "n",
                   main = paste("Mean connectivity"))
              graphics::text(SFTObject@fitIndices[,1],
                             SFTObject@fitIndices[,5],
                             labels = SFTObject@Parameters@powerVector,
                             cex = 0.9,
                             col = "red")
              SFTObject@Plot <- recordPlot()
              SFTObject
            })

  ## plotEigenproteinNetwork ####
  setMethod("plotEigenproteinNetwork",
            signature(Results = "WGCNAResults"),
            definition = function(Results){

              EigenproteinNetwork <- new("EigenproteinNetwork")
              EigenproteinNetwork@Plot <- recordPlot(
                WGCNA::plotEigengeneNetworks(Results@MEs,
                                             "Eigenprotein Network",
                                             marHeatmap = c(3,4,2,2),
                                             marDendro = c(3,4,2,5),
                                             plotDendrograms = TRUE,
                                             xLabelsAngle = 90)
              )
              EigenproteinNetwork
            })

  setMethod("PlotProteinDendrogram", signature(Results = "WGCNAResults"),
            definition = function(Results){
              ProteinDendrogram <- new("ProteinDendrogram")

              WGCNA::plotDendroAndColors(dendro = Results@dendrogram@hclust,
                                         colors = data.frame(Results@unmergedColors[Results@goodGenes == TRUE],
                                                             Results@colors[Results@goodGenes == TRUE]),
                                         groupLabels = c("Unmerged Modules",
                                                         "Merged Modules"),
                                         dendroLabels = FALSE)
              ProteinDendrogram@Plot <- recordPlot()
              ProteinDendrogram
            })
  setMethod("PlotEigenproteinHeatmap", signature(WGCNAResults = "WGCNAResults"),
            definition = function(WGCNAResults){
              EP_Heatmap <- new("EigenproteinHeatmap")

              MEs_matrix <- as.matrix(WGCNAResults@MEs)
              ## Define dist and hclust functions for use by pheatmap
              distFunction <- function(x) as.dist(1 - abs(cor(x,
                                                              method = "pearson")))
              hclustFunction <- function(x) hclust(x, method = "average")

              ## Generate the dendrograms
              hclust.col <- hclustFunction(distFunction(WGCNAResults@MEs))
              hclust.row <- hclustFunction(distFunction(t(WGCNAResults@MEs)))
              hclust.row$labels <- WGCNAResults@Samples

              ## Create row annotations dataframe

              rownames(MEs_matrix) <- WGCNAResults@Samples
              rownames(MEs_matrix) <- rownames(MEs_matrix)
              ## Need the above trick because using rownames() <- doesn't set the
              ## rownames attribute within object matrix that pheatmap uses.
              ## Which is dumb, but whatever I guess.
              ## Only took me an hour to figure that out

              ## Row annotations
              ann_row <- data.frame(Condition = WGCNAResults@ExperimentalGroups)
              rownames(ann_row) <- WGCNAResults@Samples

              ## Col annotations
              ## Current col names are ME + color. Use substring to pull only the
              ##  color name
              ann_col <- substr(hclust.col$labels, 3, nchar(hclust.col$labels))
              col_df <- data.frame(module = ann_col)
              rownames(col_df) <- hclust.col$labels

              ## Create color scheme
              scale_colors <- RColorBrewer::brewer.pal(11, "PRGn")

              EP_Heatmap@Plot <- pheatmap::pheatmap(MEs_matrix,
                                                    cluster_cols = hclust.col,
                                                    cluster_rows = hclust.row,
                                                    annotation_row = ann_row,
                                                    scale = "column",
                                                    color = scale_colors,
                                                    show_rownames = TRUE)
              EP_Heatmap

            })
  setMethod("PlotModuleEigenproteinDiagnostics", signature(WGCNAResults = "WGCNAResults"),
            definition = function(WGCNAResults){

              MEOutput <- new("ModuleEigenproteinDiagnostics")
              MEs <- WGCNAResults@MEs
              MET <- WGCNA::orderMEs(MEs = MEs)


              WGCNA::plotEigengeneNetworks(MET,
                                           "Eigenprotein Dendrogram \n (Distance: 1 - corr(i,j))",
                                           marDendro = c(0,4,2,0),
                                           plotHeatmaps = FALSE)
              MEOutput@CorrDendrogram <- recordPlot()

              WGCNA::plotEigengeneNetworks(MET,
                                           "Eigenprotein Adjacency Heatmap",
                                           marHeatmap = c(3,4,2,2),
                                           xLabelsAngle = 90,
                                           plotAdjacency = TRUE,
                                           plotDendrograms = FALSE)
              MEOutput@AdjacencyHeatmap  <- recordPlot()

              WGCNA::plotEigengeneNetworks(MET,
                                           "Eigenprotein Correlation Heatmap",
                                           marHeatmap = c(3,4,2,2),
                                           plotDendrograms = FALSE,
                                           plotAdjacency = FALSE,
                                           xLabelsAngle = 90,
                                           heatmapColors = WGCNA::blueWhiteRed(50))
              MEOutput@CorrHeatmap <- recordPlot()

              MEOutput
            })
  ## ModuleEigenprotein Plotting methods ####
  ## Facet Wrapped
  ## Boxplot
  setMethod("BoxPlot_ModuleFacetWrapped_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "PairedComparisons"),
            definition = function(METidy, comparisons){
              x <- tibble::tibble(METidy@ModuleEigenproteins)
              comps <- comparisons@Comparisons
              ggplot(data = x,
                     mapping = aes(x = `ExperimentalGroup`,
                                   y = `Eigenprotein`)) +
                geom_boxplot() +
                facet_wrap(~ `Module`) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90)) +
                ggpubr::stat_compare_means(method = "t.test",
                                           label = "p.signif",
                                           comparisons = comps)

            })


  setMethod("BoxPlot_ModuleFacetWrapped_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "missingOrNULL"),
            definition = function(METidy, comparisons){

              x <- tibble::tibble(METidy@ModuleEigenproteins)
              ggplot(data = x,
                     mapping = aes(x = `ExperimentalGroup`,
                                   y = `Eigenprotein`)) +
                geom_boxplot() +
                facet_wrap(~ `Module`) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90))
            })
  ## Violin Plot
  setMethod("ViolinPlot_ModuleFacetWrapped_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "PairedComparisons"),
            definition = function(METidy, comparisons){

              x <- tibble::tibble(METidy@ModuleEigenproteins)
              comps <- comparisons@Comparisons

              ggplot(data = x,
                     mapping = aes(x = `ExperimentalGroup`,
                                   y = `Eigenprotein`)) +
                geom_violin(draw_quantiles = 0.5) +
                facet_wrap(~ `Module`) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90)) +
                ggpubr::stat_compare_means(method = "t.test",
                                           label = "p.signif",
                                           comparisons = comps)

            })

  setMethod("ViolinPlot_ModuleFacetWrapped_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "missingOrNULL"),
            definition = function(METidy, comparisons){

              x <- tibble::tibble(METidy@ModuleEigenproteins)
              ggplot(data = x,
                     mapping = aes(x = `ExperimentalGroup`,
                                   y = `Eigenprotein`)) +
                geom_violin(draw_quantiles = 0.5) +
                facet_wrap(~ `Module`) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90))
            })

  ## Density Plot
  setMethod("DensityPlot_ModuleFacetWrapped_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy"),
            definition = function(METidy){

              x <- tibble::tibble(METidy@ModuleEigenproteins)

              ME_means_by_group <- x %>%
                group_by(`ExperimentalGroup`, `Module`)%>%
                dplyr::summarise("mean" = mean(`Eigenprotein`))

              ggplot(data = x,
                     mapping = aes(x = `Eigenprotein`,
                                   col = `ExperimentalGroup`,
                                   fill = `ExperimentalGroup`)) +
                geom_density(alpha = 0.4) +
                geom_vline(data = ME_means_by_group,
                           aes(xintercept = `mean`,
                               col = `ExperimentalGroup`)) +
                facet_wrap(~ `Module`, scales = "free") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90))
            })


  ## Individual Plots BoxPlot
  setMethod("BoxPlot_IndividualModule_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "PairedComparisons"),
            definition = function(METidy,
                                  comparisons){
              ME_data <- tibble::tibble(METidy@ModuleEigenproteins)
              comps <- comparisons@Comparisons

              ggplot_output_list <- list()
              unique_modules <- unique(ME_data$Module)

              for(i in 1:length(unique_modules)){
                temp_data <- ME_data %>%
                  filter(`Module` == unique_modules[i])

                ggplot_output_list[[i]] <- ggplot(data = temp_data,
                                                  mapping = aes(x = `ExperimentalGroup`,
                                                                y = `Eigenprotein`)) +
                  geom_boxplot() +
                  ggtitle(paste(unique_modules[i])) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle = 90)) +
                  ggpubr::stat_compare_means(method = "t.test",
                                             label = "p.signif",
                                             comparisons = comps)
              }
              ggplot_output_list
            })
  setMethod("BoxPlot_IndividualModule_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "missingOrNULL"),
            definition = function(METidy,
                                  comparisons){
              ME_data <- tibble::tibble(METidy@ModuleEigenproteins)

              ggplot_output_list <- list()
              unique_modules <- unique(ME_data$Module)

              for(i in 1:length(unique_modules)){
                temp_data <- ME_data %>%
                  filter(`Module` == unique_modules[i])

                ggplot_output_list[[i]] <- ggplot(data = temp_data,
                                                  mapping = aes(x = `ExperimentalGroup`,
                                                                y = `Eigenprotein`)) +
                  geom_boxplot() +
                  ggtitle(paste(unique_modules[i])) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle = 90))
              }
              ggplot_output_list
            })
  ## Individual Plots Violin Plot
  setMethod("ViolinPlot_IndividualModule_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "PairedComparisons"),
            definition = function(METidy,
                                  comparisons){
              ME_data <- tibble::tibble(METidy@ModuleEigenproteins)
              comps <- comparisons@Comparisons

              ggplot_output_list <- list()
              unique_modules <- unique(ME_data$Module)

              for(i in 1:length(unique_modules)){
                temp_data <- ME_data %>%
                  filter(`Module` == unique_modules[i])

                ggplot_output_list[[i]] <- ggplot(data = temp_data,
                                                  mapping = aes(x = `ExperimentalGroup`,
                                                                y = `Eigenprotein`)) +
                  geom_violin(draw_quantiles = 0.5) +
                  ggtitle(paste(unique_modules[i])) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle = 90)) +
                  ggpubr::stat_compare_means(method = "t.test",
                                             label = "p.signif",
                                             comparisons = comps)
              }
              ggplot_output_list
            })
  setMethod("ViolinPlot_IndividualModule_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "missingOrNULL"),
            definition = function(METidy,
                                  comparisons){
              ME_data <- tibble::tibble(METidy@ModuleEigenproteins)

              ggplot_output_list <- list()
              unique_modules <- unique(ME_data$Module)

              for(i in 1:length(unique_modules)){
                temp_data <- ME_data %>%
                  filter(`Module` == unique_modules[i])

                ggplot_output_list[[i]] <- ggplot(data = temp_data,
                                                  mapping = aes(x = `ExperimentalGroup`,
                                                                y = `Eigenprotein`)) +
                  geom_violin(draw_quantiles = 0.5) +
                  ggtitle(paste(unique_modules[i])) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle = 90))
              }
              ggplot_output_list
            })

  ## Density Plot
  setMethod("DensityPlot_IndividualModule_by_Experiment",
            signature(METidy = "ModuleEigenproteinsTidy",
                      comparisons = "missingOrNULL"),
            definition = function(METidy,
                                  comparisons){
              ME_data <- tibble::tibble(METidy@ModuleEigenproteins)

              ggplot_output_list <- list()
              unique_modules <- unique(ME_data$Module)

              for(i in 1:length(unique_modules)){
                temp_data <- ME_data %>%
                  filter(`Module` == unique_modules[i])

                ggplot_output_list[[i]] <- ggplot(data = temp_data,
                                                  mapping = aes(x = `Eigenprotein`,
                                                                col = `ExperimentalGroup`,
                                                                fill = `ExperimentalGroup`)) +
                  geom_density(alpha = 0.4) +
                  ggtitle(paste(unique_modules[i])) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle = 90))
              }
              ggplot_output_list
            })
  ## plotTOM ####
  setMethod("plotTOM", signature(CleanData = "CleanData",
                                 WGCNAResults = "WGCNAResults",
                                 WGCNAParameters = "WGCNAParameters"),
            function(CleanData, WGCNAResults, WGCNAParameters){
              tomplot_output <- new("TOMPlot")
              sim_TOM <- WGCNA::TOMsimilarityFromExpr(CleanData@Data[,-1],
                                                      networkType = WGCNAParameters@networkType,
                                                      power = WGCNAParameters@power,
                                                      TOMType = WGCNAParameters@networkType,
                                                      replaceMissingAdjacencies =
                                                        WGCNAParameters@replaceMissingAdjacencies)
              dissim_TOM <- 1 - sim_TOM

              WGCNA::TOMplot(
                dissim = dissim_TOM^6,
                dendro = WGCNAResults@dendrogram@hclust,
                ColorsLeft = WGCNAResults@colors[WGCNAResults@goodGenes == TRUE]
              )
              tomplot_output@Plot <- recordPlot()
              tomplot_output
            })
  ## CreatePlotOutput ####
  setMethod("CreatePlotsOutput", signature(PathToOutput = "characterOrNULL",
                                           gProfilerPlots = "GostPlotCollection",
                                           SampleClustering = "SampleClustering",
                                           ProteinDendrogram = "ProteinDendrogram",
                                           EigenproteinDiagnostics =
                                             "ModuleEigenproteinDiagnostics",
                                           ModuleEigenproteinPlots =
                                             "ModuleEigenproteinPlots",
                                           TOMPlot = "TOMPlot",
                                           SFTPlot = "ScaleFreeTopology",
                                           EigenproteinHeatmap = "EigenproteinHeatmap"),
            definition = function(PathToOutput,
                                  gProfilerPlots,
                                  SampleClustering,
                                  ProteinDendrogram,
                                  EigenproteinDiagnostics,
                                  ModuleEigenproteinPlots,
                                  TOMPlot,
                                  SFTPlot,
                                  EigenproteinHeatmap){
              PlotsOutput <- new("PlotsOutput")
              PlotsOutput@PathToOutput <- ifelse(is.null(PathToOutput),
                                                 "", PathToOutput)
              PlotsOutput@gProfilerPlots <- gProfilerPlots
              PlotsOutput@SampleClustering <- SampleClustering
              PlotsOutput@ProteinDendrogram <- ProteinDendrogram
              PlotsOutput@EigenproteinDiagnostics <- EigenproteinDiagnostics
              PlotsOutput@ModuleEigenproteinPlots <- ModuleEigenproteinPlots
              PlotsOutput@TOMPlot <- TOMPlot
              PlotsOutput@SFTPlot <- SFTPlot
              PlotsOutput@EigenproteinHeatmap <- EigenproteinHeatmap
              PlotsOutput
            })

  ## CreateServerOutput ####
  setMethod("CreateServerOutput", signature(PlotsOutput = "PlotsOutput",
                                            DataOutput = "DataOutput"),
            function(PlotsOutput, DataOutput){

              serveroutput <- new("ServerOutput")

              serveroutput@Plots <- PlotsOutput
              serveroutput@Data <- DataOutput
              serveroutput

            })
  setMethod("CreateServerOutput", signature(PlotsOutput = "PlotsOutput",
                                            DataOutput = "missing"),
            function(PlotsOutput, DataOutput){

              serveroutput <- new("ServerOutput")

              serveroutput@Plots <- PlotsOutput
              serveroutput

            })

  ### Accessors for: get_specific_diagnostic_plot ####
  # Accessors specific for each type of plot:
  # Protein Dendrogram
  setMethod("fetch_Protein_Dendrogram",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@ProteinDendrogram@Plot
            })
  # Module Correlation Dendrogram
  setMethod("fetch_correlation_dendrogram",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@EigenproteinDiagnostics@CorrDendrogram
            })
  # Module Adjacency Heatmap
  setMethod("fetch_adjacency_heatmap",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@EigenproteinDiagnostics@AdjacencyHeatmap
            })
  # Module Correlation Heatmap
  setMethod("fetch_correlation_heatmap",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@EigenproteinDiagnostics@CorrHeatmap
            })
  # Module Eigenprotein Heatmap
  setMethod("fetch_Eigenprotein_heatmap",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@EigenproteinHeatmap@Plot
            })
  # ScaleFreeTopology Plots
  setMethod("fetch_SFT_plot",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@SFTPlot@Plot
            })
  # TOM Plot
  setMethod("fetch_TOM_plot",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@TOMPlot@Plot
            })
  # Sample Clustering
  setMethod("fetch_SampleClustering_dendro",
            signature(parent = "ServerOutput"),
            function(parent){
              parent@Plots@SampleClustering@Plot
            })
  ### get_specific_diagnostic_plot ####
  setMethod("get_specific_diagnostic_plot",
            signature(parent = "ServerOutput",
                      plot = "character"),
            definition = function(parent, plot){
              switch(plot,
                     "Protein Dendrogram" = {fetch_Protein_Dendrogram(parent)},
                     "Module Correlation Dendrogram" = {fetch_correlation_dendrogram(parent)},
                     "Module Adjacency Heatmap" = {fetch_adjacency_heatmap(parent)},
                     "Module Correlation Heatmap" = {fetch_correlation_heatmap(parent)},
                     "Module Eigenprotein Heatmap" = {fetch_Eigenprotein_heatmap(parent)},
                     "Scale Free Topology Plots" = {fetch_SFT_plot(parent)},
                     "TOM Plot" = {fetch_TOM_plot(parent)},
                     "Sample Clustering" = fetch_SampleClustering_dendro(parent))

            })

  ### Methods to display data ####
  setGeneric("prop_var_output", function(ServerOutput) standardGeneric("prop_var_output"))
  setMethod("prop_var_output", "ServerOutput", function(ServerOutput){
    prop_var <- ServerOutput@Data@VarianceExplained
  })
  ### Accessor methods to create DataOutput object ####
  setMethod("BuildDataOutput", signature(METidy = "ModuleEigenproteinsTidy",
                                         WGCNAResults = "WGCNAResults",
                                         gProfiler = "gProfilerResults",
                                         CleanData = "CleanData",
                                         MM_object = "ModuleMembership"),
            function(METidy,
                     WGCNAResults,
                     gProfiler,
                     CleanData,
                     MM_object){
              MEs_wide <- fetch_MEs_wide(WGCNAResults)
              MEs_tidy <- fetch_MEs_tidy(METidy)
              gProfiler_results <- fetch_gProfiler_results(gProfiler)

              Data_Output <- new("DataOutput")

              Data_Output@VarianceExplained  <- calc_prop_var(WGCNAResults = WGCNAResults,
                                                              CleanData = CleanData) %>%
                data.frame() %>%
                tibble::rownames_to_column(var = "Percent Variation") %>%
                tibble::tibble()

              Data_Output@ModuleEigenproteinsTidy <- MEs_tidy
              Data_Output@ModuleEigenproteinsWide <- MEs_wide
              Data_Output@gProfilerResults <- gProfiler_results
              Data_Output@ModuleMembership <- fetch_Module_Membership(MM_object = MM_object)
              Data_Output
            })

  setMethod("fetch_MEs_wide",
            "WGCNAResults",
            function(WGCNAResults){
              mes_temp <- tibble(WGCNAResults@MEs)
              samples <- WGCNAResults@Samples
              mes_output <- add_column(mes_temp, "Samples" = samples, .before = 1)
              mes_output
            })
  setMethod("fetch_MEs_wide",
            "ServerOutput",
            function(WGCNAResults){
              WGCNAResults@Data@ModuleEigenproteinsWide
            })

  setMethod("fetch_MEs_tidy", signature("ModuleEigenproteinsTidy"),
            definition = function(METidy){
              mes_output <- tibble(METidy@ModuleEigenproteins)
            })

  setMethod("fetch_gProfiler_results", signature(gProfilerResults = "gProfilerResults"),
            definition = function(gProfilerResults){
              gProfilerResults@EnrichmentResults
            })
  setMethod("fetch_gProfiler_results", signature(gProfilerResults = "ServerOutput"),
            definition = function(gProfilerResults){
              gProfilerResults@Data@gProfilerResults
            })

  setMethod("fetch_gProfiler_parameters", signature(gProfilerResults = "gProfilerResults"),
            function(gProfilerResults){
              gProfilerResults@Parameters
            })

  setMethod("fetch_Module_Membership", signature("ModuleMembership"),
            definition = function(MM_object){
              MM_object@Modules
            })
  setMethod("fetch_Module_Membership", signature("ServerOutput"),
            definition = function(MM_object){
              MM_object@Data@ModuleMembership
            })

  setMethod("calc_prop_var", "WGCNAResults",
            definition = function(WGCNAResults, CleanData){
              WGCNA::propVarExplained(datExpr = CleanData@Data[,-1],
                                      colors = WGCNAResults@colors,
                                      MEs = WGCNAResults@MEs)
            })

  setMethod("fetch_specific_gProfiler_result",
            signature(parent = "ServerOutput",
                      plot_name = "character"),
            definition = function(parent, plot_name){
              fetch_gProfiler_results(parent)[[plot_name]]$result
            })
  setGeneric("fetch_specific_ModMemb_result", function(parent, plot_name)
    standardGeneric("fetch_specific_ModMemb_result"))
  setMethod("fetch_specific_ModMemb_result",
            signature(parent = "ServerOutput",
                      plot_name = "character"),
            definition = function(parent, plot_name){
              fetch_Module_Membership(parent)[[plot_name]]
            })


  ### PlotProteinDendrogram
  setMethod("PlotProteinDendrogram", signature(Results = "WGCNAResults"),
            definition = function(Results){
              ProteinDendrogram <- new("ProteinDendrogram")

              WGCNA::plotDendroAndColors(dendro = Results@dendrogram@hclust,
                                         colors = data.frame(Results@unmergedColors[Results@goodGenes == TRUE],
                                                             Results@colors[Results@goodGenes == TRUE]),
                                         groupLabels = c("Unmerged Modules",
                                                         "Merged Modules"),
                                         dendroLabels = FALSE)
              ProteinDendrogram@Plot <- recordPlot()
              ProteinDendrogram
            })
  setMethod("plotEigenproteinNetwork",
            signature(Results = "WGCNAResults"),
            definition = function(Results){

              EigenproteinNetwork <- new("EigenproteinNetwork")
              EigenproteinNetwork@Plot <- recordPlot(
                WGCNA::plotEigengeneNetworks(Results@MEs,
                                             "Eigenprotein Network",
                                             marHeatmap = c(3,4,2,2),
                                             marDendro = c(3,4,2,5),
                                             plotDendrograms = TRUE,
                                             xLabelsAngle = 90,
                                             heatmapColors = WGCNA::blueWhiteRed(50))
              )
              EigenproteinNetwork
            })

  ## PlotEigenproteinHeatmap ####
  setMethod("PlotEigenproteinHeatmap", signature(WGCNAResults = "WGCNAResults"),
            definition = function(WGCNAResults){
              EP_Heatmap <- new("EigenproteinHeatmap")

              MEs_matrix <- as.matrix(WGCNAResults@MEs)
              ## Define dist and hclust functions for use by pheatmap
              distFunction <- function(x) as.dist(1 - abs(cor(x,
                                                              method = "pearson")))
              hclustFunction <- function(x) hclust(x, method = "average")

              ## Generate the dendrograms
              hclust.col <- hclustFunction(distFunction(WGCNAResults@MEs))
              hclust.row <- hclustFunction(distFunction(t(WGCNAResults@MEs)))
              hclust.row$labels <- WGCNAResults@Samples

              ## Create row annotations dataframe

              rownames(MEs_matrix) <- WGCNAResults@Samples
              rownames(MEs_matrix) <- rownames(MEs_matrix)
              ## Need the above trick because using rownames() <- doesn't set the
              ## rownames attribute within object matrix that pheatmap uses.
              ## Which is dumb, but whatever I guess.
              ## Only took me an hour to figure that out

              ## Row annotations
              ann_row <- data.frame(Condition = WGCNAResults@ExperimentalGroups)
              rownames(ann_row) <- WGCNAResults@Samples

              ## Col annotations
              ## Current col names are ME + color. Use substring to pull only the
              ##  color name
              ann_col <- substr(hclust.col$labels, 3, nchar(hclust.col$labels))
              col_df <- data.frame(module = ann_col)
              rownames(col_df) <- hclust.col$labels

              ## Create color scheme
              scale_colors <- RColorBrewer::brewer.pal(11, "PRGn")

              EP_Heatmap@Plot <- pheatmap::pheatmap(MEs_matrix,
                                                    cluster_cols = hclust.col,
                                                    cluster_rows = hclust.row,
                                                    annotation_row = ann_row,
                                                    scale = "column",
                                                    color = scale_colors,
                                                    show_rownames = TRUE)
              EP_Heatmap

            })

## App ####
  ### App script
  ### App Settings
  options(shiny.maxRequestSize = 30 * 1024^2)
  dir_temp <- tempdir()
  ## App library dependencies

  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(WGCNA)) install.packages("WGCNA")
  if(!require(openxlsx)) install.packages("openxlsx")
  if(!require(pheatmap)) install.packages("pheatmap")
  if(!require(plotly)) install.packages("plotly")
  if(!require(shinythemes)) install.packages("shinythemes")
  if(!require(shinyWidgets)) install.packages("shinyWidgets")
  if(!require(gprofiler2)) install.packages("gprofiler2")
  # ## File dependencies:
  ## 1. Load Classes
  ## 2. Load Generics
  ## 3. Load Methods
  # source("./R/S4_classes.R")
  # source("./R/Functions.R")
  # source("./R/S4_generics.R")
  # source("./R/S4_methods.R")

  ### Shiny UI
  ui <- bootstrapPage(
    navbarPage(title = "MetaNetwork",
               theme = shinytheme("flatly"),
               collapsible = TRUE,
               # HTML('<a style = "text-decoration:none;cursor:default;color:FFFFFF;"
               #      class="active href="#">MetaNetwork </a>'),
               id = "nav",

               tabPanel("WGCNA Workflow",
                        sidebarPanel(
                          fileInput(inputId = "dataFile",
                                    label = "Upload data file (.csv file)"),
                          fileInput(inputId = "groupsFile",
                                    label = "Upload experimental groups file (.csv)"),
                          fileInput(inputId = "databaseFile",
                                    label = "Upload data table file containing gene names,
                                    UniProt accessions, and  protein names in .tsv format."),
                          textInput(inputId = "organism",
                                    label = "Input species in g:Profiler format:
                                    H. sapiens = hsapiens; M. musculus = mmusculus."),
                          tags$a(href = "https://biit.cs.ut.ee/gprofiler/page/organism-list",
                                 "Click here for full reference of g:Profiler compatible organisms.
                                 \n Abbreviations can be found in the 'id' column."),
                          #WGCNA Workflow Parameters
                          h2("WGCNA Parameters"),
                          h3("Network Parameters"),
                          textInput(inputId = "rcutoff",
                                    label = "Scale-free topology approximation threshold",
                                    value = 0.85),
                          textInput(inputId = "powerupper",
                                    label = "Max power for scale-free network testing",
                                    value = 20),
                          checkboxInput(inputId = "automaticPowerSelection",
                                        label = "Check for automatic power selection",
                                        value = TRUE),
                          h3("Module formation options"),
                          textInput(inputId = "mcutheight",
                                    label = "Module merging cut height",
                                    value = 0.25),
                          textInput(inputId = "minModuleSize",
                                    label = "Minimum module size",
                                    value = 20),
                          br(),
                          h3("Advanced Options"),
                          textInput(inputId = "overridePowerSelection",
                                    label = "User-entered power selection. \n
                                    Value is not used unless 'Check for automatic power selection' is unchecked.",
                                    value = 12),
                          checkboxInput(inputId = "networkType",
                                        label = "Signed Network",
                                        value = TRUE),
                          checkboxInput(inputId = "replaceMissingAdjacencies",
                                        label = "Replace Missing Adjacencies",
                                        value = TRUE),
                          sliderInput(inputId = "deepSplit",
                                      label = "Module detection sensitivity",
                                      min = 0,
                                      max = 4,
                                      value = 2),
                          textInput(inputId = "maxBlockSize",
                                    label = "Maximum block size",
                                    value = 300000),
                          textInput(inputId = "nPreclusteringCenters",
                                    label = "Number of preclustering centers"),
                          sliderInput(inputId = "verbosity",
                                      label = "Output verbosity",
                                      min = 0,
                                      max = 5,
                                      value = 5),
                          br(),
                          actionButton(inputId = "RunWGCNAWorkflow",
                                       label = "Submit job")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Data",
                                     DT::DTOutput("DataFilePreview")
                            ),
                            tabPanel("Experimental Groups",
                                     DT::DTOutput("ExpGroupsFilePreview")
                            )
                          ))
               ),
               tabPanel("WGCNA Diagnostics",
                        sidebarPanel(
                          pickerInput("plot_select",
                                      "Select plot:",
                                      choices = c("Protein Dendrogram",
                                                  "Module Correlation Dendrogram",
                                                  "Module Adjacency Heatmap",
                                                  "Module Correlation Heatmap",
                                                  "Module Eigenprotein Heatmap",
                                                  "Scale Free Topology Plots",
                                                  "TOM Plot",
                                                  "Sample Clustering"),
                                      width = 3)
                          # I could put notes here using textOutput, which would be cool
                        ),
                        mainPanel("Module Diagnostics Plot",
                                  plotOutput(outputId = "diagnostic_plot"),
                                  width = 9)
               ),
               tabPanel("Module Eigenprotein Analysis",
                        sidebarPanel(
                          pickerInput("plotType",
                                      choices = c("Boxplot",
                                                  "Violin Plot",
                                                  "Density Plot"),
                                      multiple = FALSE),
                          pickerInput("module",
                                      label = "Select module",
                                      choices = c("All"),
                                      multiple = FALSE
                          ),
                          width = 3
                        ),
                        mainPanel(
                          "Module Eigenprotein Analysis",
                          plotOutput(outputId = "MEPlot")
                        ), width = 9
               ),

               tabPanel("g:Profiler enrichment",
                        sidebarPanel(
                          pickerInput("selectModule",
                                      "Select Module to view enrichment plot",
                                      choices = c("All")),
                          width = 3),
                        mainPanel(
                          "Functional annotation enrichment analysis",
                          plotlyOutput(outputId = "gProfilerPlot")
                        ), width = 9),

               tabPanel("Data Analysis",
                        sidebarPanel(
                          pickerInput("module_data",
                                      label = "Select module to view data",
                                      choices = c("Data not loaded"))
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("g:Profiler Enrichment Results",
                                     DT::DTOutput("gProfilerResults")),
                            tabPanel("Module Membership Results",
                                     DT::DTOutput("MM_results")),
                            tabPanel("Prop. Variance Explained",
                                     DT::DTOutput("prop_var")),
                            tabPanel("Module eigenprotein values",
                                     DT::DTOutput("me_wide")))
                        )),
               tabPanel("Download",
                        sidebarPanel(
                          downloadButton(outputId = "downloader",
                                         label = "Download MetaNetwork Results")
                        )),
               tabPanel("About MetaNetwork")
    )
  )

  server <- function(input, output, session){
    output$DataFilePreview <- DT::renderDT({

      req(input$dataFile)
      read_csv(input$dataFile$datapath)

    })

    output$ExpGroupsFilePreview <- DT::renderDT({

      req(input$groupsFile)
      read_csv(input$groupsFile$datapath)

    })

    WGCNA_workflow_results <- eventReactive(input$RunWGCNAWorkflow, {
      ## Require the three files to run
      req(input$dataFile)
      req(input$groupsFile)
      req(input$databaseFile)
      req(input$organism)

      ## Create new variable for the file paths
      path_dataFile <- input$dataFile$datapath
      path_groupsFile <- input$groupsFile$datapath
      path_database <- input$databaseFile$datapath

      ## Coerce the user values to their correct classes in the call to WGCNA Parameters
      ## and assign the other parameters to values
      RCutoff <- as.numeric(input$rcutoff)
      PowerUpper <- as.numeric(input$powerupper)
      MCutHeight <- as.numeric(input$mcutheight)
      minModuleSize <- as.numeric(input$minModuleSize)
      maxBlockSize <- as.numeric(input$maxBlockSize)
      nPreclusteringCenters <- as.numeric(input$nPreclusteringCenters)

      automaticPowerSelection <- input$automaticPowerSelection
      overridePowerSelection <- as.numeric(input$overridePowerSelection)
      networkType <- ifelse(input$networkType == TRUE, "signed", "unsigned")
      replaceMissingAdjacencies <- input$replaceMissingAdjacencies
      deepSplit <- input$deepSplit
      maxBlockSize <- as.numeric(input$maxBlockSize)
      verbosity <- input$verbosity

      organismID <- input$organism

      ## System threading won't work on Mac and it doesn't properly work on Windows,
      ## so I needed to explicitly disable all multithreading used by the WGCNA package.
      WGCNA::disableWGCNAThreads()

      ## Create RawData and CleanData objects from path
      raw_data <-  ReadDataFromPath(RawData(path = path_dataFile))
      cleaned_data <- BuildCleanDataFromRawData(x = raw_data, y = new("CleanData"))
      ## Build experimental groups object
      exp_groups <- BuildExperimentalGroupsFromPath(new("ExperimentalGroups",
                                                        path = path_groupsFile))
      ## Build database object
      uniprot_db <- CreateUniprotDatabase(path = path_database)
      ## Run power determination
      ## Create Power Determination Object:
      powers <- c(c(1:10), seq(from = 12, to = PowerUpper, by = 2))
      power_parameters <- PowerDeterminationParameters(
        R_squared_cutoff = RCutoff,
        powerVector = powers,
        networkType = networkType,
        UserDefinedPower = ifelse(
          automaticPowerSelection == FALSE,
          TRUE,
          FALSE),
        OverridePower = overridePowerSelection,
        AutomaticPowerSelection =
          ifelse(automaticPowerSelection == TRUE,
                 TRUE,
                 FALSE)
      )
      sft_object <- PickSoftThreshold(power_parameters, cleaned_data)
      sft_object <- ScaleFreeTopologyPlot(sft_object)
      ## Create the WGCNAParameters Object
      wgcna_parameters <- WGCNAParameters(power = sft_object@powerEstimate,
                                          networkType = networkType,
                                          replaceMissingAdjacencies =
                                            replaceMissingAdjacencies,
                                          TOMtype = networkType,
                                          minModuleSize = minModuleSize,
                                          mergeCutHeight = MCutHeight,
                                          deepSplit = deepSplit,
                                          maxBlockSize = maxBlockSize,
                                          nPreclusteringCenters = nPreclusteringCenters,
                                          verbose = verbosity)
      ## Run WGCNA and save results in a WGCNA Results Object
      wgcna_results_object <- WGCNAResults(
        results = RunBlockwiseModules(wgcna_parameters, cleaned_data = cleaned_data),
        CleanData = cleaned_data, ExperimentalGroups = exp_groups)

      ## Create tidy module eigenproteins
      mep_object <- ModuleEigenproteinsTidy(wgcna_results_object,
                                            ExperimentalGroups = exp_groups)

      ## Build module membership object
      module_membership <- BuildModuleMembership(new("ModuleMembership"),
                                                 wgcna_results_object,
                                                 RawDataObject = raw_data,
                                                 UniprotDatabaseObject = uniprot_db)
      ## Initialize g:Profiler enrichment parameters and gostplot parameters
      gprofiler_parameters <- gProfilerParameters2(organism = organismID)
      gostplot_parameters <- GostPlotParameters()
      ## Run g:Profiler enrichment analysis
      gProfiler_results <- gost(gprofiler_parameters,
                                module_membership)
      ## Build PlotsOutput Object
      plots_output <- BuildAllPlots(METidy = mep_object,
                                    WGCNAResults = wgcna_results_object,
                                    CleanData = cleaned_data,
                                    WGCNAParameters = wgcna_parameters,
                                    SFTObject = sft_object,
                                    GostplotParameters = gostplot_parameters,
                                    gProfilerResults = gProfiler_results,
                                    comparisonsObject = NULL)
      data_output <- BuildDataOutput(METidy = mep_object,
                                     WGCNAResults = wgcna_results_object,
                                     gProfiler = gProfiler_results,
                                     CleanData = cleaned_data,
                                     MM_object = module_membership)

      ## Build DataOutput object
      ## Combine plots and data to make final output object
      CreateServerOutput(PlotsOutput = plots_output, DataOutput = data_output)



    })
    ## Update the inputs for the selection menus when WGCNA_workflow_results finishes
    ## This observer function:
    ## 1. Updates module name depedent picker inputs
    ## 2. Selects the plots to display in response to user input
    ## 3. Creates the temporary directory in preparation for call to downloadHandler.
    observe({
      req(WGCNA_workflow_results())
      ## Updates the picker input with the names of the modules
      module_names <- fetch_gProfiler_names(WGCNA_workflow_results())

      module_colors <- list()
      for(i in 1:length(module_names)){
        module_colors[[module_names[i]]] <- module_names[i]
      }

      meplot_names <- fetch_MEPlots_names(WGCNA_workflow_results())
      meplotlist <- list()
      for(i in 1:length(meplot_names)){
        meplotlist[[meplot_names[i]]] <- meplot_names[i]
      }


      updatePickerInput(session,
                        inputId = "module",
                        label = "Select module",
                        selected = "All",
                        choices = meplotlist)
      updatePickerInput(session,
                        inputId = "selectModule",
                        "Select Module to view enrichment plot",
                        choices = module_colors)
      updatePickerInput(session,
                        inputId = "module_data",
                        choices = module_colors)

      ## Renders ME plots Diagnostics Plots on input changes
      observe({
        req(WGCNA_workflow_results())
        output$MEPlot <- renderPlot(
          get_specific_MEPlot(WGCNA_workflow_results(),
                              slot = input$plotType,
                              plot_name = input$module)
        )
      })
      ## Renders gprofiler plots on input change
      observe({
        req(WGCNA_workflow_results())
        output$gProfilerPlot <- renderPlotly(
          get_specific_gostplot(WGCNA_workflow_results(),
                                plot_name = input$selectModule)
        )
      })
      ## Renders Diagnostic plots on input changes
      observe({
        req(WGCNA_workflow_results())
        output$diagnostic_plot <- renderPlot(
          get_specific_diagnostic_plot(WGCNA_workflow_results(),
                                       plot = input$plot_select)
        )
      })
      ## Renders data tables for displaying propvar, gprofiler, and module membership results
      output$prop_var <- DT::renderDT(
        prop_var_output(WGCNA_workflow_results()),
      )

      output$me_wide <- DT::renderDT(
        fetch_MEs_wide(WGCNA_workflow_results())
      )

      observe({
        req(WGCNA_workflow_results())
        output$gProfilerResults <- DT::renderDT(
          fetch_specific_gProfiler_result(WGCNA_workflow_results(),
                                          input$module_data)
        )
      })
      observe({
        req(WGCNA_workflow_results())
        output$MM_results <- DT::renderDT(
          fetch_specific_ModMemb_result(WGCNA_workflow_results(),
                                        input$module_data)
        )
      })
      ## Creates folders and names and save plots for download in a temporary
      ## folder in preparation for call to download handler
      observe({
        req(WGCNA_workflow_results())

        directories <- c("gProfiler_Plots",
                         "Module_Eigenprotein_Plots",
                         "Module_Eigenprotein_Plots/boxplots",
                         "Module_Eigenprotein_Plots/violin",
                         "Module_Eigenprotein_Plots/density",
                         "Eigenprotein_Diagnostics")

        dir.create(file.path(dir_temp, "Results"))
        for(i in 1:length(directories)){
          dir.create(file.path(dir_temp, "Results", directories[i]))
        }
        path_to_temp <- file.path(dir_temp, "Results")
        WriteResultsToTempFolder(WGCNA_workflow_results(), path_to_temp)
        zip::zipr(zipfile = file.path(dir_temp, "WGCNAResults.zip"), files = path_to_temp)
      })

    })

    output$downloader <- downloadHandler(
        filename = function(){file.path(paste("WGCNAResults",
                                    "zip",
                                    sep = "."))},
        content = function(file){
          file.copy(file.path(dir_temp, "WGCNAResults.zip"), file)
        },
        contentType = "application/zip"
      )
}
shinyApp(ui, server)
