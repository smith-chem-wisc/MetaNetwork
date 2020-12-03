# All the libraries that you will need ----
## Need to load all the R functions required to make this thing run
source(file = "./R/installFunctions.R")
installFunctions()
installAndLoadRequiredPackages()
AnimalsAndDatabases <- tibble::tibble(readRDS(file = "./AppData/AnimalsAndDatabases.rds"))
# Options for this shiny session ----
options(shiny.maxRequestSize = 50*1024^2) # increases the upload size allowed to 50 mb.


# Define UI for application that draws a histogram

ui <- shinyUI(navbarPage("WGCNA and GO analysis",
                         #Page for step 1 ----
                         tabPanel("Step 1: WGCNA Analysis",
                                  #Side bar controls for step 1
                                  sidebarPanel(
                                    # File input parameters ----
                                    h2("File input"),
                                    fileInput(inputId = "dataFile", label = "Data (.csv file) See note for required formatting."),
                                    fileInput(inputId = "groupsFile", label = "Groups (.csv file) See note for required formatting."),
                                    fileInput(inputId = "databaseFile", label = "Enter Database File Downloaded from Uniprot"),
                                    # WGCNA Workflow parameters ----
                                    h2("WGNCA parameters"),
                                    textInput(inputId = "rcutoff" ,
                                              label = "Scale-free cutoff",
                                              value = 0.85),
                                    textInput(inputId = "mcutheight",
                                              label = "Module Merge Cut Height",
                                              value = 0.25),
                                    textInput(inputId = "powerupper",
                                              label = "Upper Power",
                                              value = 20),
                                    textInput(inputId = "minmodsize",
                                              label = "Minimum Module Size",
                                              value = 20),
                                    textInput(inputId = "ignorecols",
                                              label = "Non-data columns",
                                              value = 1),
                                    checkboxInput(inputId = "automaticPowerSelection", 
                                                  value = FALSE, label = "Check for automatic power selection"),

                                    # line break (to make it look nicer) ----
                                    br(),
                                    h3("Advanced options"),
                                    textInput(inputId = "threads",
                                              label = "Enter how many threads to use", value = ""),
                                    textInput(inputId = "overridePowerSelection", 
                                              label = "Enter custom power"),
                                    br(), 
                                    # Submit WGCNA Job ----
                                    actionButton(inputId = "action", label = "Submit Job")
                                  ),
                                  # Main panel ----
                                  mainPanel(
                                    ## Need to output the table again. It got accidentally deleted
                                    tableOutput("preview"),
                                    textOutput(outputId = "workflowOutput")
                                  )

                         ),
                         tabPanel("Step 2: GO Enrichment Analysis",
                                  sidebarPanel(
                                    fileInput(inputId = "WGCNAResults", label = "Import WGCNA results worksheet" ),
                                    selectInput(inputId = "organismID", 
                                                label = "Select organism", 
                                                choices = AnimalsAndDatabases[,1]),
                                    br(),
                                    actionButton(inputId = "action2", label = "Submit Job")
                                  ),
                                  mainPanel()
                          )

  )
)
