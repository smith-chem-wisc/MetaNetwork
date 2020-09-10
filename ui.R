# All the libraries that you will need ----
## Need to load all the R functions required to make this thing run
source(file = "./R/installFunctions.R")
installFunctions()
installAndLoadRequiredPackages()


# Options for this shiny session ----
options(shiny.maxRequestSize = 30*1024^2) # increases the upload size allowed to 30 mb.


# Define UI for application that draws a histogram

ui <- shinyUI(navbarPage("WGCNA and GO analysis",
                         #Page for step 1 ----
                         tabPanel("Step 1: WGCNA Analysis", 
                                  #Side bar controls for step 1    
                                  sidebarPanel(
                                    # File input parameters ----
                                    fileInput(inputId = "dataFile", label = "Data (.csv file) See note for required formatting."), 
                                    fileInput(inputId = "groupsFile", label = "Groups (.csv file) See note for required formatting."), 
                                    fileInput(inputId = "databaseFile", label = "Enter Database File Downloaded from Uniprot"),
                                    # WGCNA Workflow parameters ----
                                    textInput(inputId = "rcutoff" , 
                                              label = "R Cutoff", 
                                              value = 0.85), 
                                    textInput(inputId = "mcutheight", 
                                              label = "M Cut Height", 
                                              value = 0.25), 
                                    textInput(inputId = "powerupper", 
                                              label = "Power Upper", 
                                              value = 20), 
                                    textInput(inputId = "minmodsize", 
                                              label = "Min Module Size", 
                                              value = 20), 
                                    textInput(inputId = "ignorecols", 
                                              label = "Enter how many columns to ignore", 
                                              value = 2),
                                    #textInput(inputId = "workbookname", label = "Enter custom name for the excel workbook output", value = ""), Maybe someday.
                                    #checkboxInput(inputId = "ttestbool", label = "Check if data includes column of TTEST values", value = FALSE),
                                    #checkboxInput(inputId = "exportvisAnt", label = "Check if you want to export to VisANT. Not functional yet.", value = FALSE),
                                    #textInput(inputId = "filerename", label = "Enter analysis name", value = ""),
                                    # line break (to make it look nicer) ----
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
                                    fileInput(inputId = "WGCNAResults", label = "Import WGCNA results worksheet", ),
                                    selectInput(inputId = "organismID", label = "Select organism", choices = c("Human", "Mouse"), 
                                                selected = "Human"),
                                    br(), 
                                    actionButton(inputId = "action2", label = "Submit Job")
                                  ), 
                                  mainPanel()
                          )
                                  
  )
)


