# MetaNetwork
Network analysis and visualization of proteomics data using weighted gene correlation network analysis. This file details how to run MetaNetwork. [MetaNetwork's wiki](https://github.com/smith-chem-wisc/MetaNetwork/wiki) contains a vignette using data available in this GitHub repository and details how to use and interpret MetaNetwork's results.

## How to Run MetaNetwork
### System Requirements
MetaNetwork requires: 
  R 4.0.2 or later. 
  (optional) RStudio 1.3.1073
### Running MetaNetwork via RGui
#### Windows
If running in the RGui, MetaNetwork requires the installation of (Pandoc)[https://pandoc.org/]. For Windows computers, run the following code in RGui to install Pandoc: 

install.packages(c("installr", "rmarkdown"))

installr::install.pandoc() 

Pandoc will begin downloading and require a restart before MetaNetwork will function correctly. 
After the computer is restarted, open RGui again and run 

rmarkdown::find_pandoc()

This command should return the version and location of the Pandoc installation. MetaNetwork will be ready to run using the following commands: 

install.packages("shiny") 

shiny::runGitHub("MetaNetwork", "smith-chem-wisc")

[![image](https://user-images.githubusercontent.com/64652734/114082840-dd83de80-9873-11eb-86cf-1f1e6bdac27b.png)]

#### MacOS
On MacOS, we recommend using RStudio to run MetaNetwork if because the install.pandoc() command is not supported, complicating Pandoc installation. Fortunately, RStudio is packaged with its own Pandoc, so MetaNetwork will be fully functional on MacOS. 

### Running MetaNetwork via RStudio 
#### Windows and Mac
Type the following commands into the console of RStudio to run MetaNetwork on either Windows or Mac: 

install.packages("shiny") 

shiny::runGitHub("MetaNetwork", "smith-chem-wisc")

[![image](https://user-images.githubusercontent.com/64652734/114083000-12903100-9874-11eb-9df8-97c2febd74af.png)
]

### Running MetaNetwork via ShinyApps server
https://avc167.shinyapps.io/MetaNetwork_Shiny/ is the link to the web app version of MetaNetwork. Note that this version of MetaNetwork does not have access to the console log or any error messages produced, so it is impossible to troubleshoot. Future updates will add console logs and error messages. 

## Using MetaNetwork
When running MetaNetwork for the first time, it will take a few moments to load. The GUI should then be visible. 
[![image](https://user-images.githubusercontent.com/64652734/114083377-816d8a00-9874-11eb-8e1b-113829b1c5be.png)
]

While running MetaNetwork, the R console will contain all the messages and any potential errors occuring. Once MetaNetwork is finished running, it will automatically generate plots and data tables in the WGCNA Diagnostics, Module Eigenprotein Analysis, g:Profiler Enrichment, and Data Analysis tabs. The data will also be downloadable under the Download tab.  
### MetaNetwork Workflow
### Uploading data file
The data file should be in the format of UniProt Accessions, any optional identifiers (like gene symbol for example), then data, as shown in the example below. 
[![image](https://user-images.githubusercontent.com/64652734/114086819-bc71bc80-9878-11eb-86f6-1aa83e659d08.png)
]

In the example, the first column is Accessions, and the following columns are data. The heading is the name of the sample. The only column name that is mandatory is "Accession." 


### Uploading experimental groups file
The experimental groups file should contain one column of sample identifiers and one column of experimental groups. The column labeled SampleID must be matched to the column heading for the columns in the data file, as seen in the image below. 
[![image](https://user-images.githubusercontent.com/64652734/114086993-f478ff80-9878-11eb-9637-da03344a71f9.png)
]

The second columns, Experiment, lists the experimental condition to which the sample belongs. 

### Uploading UniProt Data Table File
Finally, the UniProt Data Table upload should be a .tsv or .tab file downloaded from UniProt containing an Entry column, Protein name column, and Gene name column. 
[![image](https://user-images.githubusercontent.com/64652734/114087471-8d0f7f80-9879-11eb-9428-9cd8075f111e.png)
]

### WGCNA Workflow
The modifiable values enable near-complete control over the WGCNA workflow in MetaNetwork. The following briefly lists the purpose of each option. 
#### Network Parameters
##### Scale-free topology approximation threshold 
The underlying assumption of WGCNA is that the data is transformable into a scale-free network. This option allows users to control the cutoff for determining whether or not the data submitted reaches the threshold for a scale-free network. 
##### Max power for scale-free network testing 
When "Check for automatic power selection is true," MetaNetwork calculates the lowest power that enables the scale-free topology model fit to exceed the "Scale-free topology approximation threshold value."
#### Module Formation Options
##### Module merging cut height
After modules are formed, they are clustered based on dissimilarity correlation (1 - correlation(module_i, module_j). This value determines the threshold (cut height) below which modules are merged. A value of 0.25 is aproximately equal to a 0.75 correlation coefficient. 
#### Advanced Options
##### User entered power selection. 
For users that do not want to use automatic power selection, they can input the power they would like to use instead here. 
##### Signed Network
Check for signed network, uncheck for unsigned network. 
##### Module Detection Sensitivity
Increasing this value will increase the number of modules detected. Decreasing it will decrease the number. 
##### Maximum Block Size and Number of preclustering centers
R does all calculations and stores all objects in the computer's RAM. If the calculations will exceed the computers RAM, the data will be pre-clustered using K-means, and the module memberships will be calculated on the k-means determined clusters. The inital number of clusters can be controlled using the Number of preclustering centers input. 
##### Output verbosity
Value indicating the amount of messaging MetaNetwork will produce in the console. 

### Submit Job
After uploading your data, MetaNetwork will display a preview in the main panel. Make sure everything looks correct, and then click the "Submit Job" button. We approximate that MetaNetwork will complete an analysis of 3000 proteins on a computer with 16 GB of RAM in under ten minutes. However, a slow internet connection will limit the speed for g:Profiler functional annotation enrichment, slowing the workflow.  

### MetaNetwork Output
#### WGCNA Diagnostics
#### Module Eigenprotein Analysis
#### g:Profiler Enrichment
#### Data Analysis
#### Download
