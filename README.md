# MetaNetwork
Network analysis and visualization of proteomics data using weighted gene correlation network analysis. This file details how to run MetaNetwork. [MetaNetwork's wiki](https://github.com/smith-chem-wisc/MetaNetwork/wiki) contains a vignette using data available in this GitHub repository and details how to use and interpret MetaNetwork's results.

## How to Run MetaNetwork

### Installing and running MetaNetwork via RStudio (Windows and MacOS)

1. Install [RStudio](https://www.rstudio.com)
2. Type the following commands into the console of RStudio to run MetaNetwork on either Windows or MacOS: 

```
install.packages("shiny") 
shiny::runGitHub("MetaNetwork", "smith-chem-wisc")
```

![image](https://user-images.githubusercontent.com/64652734/114083000-12903100-9874-11eb-9df8-97c2febd74af.png)

### Installing and running MetaNetwork via RGui

1. Please see instructions below for setting up Pandoc in the system environment. 
2. Then, MetaNetwork will be ready to run using the following commands:

```
install.packages("shiny") 
shiny::runGitHub("MetaNetwork", "smith-chem-wisc")
```

![image](https://user-images.githubusercontent.com/64652734/114082840-dd83de80-9873-11eb-86cf-1f1e6bdac27b.png)

### Running MetaNetwork via ShinyApps server
https://avc167.shinyapps.io/MetaNetwork_Shiny/ is the link to the web app version of MetaNetwork. Note that this version of MetaNetwork does not have access to the console log or any error messages produced, so it is impossible to troubleshoot. Future updates will add console logs and error messages. 

## System Requirements
MetaNetwork requires: 
* R 4.0.2 or later
* RStudio 1.3.1073 (optional but recommended)

### MacOS
On MacOS, we recommend using RStudio to run MetaNetwork because the install.pandoc() command is not supported, complicating Pandoc installation. Fortunately, RStudio is packaged with its own Pandoc, so MetaNetwork will be fully functional on MacOS.

### Windows
If running in the RGui, MetaNetwork requires the installation of (Pandoc)[https://pandoc.org/] and (Rtools)[https://cran.r-project.org/bin/windows/Rtools/]. For Windows computers, run the following code in RGui to install Pandoc: 

```
install.packages(c("installr", "rmarkdown"))
installr::install.pandoc()
```

Pandoc will begin downloading and require a restart before MetaNetwork will function correctly. 
After the computer is restarted, open RGui again and run. 
```
rmarkdown::find_pandoc()
```
This command should return the version and location of the Pandoc installation. 

For Anaconda users: Due to a bug in RStudio, installations of RStudio from conda will not be able to find the Pandoc folder. As a result, we recommend downloading RStudio directly from the RStudio website. 



## Using MetaNetwork
When running MetaNetwork for the first time, it will take a few moments to download and install the necessary R packages. The GUI will then be visible. 
![image](https://user-images.githubusercontent.com/64652734/114083377-816d8a00-9874-11eb-8e1b-113829b1c5be.png)

While running MetaNetwork, the R console will contain all the messages and any potential errors occuring. Once MetaNetwork is finished running, it will automatically generate plots and data tables in the WGCNA Diagnostics, Module Eigenprotein Analysis, g:Profiler Enrichment, and Data Analysis tabs. The data will also be downloadable under the Download tab.  

### MetaNetwork Workflow
### Uploading data file
The data file should be in the format of UniProt Accessions, any optional identifiers (like gene symbol for example), then data, as shown in the example below. 
![image](https://user-images.githubusercontent.com/64652734/114086819-bc71bc80-9878-11eb-86f6-1aa83e659d08.png)

In the example, the first column is Accessions, and the following columns are data. The heading is the name of the sample. The only column name that is mandatory is "Accession." 


### Uploading experimental groups file
The experimental groups file should contain one column of sample identifiers and one column of experimental groups. The column labeled SampleID must be matched to the column heading for the columns in the data file, as seen in the image below. 
![image](https://user-images.githubusercontent.com/64652734/114086993-f478ff80-9878-11eb-9637-da03344a71f9.png)

The second columns, Experiment, lists the experimental condition to which the sample belongs. 

### Uploading UniProt Data Table File
Finally, the UniProt Data Table upload should be a .tsv or .tab file downloaded from UniProt containing an Entry column, Protein name column, and Gene name column. 
![image](https://user-images.githubusercontent.com/64652734/114087471-8d0f7f80-9879-11eb-9428-9cd8075f111e.png)
