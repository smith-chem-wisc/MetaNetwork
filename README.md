# MetaNetwork
A Weighted Gene Co-expression Network Analysis Rshiny Application

## Installing MetaNetwork
MetaNetwork requires the following packages to be loaded in an R session: 
WGCNA, heatmap3, tidyverse, readxl, openxlsx, qtl, corrplot, BiocManager, allez. We recommend using RStudio to run MetaNetwork due to its user-friendly folder navigation interface.

After downloading the repository from GitHub, open RStudio. Navigate to the File dropdown menu and choose "New Project". Select the option for "Existing Directory", select "Browse..." and navigate to the folder containing MetaNetwork. This will create a new R project and set the working directory in the same folder as the ui.R and server.R files. To launch MetaNetwork, copy and paste the following code into the console: 

install.packages("shiny")

library(shiny)

shinyAppDir(getwd())


## Step 1: WGCNA Analysis
### Uploading data file
Upload your data to MetaNetwork using the included template excel sheet. Currently, MetaNetwork requires protein identifiers to be Uniprot Accessions in the first column. The second column, for gene symbols, is not required; however, the amount of columns to ignore will require adjusting.  

### Uploading groups file
Using the included GroupsTemplate.xlsx file, enter the names of your samples in the experiment column. Make sure they match the names of your samples in the DataTemplate.xlsx file. In the SampleID column, list the corresponding experimental conditions corresponding to each of your samples. 

### Uploading Uniprot Database File
The MetaNetwork repository contains a reviewed, human uniprot database and reviewed, mouse uniprot database. After clicking the upload button, navigate to the UniprotDB folder to select the database for your sample.  

### User Input Values
The modifiable values in Step 1: WGCNA Analysis enable custom user control of the clustering, module merging, and minimum module size. They are automatically set to default values commonly used in WGCNA analysis. "Enter how many columns to ignore" is set automatically at two. If you only have a column of acessions with no corresponding gene, set this equal to 1. 

### Submit Job
After uploading your data, MetaNetwork will display a preview. Make sure everything looks correct, and then click the "Submit Job" button. 

### Output
After the completion of the WGCNA workflow, there will be a new folder entitled "Results" in your working directory. It will contain quality control and diagnostic figures, as well as the module memberships and module eigenproteins. The module memberships can be found in the ResultsWGCNA.xlsx file, while the module eigenproteins can be found in Eigenproteins_by_sample.csv. 

## Step 2: GO Enrichment Analysis
Gene Ontology Enrichment Analysis is performed by the allez package (insert citation). This step only requires uploading the ResultsWGCNA.xlsx file from Results folder and selecting the correct organism from the dropdown selection box. This version of MetaNetwork supports only mouse and human databases.

# For further reading

# Citations
