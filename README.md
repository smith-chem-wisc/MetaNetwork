# MetaNetwork
Network analysis and visualization of proteomics data using weighted gene correlation network analysis. This file details how to run MetaNetwork. [MetaNetwork's wiki](https://github.com/smith-chem-wisc/MetaNetwork/wiki) contains an overview of the options available for MetaNetwork's WGCNA workflow and a vignette that details how to use and interpret MetaNetwork's results.

## How to Run MetaNetwork

## System Requirements
MetaNetwork requires: 
* Docker: https://www.docker.com/get-started

## R Package Dependencies
MetaNetwork's R package dependencies are handled by renv, a version manager R package ensuring uniform package versioning when MetaNetwork is deployed. For a complete list of dependencies, please review the renv.lock file in this repository, found at this link: https://github.com/smith-chem-wisc/MetaNetwork/blob/master/renv.lock.

### Run from MetaNetwork via Docker

1. Download [Docker](https://docs.docker.com/get-docker/) compatible with your operating system. 
2. Open Terminal (Mac) or Powershell (Windows).
3. Run the following code to download and launch MetaNetwork: 

```
docker pull avc167/metanetwork:latest

docker run -p 3838:3838 avc167/metanetwork:latest
```

Open a web browser and type localhost:3838 (Windows) or 0.0.0.0:3838 into the search bar. MetaNetwork's GUI will now be visible. 


#### IMPORTANT NOTE FOR MAC USERS
Docker for MacOS requires users to allocate memory for its processes. If not enough memory is allocated to MetaNetwork, it will crash without warning or error message. To increase the amount of memory allocated to MetaNetwork, you must open the Docker Desktop application and navigate to settings (circled in red in the below image): 
![image](https://user-images.githubusercontent.com/64652734/133110120-874da43b-2db9-44e0-82c5-422b8f01a17e.png)

Then click Resources, and use the slider to increase the amount of memory and/or CPUs allocated to MetaNetwork's processes. Typically 10 GB of memory enables an analysis of ~8000 proteins without memory issues. 
![image](https://user-images.githubusercontent.com/64652734/133110586-7799b2ec-ee56-49ef-bc46-f13d370e98ef.png) 

## Using MetaNetwork
When running MetaNetwork for the first time in a container, it will take a few moments to load the necessary R packages. After a short period of time, the GUI will be visible. 
![image](https://user-images.githubusercontent.com/64652734/114083377-816d8a00-9874-11eb-8e1b-113829b1c5be.png)

While running MetaNetwork, the R console, viewable either from the terminal or the Docker Desktop app, will contain all the messages and/or errors. Once MetaNetwork is finished running, it will automatically generate plots and data tables in the WGCNA Diagnostics, Module Eigenprotein Analysis, g:Profiler Enrichment, and Data Analysis tabs. The data will also be downloadable under the Download tab.  

### MetaNetwork Workflow
### Uploading data file
The data file should be in the format of UniProt Accessions, any optional identifiers (like gene symbol for example), then data, as shown in the example below. 
![image](https://user-images.githubusercontent.com/64652734/114086819-bc71bc80-9878-11eb-86f6-1aa83e659d08.png)

In the example, the first column is Accessions, and the following columns are data. The heading is the name of the sample. The first column must have a heading of "Accession." 


### Uploading experimental groups file
The experimental groups file should contain one column of sample identifiers and one column of experimental groups. These columns must be labeled as SampleID and Experiment. The column labeled SampleID must be matched to the column heading for the columns in the data file, as seen in the image below. 
![image](https://user-images.githubusercontent.com/64652734/114086993-f478ff80-9878-11eb-9637-da03344a71f9.png)

The second columns, Experiment, lists the experimental condition to which the sample belongs. 

### Uploading UniProt Data Table File
Finally, the UniProt Data Table upload should be a .tsv or .tab file downloaded from UniProt containing an Entry column, Protein name column, and Gene name column. 
![image](https://user-images.githubusercontent.com/64652734/114087471-8d0f7f80-9879-11eb-9428-9cd8075f111e.png)

### Encountering issues
Please submit any issues, including the full R terminal log from the terminal or Docker desktop, to the Issues page: https://github.com/smith-chem-wisc/MetaNetwork/issues. 
