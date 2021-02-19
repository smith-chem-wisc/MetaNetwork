README

# Explanation of Files: 

## EigenproteinsBySample
Each pdf file contains the module eigenproteins broken down by sample. 

## DendroColorMergedClust.pdf
Contains the protein dendrogram and module membership for each protein

## DendrogramEigenproteins
Contains the module clustering and a heatmap of the adjacency values between each module.

## Eigenprotein_adjacency heatmap.pdf
Shows the eigenprotein clustering and a heatmap of the adjacency between each eigenprotein. 

## Module EigenproteinMergeDendrogram.pdf 
Contains two dendrograms: The first is the module clustering pre-merging with the red line corresponding to the user specified cut height. The second dendrogram shows the module clustering following the merging algorithm. 

## SampleClustering.pdf
Contains a dendrogram with sample level clustering used to identify sample outliers. 

## ScaleFreeTopology.pdf
Shows the model fit (R^2) versus the power and the mean connectivity versus power. 

## Eigenproteins_by_sample.csv
Tidy output of eigenproteins. 

## EigenproteinsWide.csv
Human-readable eigenproteins for each sample. 

## varianceExplained.csv
Lists the variance explained by each module eigenprotein. 

## AllezEnrichment.xlsx
Output of the GO enrichment workflow. Contains GO terms ordered by highest to lowest z-score for each module. 

## ResultsWGCNA.xlsx
Output of WGCNA workflow. Contains the module memberships, protein intensity data, correlation coefficient for each protein with the module eigenproteins. 
First sheet has all information. Subsequent sheets contain individuals modules. 

## NetworkHeatmap.png
Heatmap of the dissimilarity TOM values between each protein. 