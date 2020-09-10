## Test data
require(org.Hs.eg.db)
require(org.Mm.eg.db)
require(tidyverse)
require(biomaRt)
require(testthat)
testUniverse <- read_csv(file.path("tests", "TestDataSet", "Unsupervised_data.csv"))
LightCyanTestAccessions <- read_csv("./tests/TestDataSet/LightCyanTestAccessions.txt", 
                                    col_names = FALSE)
mart <- fetchMart("Human")
testGeneSymbols <- convertAccessions(LightCyanTestAccessions, mart = mart)
testGeneSymbolsUniverse <- convertAccessions(testUniverse[1], mart = mart)
testAllezEnrichment <- enrichAllez(GeneSymbols = testGeneSymbols[[1]], 
            GeneUniverse = testGeneSymbolsUniverse[[1]])

head(testAllezEnrichment$setscores)



expect_equal(length(testAllezEnrichment$setscores$Term), 5224)
expect_equal(base::colnames(testAllezEnrichment$setscores)[1], "GO_Term")
