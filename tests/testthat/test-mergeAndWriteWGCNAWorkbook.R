## test-mergeAndWriteWGCNAWorkbook.R 
source("./R/mergeAndWriteWGCNAWorkbook.R")

testDatabase <- read_tsv(file = file.path("TestDataSet", "filtered_Hsapiens.tab"))
set.seed(9876)
testData <- rnorm(n = 3000, mean = 16, sd = 3.2)
set.seed(9876)
testUniprotAccessions <- base::sample(testDatabase$Entry, 
                                      size = length(testData), 
                                      replace = FALSE)
testAllDataTibble  <- tibble(testUniprotAccessions, testData)
colnames(testAllDataTibble) <- c("accession", "expression")

testModulesMembership <- list(testAllDataTibble[1:750,], 
                              testAllDataTibble[751:1500,], 
                              testAllDataTibble[1501:2250,], 
                              testAllDataTibble[2251:3000,]) 
names(testModulesMembership) <- c("blue", "red", "green", "purple")

tempFilePath <- paste(tempdir(), "\\", "ResultsWGCNA.xlsx", sep = "")
dir.create(tempFilePath)
mergeAndWriteWGCNAWorkbook(selectedDatabase = testDatabase, 
                           allData = testAllDataTibble, 
                           dataList = testModulesMembership, 
                           filePath = tempFilePath)

testResultsWGCNA <- read_excel_allsheets(paste(tempFilePath,"\\","ResultsWGCNA.xlsx", sep = ""))

test_that("all accessions written", {
  expect_equal(length(testResultsWGCNA[[1]][,1]), length(testAllDataTibble[[1]]))
  expect_equal(testUniprotAccessions, testResultsWGCNA[[1]][,1])
  expect_equal(length(testResultsWGCNA[[2]][,1]),750)
  expect_equal(length(testResultsWGCNA[[1]][1,]), 4)
})
unlink(tempFilePath)

