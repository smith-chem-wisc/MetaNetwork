require(tidyverse)
## testConversions is explicily tested while fetchMart is implicitly tested
testAccessions <- read_csv("./tests/TestDataSet/LightCyanTestAccessions.txt", 
                           col_names = FALSE)
mart <- fetchMart("Human")
testConversions <-  convertAccessions(testAccessions, mart = mart)

expect_equal(length(testConversions[[1]]), 34)
