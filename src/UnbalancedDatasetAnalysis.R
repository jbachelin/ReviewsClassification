## Authors : Eric HAMMEL & Jordan BACHELIN
## Date : 30/12/2017
## Desc : Predicting Polarity in the un balanced data set of reviews

## importing data
source("src/DataLoading.R")
source("src/PreProcessingReviews.R")

## importaing library to perform the prediction tasks
library("caret")
library("wordnet")

ReviewsCleaned <- CleaningReviews(dataset = opinions$rewiew,
                           RemoveSw = FALSE, StemDoc = FALSE)
inspect(ReviewsCleaned[[1]])
# TagReviews <- treetag(ReviewsCleaned, lang = "en" ,TT.tknz = TRUE)
toto <- lapply(ReviewsCleaned[[1]], function(x) {
  x.filter <- getTermFilter("ExactMatchFilter", x, TRUE)
  terms <- getIndexTerms("NOUN", 1, x.filter)
  sapply(terms, getLemma)
})
