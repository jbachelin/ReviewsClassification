## Authors : Eric HAMMEL & Jordan BACHELIN
## Date : 30/12/2017
## Desc : Predicting Polarity in the un balanced data set of reviews

rm(list = ls())

## importing data
source("src/DataLoading.R")
source("src/PreProcessingReviews.R")

library("hunspell")
string <- c("cirspy crust margherita pizza")
string2 <- c("great indian food")
split.str <- unlist(strsplit(string, split = " "))
toto <- SpellChecker(ToCheck = split.str)

#### Lematize Corpus ####
ReviewsCleaned <- CleaningReviews(dataset = opinions$review,
                                  RemoveSw = FALSE, StemDoc = FALSE)
test <- lapply(X = ReviewsCleaned, function(x){ x })
lemmed <- LemmatizeReviews(CorpusVector = test, RmStopwords = TRUE)
lemmed2 <- Corpus(VectorSource(lemmed))
lemmed2 <- tm_map(lemmed2, stripWhitespace) # Suppression des espaces en trop

lemmed.dtm <- t(as.matrix(TermDocumentMatrix(lemmed2,
                                           control = list(weighing = weightTf))))


string <- c("cirspy crust margherita pizza")
tag.reviews <- treetag(string, treetagger = "manual", lang = "en",
                       TT.options = list(path = "~/Treetagger",
                                         preset = "en"),
                       format = "obj")

string2 <- c("great indian food")
tag.reviews <- treetag(string2, treetagger = "manual", lang = "en",
                       TT.options = list(path = "~/Treetagger",
                                         preset = "en"),
                       format = "obj")
tag.reviews@TT.res$lemma

toto <- list(string, string2)
rmsw <- TRUE
lapply(X = toto, FUN = function(x){
  tag.reviews <- treetag(x, treetagger = "manual", lang = "en",
                         TT.options = list(path = "~/Treetagger",
                                           preset = "en"),
                         format = "obj", stemmer = stemDocument,
                         stopwords = stopwords("en"))
  
  if (rmsw == TRUE) {
    swInd <- which(tag.reviews@TT.res$stop == TRUE)
    if (identical(swInd, integer(0)) == TRUE)
    {
      tag.reviews@TT.res$lemma[which(tag.reviews@TT.res$lemma == "<unknown>")] <- tag.reviews@TT.res$token[which(tag.reviews@TT.res$lemma == "<unknown>")]
      tag.reviews@TT.res$lemma
    } else {
      tag.reviews@TT.res$lemma[which(tag.reviews@TT.res$lemma == "<unknown>")] <- tag.reviews@TT.res$token[which(tag.reviews@TT.res$lemma == "<unknown>")]
      tag.reviews@TT.res$lemma[-swInd]
    }
    
  }else{
    tag.reviews@TT.res$lemma[which(tag.reviews@TT.res$lemma == "<unknown>")] <- tag.reviews@TT.res$token[which(tag.reviews@TT.res$lemma == "<unknown>")]
      tag.reviews@TT.res$lemma
  }
  

  })

##### Ajouter Négation
##### https://stackoverflow.com/questions/21811580/negation-handling-in-r-how-can-i-replace-a-word-following-a-negation-in-r
