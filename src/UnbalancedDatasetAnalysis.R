## Authors : Eric HAMMEL & Jordan BACHELIN
## Date : 30/12/2017
## Desc : Predicting Polarity in the un balanced data set of reviews

# rm(list = ls())

## importing data
source("src/DataLoading.R")
source("src/PreProcessingReviews.R")

## importaing library to perform the prediction tasks
library("caret") # https://topepo.github.io/caret/available-models.html
library("wordnet")

ReviewsCleaned <- CleaningReviews(dataset = opinions$review,
                           RemoveSw = TRUE, StemDoc = TRUE)
review.dtm <- t(as.matrix(TermDocumentMatrix(ReviewsCleaned,
                                 control = list(weighing = weightTfIdf))))
pol <- as.matrix(opinions$polarity[which(duplicated(opinions$review) == FALSE)])
review.dtm <- data.frame(review.dtm, pol)

## c'est un problème d'apprentissage sur des classes déséquilibrées
pol.nb <- table(pol)
pol.prop <- prop.table(pol.nb)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(pol.nb, main = "nombre d'occurence des avis positifs et négatifs")
barplot(pol.prop, main = "proportions des avis positif et négatifs")
layout(matrix(1, nrow = 1, ncol = 1))

## utilisation d'un algorithme de rééquilibrage des données
# définition d'un jeu de test  
review.train.index <- createDataPartition(review.dtm$pol,
                                          times = 1, p = 0.8, list = FALSE)
review.train <- review.dtm[review.train.index,]
dim(review.train) ## 434 1046

review.test <- review.dtm[-review.train.index,]
dim(review.test) ## 108 1046

pol.nb <- table(review.train$pol)
pol.prop <- prop.table(pol.nb)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(pol.nb, main = "nombre d'occurence des avis positifs et négatifs")
barplot(pol.prop, main = "proportions des avis positif et négatifs")
layout(matrix(1, nrow = 1, ncol = 1))

## SMOTE
library("DMwR") # balance training dataset
resamp1 <- SMOTE(pol ~ ., data = review.train, K = 5)
pol.nb <- table(resamp1$pol)
pol.prop <- prop.table(pol.nb)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(pol.nb, main = "nombre d'occurence des avis positifs et négatifs")
barplot(pol.prop, main = "proportions des avis positif et négatifs")
layout(matrix(1, nrow = 1, ncol = 1))

# Split new dataset

review.train.reeq.index <- createDataPartition(resamp1$pol,
                                               times = 1, p = 0.8, list = FALSE)
review.train.reeq <- resamp1[review.train.reeq.index,]
dim(review.train.reeq) ## 611 1264

review.test.reeq <- resamp1[-review.train.reeq.index,]
dim(review.test.reeq) ## 152 1264

## ADASYN
library("smotefamily")
review.train2 <- review.train
review.train2$pol <- as.numeric(as.factor(review.train$pol))
resamp2 <- ADAS(X = review.train[,-1046],
     target = review.train[,1046],
     K = 5)
pol.nb <- table(resamp2$data$class)
pol.prop <- prop.table(pol.nb)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(pol.nb, main = "nombre d'occurence des avis positifs et négatifs")
barplot(pol.prop, main = "proportions des avis positif et négatifs")
layout(matrix(1, nrow = 1, ncol = 1))
adasyned.reviews <- resamp2$data


#### Lemmatisation, à voir plus tard ####
library("wordnet")
library("koRpus") # Part of speech tagging pour la lemmatisation

ReviewsCleaned <- CleaningReviews(dataset = opinions$review,
                                  RemoveSw = FALSE, StemDoc = FALSE)
ReviewsCleaned[[1]]$content
split.review <- unlist(strsplit(x = ReviewsCleaned[[1]]$content,
                                split = " "))
##koRpus approach with treetag
tag.reviews <- treetag(ReviewsCleaned[[1]]$content, treetagger = "manual", lang = "en",
                       TT.options = list(path = "~/Treetagger",
                                         preset = "en"),
                       format = "obj", stemmer = tm::stemDocument,
                       stopwords = tm::stopwords("en"))
# pense à faire une liste de stopwords customisés ou regarder les dico données par Erwan
tag.reviews@TT.res$lemma[-which(tag.reviews@TT.res$stop == TRUE)]


## wordnet approach
# test <- lapply(split.review, function(x){
#   x.filter <- getTermFilter("ExactMatchFilter", x, TRUE)
#   terms <- getIndexTerms("NOUN", 1, x.filter)
#   sapply(terms, getLemma)
# })
