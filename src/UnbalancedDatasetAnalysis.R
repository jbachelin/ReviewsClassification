## Authors : Eric HAMMEL & Jordan BACHELIN
## Date : 30/12/2017
## Desc : Predicting Polarity in the un balanced data set of reviews

rm(list = ls())

## importing data
source("src/DataLoading.R")
source("src/PreProcessingReviews.R")

## importaing library to perform the prediction tasks
library("caret")
library("DMwR") # balance training dataset
library("wordnet")

ReviewsCleaned <- CleaningReviews(dataset = opinions$rewiew,
                           RemoveSw = TRUE, StemDoc = TRUE)

review.dtm <- t(as.matrix(TermDocumentMatrix(ReviewsCleaned)))
class(review.dtm[1,1])
dim(review.dtm) # 542 1045
pol <- as.matrix(opinions$polarity[which(duplicated(opinions$rewiew) == FALSE)])
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

pol.nb <- table(review.dtm$pol)
pol.prop <- prop.table(pol.nb)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(pol.nb, main = "nombre d'occurence des avis positifs et négatifs")
barplot(pol.prop, main = "proportions des avis positif et négatifs")
layout(matrix(1, nrow = 1, ncol = 1))

resamp1 <- SMOTE(pol ~ ., data = review.dtm, K = 5)
dim(resamp1)
pol.nb <- table(resamp1$pol)
pol.prop <- prop.table(pol.nb)
layout(matrix(1:2, nrow = 1, ncol = 2))
barplot(pol.nb, main = "nombre d'occurence des avis positifs et négatifs")
barplot(pol.prop, main = "proportions des avis positif et négatifs")
layout(matrix(1, nrow = 1, ncol = 1))

#### Lemmatisation, à voir plus tard ####
# x.filter <- getTermFilter("ExactMatchFilter", ReviewsCleaned[[1]], TRUE)
# terms <- getIndexTerms("NOUN", 1, x.filter)
# sapply(terms, getLemma)

# TagReviews <- treetag(ReviewsCleaned, lang = "en" ,TT.tknz = TRUE)
# toto <- lapply(ReviewsCleaned$content, function(x) {
#   x.filter <- getTermFilter("ExactMatchFilter", x, TRUE)
#   terms <- getIndexTerms("NOUN", 1, x.filter)
#   sapply(terms, getLemma)
# })
