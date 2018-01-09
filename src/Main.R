## Authors : Eric HAMMEL & Jordan BACHELIN
## Date : 06/01/2018
## Desc : Programme principal

rm(list = ls())

source("src/DataLoading.R")
source("src/DataClassification.R")
source("src/PreProcessingReviews.R")

library(XML)
library(dplyr)
library("caret") # https://topepo.github.io/caret/available-models.html
library("wordnet")
library("DMwR") # balance training dataset
library(klaR)
library(InformationValue)
library(kernlab)
library(nnet)

# Chargement des données ####

data <- xmlParse("data/train_2017.xml")
xml_data <- xmlToList(data)

# Parsage et nettoyage des données ####

opinions <- data.frame(review = character(0),
                       target = character(0),
                       category = character(0),
                       polarity = character(0),
                       from = numeric(),
                       to = numeric(),
                       index = numeric(),
                       stringsAsFactors = FALSE)

opinions <- filldf(xml_data, opinions)
opinions <- opinions[, -2] # On retire la variable "target" comme indique dans le sujet
opinions <- opinions[-which(opinions$polarity == "neutral" | is.na(opinions$polarity)),] # On ne prend pas en consideration les polarityarites neutres ou manquantes

# Préparation des données avant classification ####

exceptions <- grep(pattern = "not|n't", x = stopwords("en"), value = TRUE)
ReviewsCleaned <- CleaningReviews(dataset = opinions$review,
                                  RemoveSw = TRUE, ExceptionVector = exceptions, StemDoc = TRUE)
review.dtm <- t(as.matrix(TermDocumentMatrix(ReviewsCleaned,
                                             control = list(weighing = weightTfIdf))))
polarity <- as.matrix(opinions$polarity[which(duplicated(opinions$review) == FALSE)])
review.dtm <- data.frame(review.dtm, polarity)

# Séparation des données en deux (jeux d'entrainement et de test) ####

set.seed(1234)
review.train.index <- createDataPartition(review.dtm$polarity,
                                          times = 1, p = 0.8, list = FALSE)
review.train <- review.dtm[review.train.index,]
dim(review.train) ## 434 1264

review.test <- review.dtm[-review.train.index,]
dim(review.test) ## 108 1264

# Rééquilibrage du jeu de données initial ####

resamp1 <- DMwR::SMOTE(polarity ~ ., data = review.train, K = 5)

# Mise en place d'une Cross-Validation
tr.control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
#                           ,classProbs = TRUE)

# Tests de différents algorithmes de classification ####

# Naive Bayes ####

# TRAIN NAIVE BAYES MODEL
model.nb <- train(polarity ~ ., data = review.train, method = "nb", trControl = tr.control)
system.time(res1 <- my_naive_bayes(model.nb, review.test)) # 14.57s
res1

# Avec le jeu de donnees reequilibre
model.nb.reeq <- train(polarity ~ ., data = resamp1, method = "nb", trControl = tr.control)
system.time(res2 <- my_naive_bayes(model.nb.reeq, review.test)) # 16.72s
res2

# Regression logistique ####

# TRAIN MODEL
model.rl <- train(polarity ~ ., data = review.train, method = "glm", family = "binomial", trControl = tr.control)
system.time(res3 <- my_rl(model.rl, review.test)) # 40.80s
res3

# Avec le jeu de donnees reequilibre
model.rl.reeq <- train(polarity ~ ., data = resamp1, method = "glm", family = "binomial", trControl = tr.control)
system.time(res4 <- my_rl(model.rl.reeq, review.test)) # 91.91s
res4

# Réseaux de neurones ####

# TRAIN NEURAL NETWORK
ideal <- class.ind(review.dtm$polarity)
modelANN = nnet(review.train[, -which(names(review.train) == "polarity")], ideal[review.train.index,], size = 10, softmax = TRUE, MaxNWts = 15000)
system.time(res5 <- my_nn(modelANN, review.test)) # 0.06s
res5

# Avec le jeu de donnees reequilibre

resamp1.index <- as.matrix(1:nrow(resamp1), ncol = 1)
ideal <- class.ind(resamp1$polarity)
modelANN.reeq = nnet(resamp1[, -which(names(resamp1) == "polarity")], ideal[resamp1.index,], size = 10, softmax = TRUE, MaxNWts = 15000)
system.time(res6 <- my_nn(modelANN.reeq, review.test)) # 0.09s
res6

# SVM ####

# TRAIN MODEL
rbf <- rbfdot(sigma = 0.1)
model.svm <- ksvm(polarity ~ ., data = review.train, type = "C-bsvc", kernel = rbf, C = 10, prob.model = TRUE)
system.time(res7 <- my_svm(model.svm, review.test)) # 0.72s
res7

# Avec le jeu de donnees reeq, -which(names(uilibre
model.svm.reeq <- ksvm(polarity ~ ., data = resamp1, type = "C-bsvc", kernel = rbf, C = 10, prob.model = TRUE)
system.time(res8 <- my_svm(model.svm.reeq, review.test)) # 0.33s
res8

# Calcul temps d'exécution ####

library(microbenchmark)
library(ggplot2)

mb <- microbenchmark(
  res1 <- my_naive_bayes(model.nb, review.test),
  res2 <- my_naive_bayes(model.nb.reeq, review.test),
  res3 <- my_rl(model.rl, review.test),
  res4 <- my_rl(model.rl.reeq, review.test),
  res5 <- my_nn(modelANN, review.test),
  res6 <- my_nn(modelANN.reeq, review.test),
  res7 <- my_svm(model.svm, review.test),
  res8 <- my_svm(model.svm.reeq, review.test),
  times = 5L,
  unit = "s"
)

# autoplot(mb)
plot(mb)
