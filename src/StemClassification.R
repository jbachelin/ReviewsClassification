## Authors : Eric HAMMEL & Jordan BACHELIN
## Date : 06/01/2018
## Desc : Programme principal

rm(list = ls())

source("src/DataLoading.R") # Fonction fill_df
source("src/DataClassification.R") # All classification algorithms
source("src/PreProcessingReviews.R") # Fonction to prepare data to classification

library(XML)
library(dplyr)
library("caret") # https://topepo.github.io/caret/available-models.html
library("wordnet")
library("DMwR") # balance training dataset
library(klaR)
library(InformationValue)
library(kernlab)
library(nnet)
library(e1071)
library(pROC)

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
system.time(res.nb <- my_naive_bayes(model.nb, review.test))
res.nb

# Avec le jeu de donnees reequilibre
model.nb.reeq <- train(polarity ~ ., data = resamp1, method = "nb", trControl = tr.control)
system.time(res.nb.reeq <- my_naive_bayes(model.nb.reeq, review.test))
res.nb.reeq

# Regression logistique ####

# TRAIN MODEL
model.rl <- train(polarity ~ ., data = review.train, method = "glm", family = "binomial", trControl = tr.control)
system.time(res.rl <- my_rl(model.rl, review.test))
res.rl

# Avec le jeu de donnees reequilibre
model.rl.reeq <- train(polarity ~ ., data = resamp1, method = "glm", family = "binomial", trControl = tr.control)
system.time(res.rl.reeq <- my_rl(model.rl.reeq, review.test))
res.rl.reeq

# Réseaux de neurones ####

# TRAIN NEURAL NETWORK
ideal <- class.ind(review.dtm$polarity)
modelANN = nnet(review.train[, -which(names(review.train) == "polarity")], ideal[review.train.index,], size = 10, softmax = TRUE, MaxNWts = 15000)
system.time(res.nn <- my_nn(modelANN, review.test))
res.nn

# Avec le jeu de donnees reequilibre

resamp1.index <- as.matrix(1:nrow(resamp1), ncol = 1)
ideal <- class.ind(resamp1$polarity)
modelANN.reeq = nnet(resamp1[, -which(names(resamp1) == "polarity")], ideal[resamp1.index,], size = 10, softmax = TRUE, MaxNWts = 15000)
system.time(res.nn.reeq <- my_nn(modelANN.reeq, review.test))
res.nn.reeq

# SVM ####

# TRAIN MODEL
rbf <- rbfdot(sigma = 0.1)
model.svm <- ksvm(polarity ~ ., data = review.train, type = "C-bsvc", kernel = rbf, C = 10, prob.model = TRUE)
system.time(res.svm <- my_svm(model.svm, review.test))
res.svm

# Avec le jeu de donnees reequilibre
model.svm.reeq <- ksvm(polarity ~ ., data = resamp1, type = "C-bsvc", kernel = rbf, C = 10, prob.model = TRUE)
system.time(res.svm.reeq <- my_svm(model.svm.reeq, review.test))
res.svm.reeq

# Ecriture des résultats de la classification dans un fichier à part

res.df <- data.frame(review.test, predicted.class = res.svm.reeq$predicted)
write.csv(res.df, file = "data/FINAL_RESULTS_SVM_HAMMEL_BACHELIN.csv", row.names = FALSE)

# Calcul temps d'exécution moyen ####

library(microbenchmark)
library(ggplot2)

mb <- microbenchmark(
  res.nb <- my_naive_bayes(model.nb, review.test),
  res.nb.reeq <- my_naive_bayes(model.nb.reeq, review.test),
  res.rl <- my_rl(model.rl, review.test),
  res.rl.reeq <- my_rl(model.rl.reeq, review.test),
  res.nn <- my_nn(modelANN, review.test),
  res.nn.reeq <- my_nn(modelANN.reeq, review.test),
  res.svm <- my_svm(model.svm, review.test),
  res.svm.reeq <- my_svm(model.svm.reeq, review.test),
  times = 5L,
  unit = "s"
)

# autoplot(mb)
plot(mb)

# Création des courbes ROC et calcul de l'aire sous la courbe (AUC)

# courbe ROC
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels))
}

# ROC_res.nb <- simple_roc(labels = res.nb$predicted == "negative", scores = review.test$polarity)
# ROC_res.nb.reeq <- simple_roc(labels = res.nb.reeq$predicted == "negative", scores = review.test$polarity)
ROC_res.rl <- simple_roc(labels = res.rl$predicted == "negative", scores = review.test$polarity)
ROC_res.rl.reeq <- simple_roc(labels = res.rl.reeq$predicted == "negative", scores = review.test$polarity)
ROC_res.nn <- simple_roc(labels = res.nn$predicted == "negative", scores = review.test$polarity)
ROC_res.nn.reeq <- simple_roc(labels = res.nn.reeq$predicted == "negative", scores = review.test$polarity)
ROC_res.svm <- simple_roc(labels = res.svm$predicted == "negative", scores = review.test$polarity)
ROC_res.svm.reeq <- simple_roc(labels = res.svm.reeq$predicted == "negative", scores = review.test$polarity)

## Affichage cumulée des courbes ROC
plot(ROC_res.rl, axes = F, xlab = "Specificity (%)", ylab = "Sensitivity (%)", type = "l")
axis(2, at = seq(0,1,by = .1), labels = paste(seq(0,1, by = .1)), tick = TRUE)
axis(1, at = seq(0,1,by = .1), labels = paste(seq(1,0, by = -.1)), tick = TRUE)
lines(x = c(0,1), y = c(0,1))
lines(ROC_res.rl.reeq, col = "pink")
lines(ROC_res.nn, col = "yellow")
lines(ROC_res.nn.reeq, col = "red")
lines(ROC_res.svm, col = "green")
lines(ROC_res.svm.reeq, col = "blue")
legend(0.5, 0.25, legend=c("RL simple", "RL avec réequilibrage", "RN simple", "RN avec rééquilibrage", "SVM simple", "SVM avec rééquilibrage"),
       col=c("black", "pink", "yellow", "red", "green", "blue"), lty=1, cex=0.8)

# Aire sous la courbe (Area Under Curve)

# AUC_res.nb <- auc(as.numeric(res.nb$predicted),as.numeric(review.test$polarity)) # Ne marche pas car une seule classe prédite à chaque fois
# AUC_res.nb.reeq <- auc(as.numeric(res.nb.reeq$predicted),as.numeric(review.test$polarity)) # Idem
AUC_res.rl <- auc(as.numeric(as.factor(res.rl$predicted)),as.numeric(review.test$polarity)) # Area under the curve: 0.8785
AUC_res.rl.reeq <- auc(as.numeric(as.factor(res.rl.reeq$predicted)),as.numeric(review.test$polarity)) # Area under the curve: 0.3738
AUC_res.nn <- auc(as.numeric(as.factor(res.nn$predicted)),as.numeric(review.test$polarity)) # Area under the curve: 0.7153
AUC_res.nn.reeq <- auc(as.numeric(as.factor(res.nn.reeq$predicted)),as.numeric(review.test$polarity)) # Area under the curve: 0.7531
AUC_res.svm <- auc(as.numeric(as.factor(res.svm$predicted)),as.numeric(review.test$polarity)) # Area under the curve: 0.7935
AUC_res.svm.reeq <- auc(as.numeric(as.factor(res.svm.reeq$predicted)),as.numeric(review.test$polarity)) # Area under the curve: 0.8068
