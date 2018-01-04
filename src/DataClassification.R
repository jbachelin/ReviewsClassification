# Projet Text Mining ####
# Auteurs : Jordan BACHELIN & Eric HAMMEL ####

# Test de plusieurs algorithmes de classification sur le jeu de donnees d'entrainement
# https://www.datacamp.com/courses/supervised-learning-in-r-classification

source("src/DataLoading.R")
source("src/PreProcessingReviews.R")
source("src/UnbalancedDatasetAnalysis.R")
library("NLP")
library(SnowballC)

# Naive Bayes Classification ####

# http://blog.thedigitalgroup.com/rajendras/2015/05/28/supervised-learning-for-text-classification/
# http://data-analytics.net/cep/Schedule_files/Textmining%20%20Clustering,%20Topic%20Modeling,%20and%20Classification.htm

library('e1071')
library('SparseM')

# TRAIN NAIVE BAYES MODEL
model <- naiveBayes(review.train, review.train$pol)

# PREDICTION
results <- predict(model, review.test)
t.nb.1 <- table(as.factor(results), as.factor(review.test$pol))
caret::confusionMatrix(t.nb.1)

# TRAIN NAIVE BAYES MODEL (AVEC REEQUILIBRAGE DU JEU DE DONNEES)
model.reeq <- naiveBayes(review.train.reeq, review.train.reeq$pol)

# PREDICTION
results.reeq <- predict(model.reeq, review.test.reeq)
t.nb.2 <- table(as.factor(results.reeq), as.factor(review.test.reeq$pol))
caret::confusionMatrix(t.nb.2)

# Régression logistique ####

library(InformationValue)
model.rl <- glm(review.train$pol ~ ., family = binomial, data = review.train)
summary(model.rl)
predicted <- predict(model.rl, review.test, type = "response")
optCutOff1 <- optimalCutoff(review.test$pol, predicted)[1]
predicted <- ifelse(predicted > optCutOff1, "positive", "negative")
t.rl <- table(as.factor(predicted), as.factor(review.test$pol))
caret::confusionMatrix(t.rl)

# Avec le jeu de donnees reequilibre cette fois

model.rl.reeq <- glm(review.train.reeq$pol ~ ., family = binomial, data = review.train.reeq)
summary(model.rl.reeq)
predicted.reeq <- predict(model.rl.reeq, review.test.reeq, type = "response")
optCutOff2 <- optimalCutoff(review.test.reeq$pol, predicted.reeq)[1]
predicted.reeq <- ifelse(predicted.reeq > optCutOff2, "positive", "negative")
t.rl.reeq <- table(as.factor(predicted.reeq), as.factor(review.test.reeq$pol))
caret::confusionMatrix(t.rl.reeq)

# Classification avec un réseau de neurones ####

library(nnet)
ideal <- class.ind(review.dtm$pol)
reviewANN = nnet(review.train[,-1264], ideal[review.train.index,], size=10, softmax=TRUE, MaxNWts = 12662)
res.ann = predict(reviewANN, review.test[,-1264], type = "class")
t.ann <- table(res.ann, review.test$pol)
caret::confusionMatrix(t.ann)

# Avec le jeu de donnees reequilibre

ideal.reeq <- class.ind(resamp1$pol)
reviewANN.reeq = nnet(review.train.reeq[,-1264], ideal.reeq[review.train.reeq.index,], size=10, softmax=TRUE, MaxNWts = 12662)
res.ann.reeq = predict(reviewANN.reeq, review.test.reeq[,-1264], type = "class")
t.ann.reeq <- table(res.ann.reeq, review.test.reeq$pol)
caret::confusionMatrix(t.ann.reeq)

# SVM ####

library(kernlab)
rbf <- rbfdot(sigma=0.1)
reviewSVM <- ksvm(pol~.,data=review.train,type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)
fitted(reviewSVM)
res.svm <- predict(reviewSVM, review.test[,-1264], type="probabilities")
predicted.svm <- ifelse(res.svm[, 1] > 0.5, "negative", "positive")
t.svm <- table(predicted.svm, review.test$pol)
caret::confusionMatrix(t.svm)

# Avec le jeu de donnees reequilibre

reviewSVM.reeq <- ksvm(pol~.,data=review.train.reeq,type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)
fitted(reviewSVM.reeq)
res.svm.reeq <- predict(reviewSVM.reeq, review.test.reeq[,-1264], type="probabilities")
predicted.svm.reeq <- ifelse(res.svm.reeq[, 1] > 0.5, "negative", "positive")
t.svm.reeq <- table(predicted.svm.reeq, review.test.reeq$pol)
caret::confusionMatrix(t.svm.reeq)

#### Other classifications for fun ####

# Adaboost : cf.documentation ####
library(fastAdaboost)
res.ada <- adaboost(pol ~ ., data = review.dtm, nIter = 10)
res.ada

# kNN ####
# http://www.dataperspective.info/2013/07/document-classification-using-r.html
