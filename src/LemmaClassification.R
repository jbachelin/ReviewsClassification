## Authors : Eric HAMMEL & Jordan BACHELIN
## Date : 30/12/2017
## Desc : Predicting Polarity in the un balanced data set of reviews

rm(list = ls())

## importing data
source("src/DataLoading.R")
source("src/PreProcessingReviews.R")

## import library
library("caret")
library("DMwR") # SMOTE
library("klaR") # naive bayes
library("e1071") #SVM
library("pROC") # ROC curves

#### Lematize Corpus ####
ReviewsCleaned <- CleaningReviews(dataset = opinions$review,
                                  RemoveSw = FALSE, StemDoc = FALSE)
test <- lapply(X = ReviewsCleaned, function(x){ x })
lemmed <- LemmatizeReviews(CorpusVector = test, RmStopwords = TRUE)
lemmed2 <- Corpus(VectorSource(lemmed))
lemmed2 <- tm_map(lemmed2, stripWhitespace) # Suppression des espaces en trop

lemmed.dtm <- t(as.matrix(TermDocumentMatrix(lemmed2,
                                             control = list(weighing = weightTf))))
dim(lemmed.dtm) # 542 1072
pol <- as.matrix(opinions$polarity[which(duplicated(opinions$review) == FALSE)])
lemmed.dtm <- data.frame(lemmed.dtm, pol)

## partitioning data
set.seed(1234)
train.idx <- createDataPartition(y = lemmed.dtm$pol, p = 0.8, list = FALSE)
train.dtm <- lemmed.dtm[train.idx,]
resampled.train.dtm <- SMOTE(pol ~., data = train.dtm, K = 5)
test.dtm <- lemmed.dtm[-train.idx,]
#10 fold-CrossValidation
tr.control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
#                           ,classProbs = TRUE)

#### Naive Bayes ####
## Non-resampled data
model.naive.bayes1 <- train(pol ~ ., data = train.dtm, method = "nb",
                            trControl = tr.control)
# assessing naive bayesion model
mnb1.validate <- predict(model.naive.bayes1$finalModel,
                         newdata = test.dtm)
res1 <- data.frame(mnb1.validate, pol = test.dtm$pol)
confusionMatrix(data = res1$class, reference = res1$pol)
#ROC curve
mnb1.prob <- predict(model.naive.bayes1, test.dtm, type = "prob")
mnb1.roc <- roc(response = mnb1.prob, predictor = res1$pol) # à demander au prof
plot(mnb1.roc, col = "red")

## Resampled data
model.naive.bayes2 <- train(pol ~ ., data = resampled.train.dtm, method = "nb",
                            trControl = tr.control)
# assessing naive bayesion model
mnb2.validate <- predict(model.naive.bayes2$finalModel, newdata = test.dtm)
res2 <- data.frame(mnb2.validate, pol = test.dtm$pol)
caret::confusionMatrix(data = res2$class, reference = res1$pol)

#### Logistic Regression ####
## Non-resampled
# http://larmarange.github.io/analyse-R/regression-logistique.html
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
model.logistic1 <- train(pol ~ ., data = train.dtm, method = "glm",
                         family = "binomial", trControl = tr.control)
# assessing logistic regression
library("InformationValue")
ml1.validate <- predict(model.logistic1$finalModel, test.dtm)
threshold <- optimalCutoff(test.dtm$pol, ml1.validate)
ml1.validate <- ifelse(ml1.validate > threshold, "positive", "negative")
res3 <- data.frame(ml1.validate, pol = test.dtm$pol)
caret::confusionMatrix(data = res3$ml1.validate, reference = res3$pol)
# ROC curve
mnb1.prob <- predict(model.naive.bayes1, test.dtm, type = "prob")

ml1.roc <- roc(test.dtm$pol, data = test.dtm, levels = c("negative", "positive"))
plot(ml1.roc, col = "red")

## Resampled data
model.logistic2 <- train(pol ~ ., data = resampled.train.dtm, method = "glm",
                         family = "binomial", trControl = tr.control)
# assessing logistic regression
library("InformationValue")
ml1.validate <- predict(model.logistic1$finalModel, test.dtm)
threshold <- optimalCutoff(test.dtm$pol, ml1.validate)
ml1.validate <- ifelse(ml1.validate > threshold, "positive", "negative")
res3 <- data.frame(ml1.validate, pol = test.dtm$pol)
caret::confusionMatrix(data = res3$ml1.validate, reference = res3$pol)

## SVM 
model.svm1 <- train(pol ~ ., data = lemmed.dtm, method = 'svmLinear2',
                    trControl = tr.control, metric = "ROC")
# assessing logistic regression
msvm1.validate <- predict(model.naive.bayes1$finalModel, test.dtm)
res3 <- data.frame(msvm1.validate, pol = test.dtm$pol)
confusionMatrix(data = res3$class, reference = res3$pol)
# ROC curve
mnb1.prob <- predict(model.naive.bayes1, test.dtm, type = "prob")
msvm1.roc <- roc(test.dtm$pol, data = test.dtm, levels = c("negative", "positive"))
plot(msvm1.roc, col = "red")
