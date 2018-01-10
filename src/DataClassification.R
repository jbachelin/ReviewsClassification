# Projet Text Mining ####
# Auteurs : Jordan BACHELIN & Eric HAMMEL ####
# Desc : Fonctions permettant l'exécution des différents algorithmes de classification retenus

# Test de plusieurs algorithmes de classification sur le jeu de donnees d'entrainement
# https://www.datacamp.com/courses/supervised-learning-in-r-classification

# Naive Bayes Classification ####

# http://blog.thedigitalgroup.com/rajendras/2015/05/28/supervised-learning-for-text-classification/
# http://data-analytics.net/cep/Schedule_files/Textmining%20%20Clustering,%20Topic%20Modeling,%20and%20Classification.htm

my_naive_bayes <- function(model, test){

  # PREDICTION
  predicted <- predict(model, newdata = test)
  
  # DISPLAY RESULTS
  t <- table(predicted, test$polarity)
  cm <- caret::confusionMatrix(t)
  return(list(cm = cm, predicted = predicted))
}

# Régression logistique ####

my_rl <- function(model, test){
  require(InformationValue)
  require('caret')
  
  # PREDICTION
  predicted <- predict(model$finalModel, test)
  threshold <- optimalCutoff(test$polarity, predicted)
  predicted <- ifelse(predicted > threshold, "positive", "negative")
  
  # DISPLAY RESULTS
  res3 <- data.frame(predicted, polarity = test$polarity)
  cm <- caret::confusionMatrix(data = res3$predicted, reference = res3$polarity)
  return(list(cm = cm, predicted = predicted))
}

# Classification avec un réseau de neurones ####

my_nn <- function(model, test){
  require(nnet)
  require(caret)
  
  # PREDICTION
  res.ann = predict(model, test[, -which(names(test) == "polarity")], type = "class")
  
  # DISPLAY RESULTS
  t <- table(res.ann, test$polarity)
  cm <- caret::confusionMatrix(t)
  return(list(cm = cm, predicted = res.ann))
}

# SVM ####

my_svm <- function(model, test){
  require(kernlab)
  
  # PREDICTION
  fitted(model) # reviewSVM
  res.svm <- predict(model, test, type = "probabilities")
  predicted.svm <- ifelse(res.svm[, 1] > 0.5, "negative", "positive")
  
  # DISPLAY RESULTS
  t <- table(predicted.svm, test$polarity)
  cm <- caret::confusionMatrix(t)
  return(list(cm = cm, predicted = predicted.svm))
}
