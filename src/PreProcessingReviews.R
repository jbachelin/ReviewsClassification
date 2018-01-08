## Authors : Eric HAMMEL & Jordan BACHELIN
## Date 30/12/2017
## Desc : Nettoyage des avis 

library("tm")
library("NLP")
library(SnowballC)
library("koRpus") # PoS tagging and lemmatization
library("hunspell") # spell checking

## Fonction qui réalise le nettoyage de base d'un ensemble de textes
## INPUT :
##    dataset : objet contenant les avis 
## OUTPUT :
##    l'ensemble des commentaires nettoyés
## NOTE :  l'implémentation s'est inspirée de l'esprit de la fonction "SMOTE" du package DMwR,
##         cependant certaines tâche ont été adaptées aux données de grand volume
CleaningReviews <- function(dataset, RemoveSw = FALSE, StemDoc = FALSE)
{
  
  
  reviews <- as.matrix(unique(dataset))
  review.corpus <- Corpus(VectorSource(reviews))
  review.corpus <- tm_map(review.corpus, content_transformer(tolower)) # Mise en minuscule
  review.corpus <- tm_map(review.corpus, removeNumbers) # Suppression des nombres
  review.corpus <- tm_map(review.corpus, removePunctuation) # Suppression de la ponctuation

  if (isTRUE(RemoveSw)) review.corpus <- tm_map(review.corpus, removeWords, stopwords('english'))
  # l'algorithme de porter est utilisé pas défaut
  if (isTRUE(StemDoc)) review.corpus <- tm_map(review.corpus, wordStem, language = "english")

  review.corpus <- tm_map(review.corpus, stripWhitespace) # Suppression des espaces en trop
  
  return(review.corpus)
}

## Fonction qui lemmatise les avis du corpus et réalise PoS
## INPUT :
##    CorpusVector : Corpus d'avis à lemmatiser
##    ExceptionVector : vecteur de mot-outils à garder
##    RmStopwords : booléen qui permet de réaliser la suppression des mots outils ou non
## OUTPUT :
##    Corpus des commentaires lemmatisé
## NOTE :  Certains mots n'ont pas été lemmatisé, donc le token a été utilisé
LemmatizeReviews <- function(CorpusVector, ExceptionVector = NULL, RmStopwords = FALSE)
{
  ## création de la liste stop words customisés
  if (is.null(ExceptionVector)) CustomStopwords <- stopwords("en") else CustomStopwords <- setdiff(stopwords("en"),ExceptionVector) 
  LemmatizedCorpus <- lapply(X = CorpusVector, FUN = function(x){
      tag.reviews <- treetag(x, treetagger = "manual", lang = "en",
                             TT.options = list(path = "~/Treetagger",
                                               preset = "en"),
                             format = "obj", stemmer = stemDocument,
                             stopwords = CustomStopwords)
      if (RmStopwords == TRUE) {
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
  return(LemmatizedCorpus)
}

## Fonction qui réalise une correction orthographique des mots comportant des erreurs
## INPUT :
##    ToCheck : vecteur des token à corriger
## OUTPUT :
##    Vecteur de mots corrigé
SpellChecker <- function(ToCheck)
{
    ToCheck[which(hunspell_check(ToCheck) == FALSE)] <- lapply(ToCheck[which(hunspell_check(ToCheck) == FALSE)], function(x){
    unlist(hunspell_suggest(x))[1]
  })
  
  return(unlist(ToCheck))
}


