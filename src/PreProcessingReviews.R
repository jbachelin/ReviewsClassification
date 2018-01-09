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
CleaningReviews <- function(dataset, RemoveSw = FALSE, ExceptionVector = NULL, StemDoc = FALSE)
{
  reviews <- as.matrix(unique(dataset))
  review.corpus <- Corpus(VectorSource(reviews))
  review.corpus <- tm_map(review.corpus, content_transformer(tolower)) # Mise en minuscule
  review.corpus <- tm_map(review.corpus, removeNumbers) # Suppression des nombres
  review.corpus <- tm_map(review.corpus, removePunctuation) # Suppression de la ponctuation

  if (isTRUE(RemoveSw)) {
    if (is.null(ExceptionVector)) CustomStopwords <- stopwords("en") else CustomStopwords <- setdiff(stopwords("en"), ExceptionVector)
    review.corpus <- tm_map(review.corpus, removeWords, CustomStopwords)
  }
  
  # review.corpus <- unlist(lapply(review.corpus, FUN = str_negate))
  # review.corpus <- Corpus(VectorSource(review.corpus))
  
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
  LemmatizedCorpus <- lapply(CorpusVector, function(x){
    split.review <- unlist(strsplit(x = x, split = " "))
    split.review <- SpellChecker(ToCheck = split.review)
            tag.reviews <- treetag(split.review, treetagger = "manual", lang = "en",
                               TT.options = list(path = "~/Treetagger",
                                                 preset = "en"),
                               format = "obj", stemmer = stemDocument,
                               stopwords = if (is.null(ExceptionVector)) CustomStopwords else stopwords("en"))
    if (isTRUE(RmStopwords)) {
      tag.reviews@TT.res$lemma[which(tag.reviews@TT.res$lemma == "<unknown>")] <- tag.reviews@TT.res$token[which(tag.reviews@TT.res$lemma == "<unknown>")]
      if (isTRUE(identical(split.review,character(0)))) return(x) else return(tag.reviews@TT.res$lemma[-which(tag.reviews@TT.res$stop == TRUE)])
    } else {
      tag.reviews@TT.res$lemma[which(tag.reviews@TT.res$lemma == "<unknown>")] <- tag.reviews@TT.res$token[which(tag.reviews@TT.res$lemma == "<unknown>")]
      if (isTRUE(identical(split.review,character(0)))) return(x) else return(tag.reviews@TT.res$lemma)
    }
    } 
    )
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

## Fonction qui détecte une négation et qui ajoute le préfixe au mot suivant 
## INPUT :
##    x : chaine de caractères
## OUTPUT :
##    chaine de caractères avec les préfixes au mot suivant une négation
str_negate <- function(x) {
  str_split <- unlist(strsplit(x=x, split=" "))
  is_negative <- grepl("not|n't",str_split,ignore.case=T)
  negate_me <- append(FALSE,is_negative)[1:length(str_split)]
  str_split[negate_me==T]<- paste0("not_",str_split[negate_me==T])
  paste0(str_split)
}
