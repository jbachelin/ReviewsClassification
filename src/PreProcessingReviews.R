## Authors : Eric HAMMEL & Jordan BACHELIN
## Date 30/12/2017
## Desc : Nettoyage des avis 

## Fonction qui réalise le nettoyage de base d'un ensemble de textes
## INPUT :
##    dataset : objet contenant les avis 
## OUTPUT :
##    l'ensemble des commentaires nettoyés
## NOTE :  l'implémentation s'est inspirée de l'esprit de la fonction "SMOTE" du package DMwR,
##         cependant certaines tâche ont été adaptées aux données de grand volume
CleaningReviews <- function(dataset, RemoveSw = FALSE, StemDoc = FALSE)
{
  library("tm")
  library("NLP")
  library(SnowballC)
  
  reviews <- as.matrix(unique(dataset))
  review.corpus <- Corpus(VectorSource(reviews))
  review.corpus <- tm_map(review.corpus, content_transformer(tolower)) # Mise en minuscule
  review.corpus <- tm_map(review.corpus, removeNumbers) # Suppression des nombres
  review.corpus <- tm_map(review.corpus, removePunctuation) # Suppression de la ponctuation
  if (isTRUE(RemoveSw)) review.corpus <- tm_map(review.corpus, removeWords, stopwords('english'))
  # l'algorithme de porter est utilisé pas défaut
  if (isTRUE(StemDoc)) review.corpus <- tm_map(review.corpus, stemDocument, language = "english")

    return(review.corpus)
}
