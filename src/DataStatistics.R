# Projet Text Mining ####
# Auteurs : Jordan BACHELIN & Eric HAMMEL ####

# Statistiques sur le jeu de données

source("src/DataLoading.R")
source("src/PreProcessingReviews.R")
library("wordcloud")
library("RColorBrewer")
library("tm")
library("NLP")
library(SnowballC)
library(koRpus)

length(unique(opinions$review)) # 542 avis differents dans le jeu d'entrainement après filtrage dont :

length(which(opinions$polarity == "positive")) # 408 contenant un avis positif
length(which(opinions$polarity == "negative")) # 136 contenant un avis negatif
# length(which(opinions$polarity == "neutral")) # [...] contenant un avis neutre

# length(unique(opinions$target)) # 723 cibles differentes dans le jeu d'entrainement (y compris la cible NULL)
length(unique(opinions$category)) # 12 catégories d'avis differentes dans le jeu d'entrainement (y compris NA)

# Pretraitement des avis
review.corpus <- CleaningReviews(opinions$review)
inspect(review.corpus[1])

# Approche fréquentiste des mots presents dans les avis (sans mots-outils seulement)
review.corpus.sw <- tm_map(review.corpus, removeWords, stopwords('english'))
message("Review with stopwords:")
review.corpus[[1]]$content
message("Review without stopwords:")
review.corpus.sw[[1]]$content
review.dtm.sw <- TermDocumentMatrix(review.corpus.sw)
m.sw <- as.matrix(review.dtm.sw)
review.freq.sw <- rowSums(m.sw)
review.freq.sw.sorted <- sort(review.freq.sw, decreasing = TRUE)
# Histogramme representant les 50 premiers mots les plus frequents dans le corpus total
barplot(review.freq.sw.sorted[1:50], xlab = "Word", ylab = "Frequency", las = 2)
# Nuage des mots du corpus (plus ou moins gros en fonction de leur frequence)
wordcloud(review.corpus.sw, max.words = 200, colors = brewer.pal(8, "Dark2"))

# Approche fréquentiste des mots presents dans les avis (avec stemming et TF/IDF cette fois)
review.corpus.stemmed <- tm_map(review.corpus.sw, stemDocument, language = "porter")
message("Review without stemming:")
review.corpus.sw[[1]]$content
message("Review with stemming:")
review.corpus.stemmed[[1]]$content
review.dtm.stemmed.tfidf = TermDocumentMatrix(review.corpus.stemmed, control = list(weighting = weightTfIdf))
m.stemmed.tfidf <- as.matrix(review.dtm.stemmed.tfidf)
# Ou pour plus de lisibilite
review.freq.stemmed.tfidf <- rowSums(m.stemmed.tfidf)
review.freq.stemmed.tfidf.sorted <- sort(review.freq.stemmed.tfidf, decreasing = TRUE)
# Histogramme representant les 50 premiers mots les plus frequents dans le corpus total
barplot(review.freq.stemmed.tfidf.sorted[1:50], xlab = "Word", ylab = "Frequency", las = 2)
# Nuage des mots du corpus (plus ou moins gros en fonction de leur frequence)
wordcloud(review.corpus.stemmed, max.words = 200, colors = brewer.pal(8, "Dark2"))
