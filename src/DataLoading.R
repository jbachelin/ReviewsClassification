# Projet Text Mining ####
# Auteur : Jordan BACHELIN ####

rm(list = ls())
library(XML)

# Chargement des données ####

data <- xmlParse("data/train_2017.xml")
xml_data <- xmlToList(data)

reviews <- matrix(0, length(xml_data))
target <- matrix(0, length(xml_data))
category <- matrix(0, length(xml_data))
polarity <- matrix(0, length(xml_data))
from <- matrix(0, length(xml_data))
to <- matrix(0, length(xml_data))

for (cell in 1:length(xml_data))
{
  reviews[cell] <- xml_data[[cell]]$sentences$sentence$text
  opinions <- as.list(xml_data[[cell]]$sentences$sentence$Opinions$Opinion)
  target[cell] <- if(is.null(opinions$target)) NA else opinions$target
  category[cell] <- if(is.null(opinions$category)) NA else opinions$category
  polarity[cell] <- if(is.null(opinions$polarity)) "neutral" else opinions$polarity
  # from[cell] <- opinions$from
  # to[cell] <- opinions$to
}

opinions <- data.frame(reviews, target, category, polarity, from, to)
