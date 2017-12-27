# Projet Text Mining ####
# Auteurs : Jordan BACHELIN & Eric HAMMEL ####

rm(list = ls())
library(XML)
library(dplyr)

# Chargement des données ####

data <- xmlParse("data/train_2017.xml")
xml_data <- xmlToList(data)

# reviews <- matrix(0, length(xml_data))
# target <- matrix(0, length(xml_data))
# category <- matrix(0, length(xml_data))
# polarity <- matrix(0, length(xml_data))
# from <- matrix(0, length(xml_data))
# to <- matrix(0, length(xml_data))

filldf <- function(xml_data, opinions){
  for (cell in 1:length(xml_data))
  {
    for (idx in 1:length(xml_data[[cell]]$sentences)) {
      review <- as.character(xml_data[[cell]]$sentences[[idx]]$text)
      if (length(xml_data[[cell]]$sentences[[idx]]) == 2) {
        opinions[nrow(opinions) + 1, ] <- c(review, NA, NA, NA, NA, NA, as.numeric(cell))
      } else if (length(xml_data[[cell]]$sentences[[idx]]$Opinions) > 1) {
        # Pour l'instant on ne considere pas les avis ayant plusieurs polarites  
        break
        # Code permettant de les traiter plus tard
          # for (opinion in 1:length(xml_data[[cell]]$sentences[[idx]]$Opinions)) {
          #   opinions[nrow(opinions) + 1, ] <- c(review,
          #                                       as.character(xml_data[[cell]]$sentences[[idx]]$Opinions[opinion]$Opinion[[1]]),
          #                                       as.character(xml_data[[cell]]$sentences[[idx]]$Opinions[opinion]$Opinion[[2]]),
          #                                       as.character(xml_data[[cell]]$sentences[[idx]]$Opinions[opinion]$Opinion[[3]]),
          #                                       as.numeric(xml_data[[cell]]$sentences[[idx]]$Opinions[opinion]$Opinion[[4]]),
          #                                       as.numeric(xml_data[[cell]]$sentences[[idx]]$Opinions[opinion]$Opinion[[5]]),
          #                                       as.numeric(cell))
          #}
      } else {
        opinions[nrow(opinions) + 1, ] <- c(review,
                                            as.character(xml_data[[cell]]$sentences[[idx]]$Opinions$Opinion[[1]]),
                                            as.character(xml_data[[cell]]$sentences[[idx]]$Opinions$Opinion[[2]]),
                                            as.character(xml_data[[cell]]$sentences[[idx]]$Opinions$Opinion[[3]]),
                                            as.numeric(xml_data[[cell]]$sentences[[idx]]$Opinions$Opinion[[4]]),
                                            as.numeric(xml_data[[cell]]$sentences[[idx]]$Opinions$Opinion[[5]]),
                                            as.numeric(cell))
      }
    }
  }
  opinions
}

opinions <- data.frame(rewiew = character(0),
                       target = character(0),
                       category = character(0),
                       polarity = character(0),
                       from = numeric(),
                       to = numeric(),
                       index = numeric(),
                       stringsAsFactors = FALSE)

opinions <- filldf(xml_data, opinions)
opinions <- opinions[, -2] # On retire la variable "target"
opinions <- opinions[-which(opinions$polarity == "neutral" | is.na(opinions$polarity)),] # On ne prend pas en consideration les polarites neutres ou manquantes

# test <- opinions
# test <- aggregate(. ~ test$rewiew, test, list)
