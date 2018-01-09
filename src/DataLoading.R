# Projet Text Mining ####
# Auteurs : Jordan BACHELIN & Eric HAMMEL ####

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
