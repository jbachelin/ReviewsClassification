# Projet Text Mining ####
# Auteur : Jordan BACHELIN ####

rm(list = ls())
library(XML)

# Chargement des données ####

xmldoc <- xmlParse("data/train_2017.xml")
rootNode <- xmlRoot(xmldoc)
rootNode[1]
data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
cd.catalog <- data.frame(t(data),row.names=NULL)
cd.catalog[1:2,]
