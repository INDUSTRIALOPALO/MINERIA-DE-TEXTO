
################# PRUEBA DE DATOS ####################
## Nombre: Datos del Benchmark
##---------------------------------------------------------------
## Objetivo: evaluar las diferentes combinaciones de las variantes del algoritmo k-means,
##(Hartigan, Lloyd, Forgy y MacQueen) y las distancias, (Euclidiana, Maximum, Manhattan, Minkowski)
######################################################

#importar librerias
rm(list = ls())
set.seed(0)
library(ggplot2)
library(tm)
library(caret)
library(lattice)

#---------------------------------------------------------------

#' paso 1: Cargar los datos de prueba
mensajes <- read.table(file = "SMSSpamCollection", sep = "\t")
View(mensajes)
doc <- mensajes[, 2]

#---------------------------------------------------------------

#'paso 2: Se define el corpus
doc_corpus <- Corpus(VectorSource(doc))

#---------------------------------------------------------------

#' paso 3: Se hace la respectiva limpieza de los datos de prueba
doc_corpus  <- tm_map(doc_corpus , stripWhitespace)
doc_corpus <- tm_map(doc_corpus , content_transformer(tolower))
doc_corpus  <- tm_map(doc_corpus , removePunctuation)
doc_corpus <- tm_map(doc_corpus , removeWords, stopwords("english"))
doc_corpus  <- tm_map(doc_corpus , stemDocument, language = "english")
doc_corpus  <- tm_map(doc_corpus , removeNumbers)

#---------------------------------------------------------------

#' paso3: Se transforma el Corpus a un espacio vectorial
dtm <-
  DocumentTermMatrix(doc_corpus, control = list(weighting = weightTfIdf))
inspect(dtm)

TfIdf <- as.matrix(dtm)
TfIdf <- t(TfIdf)

#---------------------------------------------------------------

#? paso 4: Se halla el K-means con las diferentes variaciones y distancias propuestas
df <- data.frame(TfIdf)
df <- t(df)

distancias1 <- dist(df, method = "minkowski", p = 5)

res <- kmeans(distancias1, 2, algorithm = "Lloyd")
res

res$withinss
res$betweenss
res$totss

#---------------------------------------------------------------

#' paso 5: comparacion de vectores de cluster y ham spam

#vector con los cluster
vectormodelo <- res$cluster
vectormodelo <- factor(vectormodelo)
resumenvmodelo <- summary(vectormodelo)
resumenvmodelo <- resumenvmodelo / 1630
etiqueta <- "spam"
if (resumenvmodelo[1] < resumenvmodelo[2]) {
  etiqueta <- "ham"
  positivo <- "2"
} else{
  etiqueta <- "spam"
  positivo <- "1"
}

#vector real
vectorreal <- mensajes[, 1]
vectorreal <- vectorreal == etiqueta
vectorreal <- vectorreal + 1
vectorreal <- factor(vectorreal)

#Realizamos la comparacion de los dos vectores

comparcion <- vectorreal == vectormodelo
summary(comparcion)

#matriz de confusión

matriz <-
  confusionMatrix(data = vectormodelo,
                  positive = positivo ,
                  reference =  vectorreal)
matriz
matriz$byClass

#---------------------------------------------------------------
