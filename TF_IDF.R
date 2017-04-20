################# EJEMPLO TF-IDF ####################
## Nombre: frecuencia de términos y frecuencia inversa de documentos
##----------------------------------------------------------------------
## Objetivo: se Calcula el TF-IDF para la colección de documentos
## Usando la matriz de frecuencia de términos, se calcula el peso de idf. 
## luego se halla el elbow method para determinar el k optimo
## y a partir de esto se aplica el k-means a los documentos obteniendo 
## el resultado de cada cluster. 
######################################################

#------------------------------------------------------------------------

#' paso 1: Importar las librerias necesarias 

rm(list = ls())
library(cluster)
library(ggplot2)
library(factoextra)
library(tm)

#------------------------------------------------------------------------

#' paso 2: se crea un vector documentos de ejemplo
x <-
  c(
    "los planetas giran alrededor del sol",
    "las agujas del reloj giran",
    "las peonzas giran al igual que giran los planetas",
    "los planetas y el sol son astros"
  )
#------------------------------------------------------------------------

#' paso 3: Se define el corpus
doc_corpus <- Corpus(VectorSource(x))

#------------------------------------------------------------------------

#' paso 4: Pre-procesamiento da forma al corpus que permite analizar métodos estadísticos
#' la cual se realiza la respectiva limpieza 
doc_corpus  <- tm_map(doc_corpus , stripWhitespace)
doc_corpus  <- tm_map(doc_corpus , removePunctuation)
doc_corpus <- tm_map(doc_corpus , removeWords, stopwords("spanish"))
doc_corpus  <- tm_map(doc_corpus , stemDocument)
doc_corpus  <- tm_map(doc_corpus , removeNumbers)

#------------------------------------------------------------------------

#' paso 5: La representación transforma el corpus de documentos a un espacio vectorial para procesarlos
dtm <-
  DocumentTermMatrix(doc_corpus, control = list(weighting = weightTf))
#inspect(dtm)

Tf <- as.matrix(dtm)
#View(Tf)

dtm <-
  DocumentTermMatrix(doc_corpus, control = list(weighting = weightTfIdf))
#inspect(dtm)

TfIdf <- as.matrix(dtm)
TfIdf <- t(TfIdf)

#------------------------------------------------------------------------
#' paso 6: Elbow method for k-means clustering
mydata <- TfIdf
k.max <- 4 # Maximal number of clusters
data <- mydata
wss <- sapply(1:k.max,
              function(k) {
                kmeans(data, k, nstart = 2)$tot.withinss
              })
plot(
  1:k.max,
  wss,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)
abline(v = 2, lty = 2)

#------------------------------------------------------------------------
#' paso 7:  K-means

df <- data.frame(TfIdf)
df <- t(df)
distancias1<-dist(df,method="euclidean")
cluster1<-hclust(distancias1)

res <- kmeans(distancias1, 2)
#res

df <- data.frame(Tf)
#df

res <- kmeans(df, 2)
#res

#------------------------------------------------------------------------