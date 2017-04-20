################# CASO DE ESTUDIO ####################
## Nombre: PROCESAMIENTO DE LENGUAJE NATURAL
##---------------------------------------------------------------
## Objetivo: se aplica técnicas de minería de datos haciendo un análisis
## descriptivo de un conjunto de datos obtenidos de la red social twitter
############################################################

#importar librerias
rm(list = ls())

#importar librerias
library(bitops)
library(RCurl)
library(twitteR)
library(tm)
library(Rcpp)
library(RColorBrewer)
library(wordcloud)
library(readr)
library(ggplot2)
library(base)
library(cluster)

##---------------------------------------------------------------

#' paso 1: importar datos de csv
vanguardia <-
  read_delim("~/vanguardia.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)
View(vanguardia)

vacios <- is.na(vanguardia$favorited)
vanguardia <- vanguardia[!vacios, ]

##---------------------------------------------------------------

#' paso 2: seleccionar el texto de los tweets
texto <- vanguardia$text
texto <- iconv(texto, to = "UTF-8")

##---------------------------------------------------------------

#' paso 3: limpiar los datos de elementos no deseados
textoclean <- gsub("(RT|via)((?:/b/w*@/w+)+)", "", texto) #Retuits
textoclean <- gsub("@/w+", "", textoclean) #@otragente
textoclean <-
  gsub("[[:punct:]]", "", textoclean) #simbolos de puntuacion
textoclean <- gsub("[[:digit:]]", "", textoclean) #numeros
textoclean <- gsub("https//t.co/w+", "", textoclean) #links
textoclean <- gsub("httpst", "", textoclean) #links
textoclean <- gsub("#", "", textoclean)
textoclean <- gsub("%", "", textoclean)

##---------------------------------------------------------------

#' paso 4: se construye un corpus
corpus <- Corpus(VectorSource(textoclean))

##---------------------------------------------------------------

#' paso 5: tokenizar los corpus

#quitamos  palabras vacias
corpus <- tm_map(corpus, removeWords,  c("Bucaramanga"))

#quitamos las palabras vacias del contenido en español
corpus <-
  tm_map(corpus, removeWords, c(stopwords("spanish"), "vanguardiacom"))

#quitamos los espacios en blanco
corpus <- tm_map(corpus, stripWhitespace)

#se pasa a minuscula
corpus <- tm_map(corpus , content_transformer(tolower))

##---------------------------------------------------------------

#' paso 6: La representación transforma el corpus de documentos a un espacio vectorial para procesarlos
#TfIdf
dtm <-
  DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
inspect(dtm)

TfIdf <- as.matrix(dtm)
TfIdf <- t(TfIdf)

##---------------------------------------------------------------

#' paso 7: se utiliza el Elbow method for k-means clustering
mydata <- TfIdf
k.max <- 15 # Maximal number of clusters
data <- mydata
wss <- sapply(1:k.max,
              function(k) {
                kmeans(data, k, nstart = 2)$tot.withinss
              })
plot(
  1:k.max,
  wss,
  type = "b",
  pch = 15,
  frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)
abline(v = 4, lty = 2)

##---------------------------------------------------------------

#' paso 8: Se obtiene el kmeans para su respectico análisis de grupos
df <- data.frame(TfIdf)
df <- t(df)

distancias1 <- dist(df, method = "manhattan")

res <- kmeans(distancias1, 4, algorithm = "Hartigan")
res

clusplot(
  as.matrix(distancias1),
  main =  " ",
  res$cluster,
  color = T,
  shade = T,
  labels = 0,
  lines = 0,
  col.clus = c ("steelblue", "darkred", "darkgreen", "darkgray")[res$cluster],
  col.p = c ("steelblue", "darkred", "darkgreen", "darkgray")[res$cluster],
  col.txt = c("steelblue", "darkred", "darkgreen", "darkgray")[res$cluster],
  cex.txt = 0.9
)

write.table(
  x = res$cluster,
  file = "clustering.csv",
  col.names = TRUE,
  row.names = FALSE
)

##---------------------------------------------------------------

#' Se guarda la tabla de clustering en un csv para su posterior análisis
clustering <- read_csv("~/clustering.csv")
View(clustering)