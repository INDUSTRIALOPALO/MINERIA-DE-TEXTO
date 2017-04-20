################# TWEETS #############################
## Nombre: ANÁLISIS DE LOS CLUSTERS
##-------------------------------------------------------------------------
## Objetivo: siguiente ejemplo muestra la búsqueda realizada en los dos meses,
## al usuario vanguardiacom en los últimos 3160 tweets, sacando información
## relevante de cada uno de los grupos obtenidos
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
##-------------------------------------------------------------------------

#' paso 1: importar datos de csv
vanguardia <-
  read_delim("~/vanguardia.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)
View(vanguardia)

vacios <- is.na(vanguardia$favorited)
vanguardia <- vanguardia[!vacios, ]

##-------------------------------------------------------------------------

#'paso 2: seleccionar el texto de los tweets
texto <- vanguardia$text
texto <- iconv(texto, to = "UTF-8")

##-------------------------------------------------------------------------

#'paso 3: limpiar los datos de elementos no deseados
textoclean <- gsub("(RT|via)((?:/b/w*@/w+)+)", "", texto) #Retuits
textoclean <- gsub("@/w+", "", textoclean) #@otragente
textoclean <-
  gsub("[[:punct:]]", "", textoclean) #simbolos de puntuacion
textoclean <- gsub("[[:digit:]]", "", textoclean) #numeros
textoclean <- gsub("https//t.co/w+", "", textoclean) #links
textoclean <- gsub("httpst", "", textoclean) #links
textoclean <- gsub("#", "", textoclean)
textoclean <- gsub("%", "", textoclean)

##-------------------------------------------------------------------------

#' paso 4: importar res$clustering de csv
clustering <- read_csv("~/clustering.csv")
View(clustering)
##-------------------------------------------------------------------------

#' paso 5: crear los grupos obtenidos del res$clustering
grupo1 <- textoclean[clustering == 1]
grupo2 <- textoclean[clustering == 2]
grupo3 <- textoclean[clustering == 3]
grupo4 <- textoclean[clustering == 4]

##-------------------------------------------------------------------------

#' paso 6: se construye un corpus para cada grupo
corpus1 <- Corpus(VectorSource(grupo1))
corpus2 <- Corpus(VectorSource(grupo2))
corpus3 <- Corpus(VectorSource(grupo3))
corpus4 <- Corpus(VectorSource(grupo4))

##-------------------------------------------------------------------------

#' paso 7: tokenizar los corpus

#se pasa a minuscula
corpus1 <- tm_map(corpus1 , content_transformer(tolower))
corpus2 <- tm_map(corpus2 , content_transformer(tolower))
corpus3 <- tm_map(corpus3 , content_transformer(tolower))
corpus4 <- tm_map(corpus4 , content_transformer(tolower))


#STOP_WORDS
stop = read.table("C:/textos/SSW1.txt", header = TRUE)
stop_vec1 = as.vector(stop$SPANISH_STOP_WORDS1)

stop = read.table("C:/textos/SSW2.txt", header = TRUE)
stop_vec2 = as.vector(stop$SPANISH_STOP_WORDS2)

stop = read.table("C:/textos/SSW3.txt", header = TRUE)
stop_vec3 = as.vector(stop$SPANISH_STOP_WORDS3)

stop = read.table("C:/textos/SSW4.txt", header = TRUE)
stop_vec4 = as.vector(stop$SPANISH_STOP_WORDS4)



#quitamos las palabras vacias del contenido en español
corpus1 <- tm_map(corpus1, removeWords, c(stopwords("spanish")))
corpus1 <- tm_map(corpus1, removeWords, stop_vec1)

corpus2 <- tm_map(corpus2, removeWords, c(stopwords("spanish")))
corpus2 <- tm_map(corpus2, removeWords, stop_vec2)

corpus3 <- tm_map(corpus3, removeWords, c(stopwords("spanish")))
corpus3 <- tm_map(corpus3, removeWords, stop_vec3)

corpus4 <- tm_map(corpus4, removeWords, c(stopwords("spanish")))
corpus4 <- tm_map(corpus4, removeWords, stop_vec4)



#quitamos los espacios en blanco
corpus1 <- tm_map(corpus1, stripWhitespace)
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus3 <- tm_map(corpus3, stripWhitespace)
corpus4 <- tm_map(corpus4, stripWhitespace)


#se crea la matriz de terminos a partir de corpus1
tdm1 <- TermDocumentMatrix(corpus1)
m1 <- as.matrix(tdm1)
tdm2 <- TermDocumentMatrix(corpus2)
m2 <- as.matrix(tdm2)
tdm3 <- TermDocumentMatrix(corpus3)
m3 <- as.matrix(tdm3)
tdm4 <- TermDocumentMatrix(corpus4)
m4 <- as.matrix(tdm4)


#ordena el uso de palabras
wf1 <- sort(rowSums(m1), decreasing = TRUE)
wf2 <- sort(rowSums(m2), decreasing = TRUE)
wf3 <- sort(rowSums(m3), decreasing = TRUE)
wf4 <- sort(rowSums(m4), decreasing = TRUE)

##-------------------------------------------------------------------------

#' paso 8: Se crea un data frame para cada grupo
dm1 <- data.frame(word = names(wf1), freq = wf1)
dm2 <- data.frame(word = names(wf2), freq = wf2)
dm3 <- data.frame(word = names(wf3), freq = wf3)
dm4 <- data.frame(word = names(wf4), freq = wf4)

##-------------------------------------------------------------------------

#' paso 9: Se obtiene la nube de palabras para cada grupo

#imprime la nube de palabras para el grupo 1
wordcloud(
  dm1$word,
  dm1$freq,
  max.words = 17,
  random.order = FALSE,
  colors = brewer.pal(8, "Set2")
)

#imprime la nube de palabras para el grupo 2
wordcloud(
  dm2$word,
  dm2$freq,
  max.words = 17,
  random.order = FALSE,
  colors = brewer.pal(8, "Set2")
)

#imprime la nube de palabras para el grupo 3
wordcloud(
  dm3$word,
  dm3$freq,
  max.words = 17,
  random.order = FALSE,
  colors = brewer.pal(8, "Set2")
)

#imprime la nube de palabras para el grupo 4
wordcloud(
  dm4$word,
  dm4$freq,
  max.words = 17,
  random.order = FALSE,
  colors = brewer.pal(8, "Set2")
)

##-------------------------------------------------------------------------

#' paso 10: Se obtiene el Histograma de la frecuencia de palabras para cada grupo

# Histograma de palabras frecuentes en el grupo 1

grafico1 <- ggplot(subset(dm1, freq >= 20), aes(word, freq))
grafico1 <-
  grafico1 + geom_bar(stat = "identity", position = "identity", aes(fill = I("gold"), colour =
                                                                      I("black")))
grafico1 <-
  grafico1 + theme(axis.text.x = element_text(
    size = rel(2),
    angle = 45,
    hjust = 1
  ))
grafico1

# Histograma de palabras frecuentes en el grupo 2

grafico2 <- ggplot(subset(dm2, freq >= 7), aes(word, freq))
grafico2 <-
  grafico2 + geom_bar(stat = "identity", position = "identity", aes(fill = I("gold"), colour =
                                                                      I("black")))
grafico2 <-
  grafico2 + theme(axis.text.x = element_text(
    size = rel(2),
    angle = 45,
    hjust = 1
  ))
grafico2

# Histograma de palabras frecuentes en el grupo 3

grafico3 <- ggplot(subset(dm3, freq >= 20), aes(word, freq))
grafico3 <-
  grafico3 + geom_bar(stat = "identity", position = "identity", aes(fill = I("gold"), colour =
                                                                      I("black")))
grafico3 <-
  grafico3 + theme(axis.text.x = element_text(
    size = rel(2),
    angle = 45,
    hjust = 1
  ))
grafico3

# Histograma de palabras frecuentes en el grupo 4

grafico4 <- ggplot(subset(dm4, freq >= 20), aes(word, freq))
grafico4 <-
  grafico4 + geom_bar(stat = "identity", position = "identity", aes(fill = I("gold"), colour =
                                                                      I("black")))
grafico4 <-
  grafico4 + theme(axis.text.x = element_text(
    size = rel(2),
    angle = 45,
    hjust = 1
  ))
grafico4

##-------------------------------------------------------------------------