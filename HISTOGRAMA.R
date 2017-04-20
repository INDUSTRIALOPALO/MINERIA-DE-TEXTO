################# HISTOGRAMAS #############################
## Nombre: ANÁLISIS DE HISTOGRAMA GENERAL
##-------------------------------------------------------------------------
## Objetivo: siguiente ejemplo muestra la búsqueda realizada en los dos meses,
## al usuario vanguardiacom en los últimos 3160 tweets, sacando información
## de frecuencias tanto de Retweets como favoritos
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
library(readr)
##--------------------------------------------------------------------------

#' paso 1: importar los datos de csv
vanguardia <-
  read_delim("~/vanguardia.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)
View(vanguardia)

vacios <- is.na(vanguardia$favorited)
vanguardia <- vanguardia[!vacios, ]

##-----------------------------------------------------------------------------

#' paso 2: Se obtienen las graficas de retweets y favoritos de los dos meses estudiados
grafica <- ggplot(data = vanguardia, mapping = aes(x = retweetCount))
grafica <-
  grafica + geom_bar(fill = I("blue")) + xlab("Retweets") + ylab("Cuenta") +
  ggtitle("Análisis de Retweets")
grafica

grafica2 <-
  ggplot(data = vanguardia, mapping = aes(x = favoriteCount))
grafica2 <-
  grafica2 + geom_bar(fill = I("blue")) + xlab("Favoritos") + ylab("Cuenta") +
  ggtitle("Análisis de Favoritos")
grafica2