################# RECOLECCION DE DATOS ####################
## Nombre: TWEETS DEL USUARIO VANGUARDIACOM
##---------------------------------------------------------------
## Objetivo: se presenta un caso de estudio de análisis de texto obteniendo 
## los tweets del usuario de un periódico local a través del API de twitter,
## obteniendo 3160 tweets de meses comprendidos entre 17/01/2017 al 17/03/2017
############################################################

#importar librerias
rm(list = ls())
library(ggplot2)
library(RCurl)
library(twitteR)
library(tm)
library(Rcpp)
library(RColorBrewer)
library(wordcloud)
library(readr)
library(base)
library(cluster)
#fin importar librerias

##---------------------------------------------------------------

#' paso 1: importar datos de twitter

################## paso 2: Se obtiene el acceso a token ##########################
## Al usar el API se autentica con las diferentes “llaves” y “tokens“ 
## de acceso que se generaron al crear la aplicación en Twitter.
## mediante la aplicación save que aporta R se guarda los tokens 
## lo cual se pueden volver a leer en una fecha posterior mediante la función load 
##################################################################################

##---------------------------------------------------------------

load("Autor.R")

#usuario
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token ,
                    access_token_secret)

##---------------------------------------------------------------

#' se obtienen los Tweets del usuario vanguardia liberal
tweets <- userTimeline("vanguardiacom", n = 3164)

##---------------------------------------------------------------

#' se guardar los datos de los tweet en un data frame
df <- twListToDF(tweets)

##---------------------------------------------------------------

#' Se Guardar en csv con el fin de hacer los análisis sin tener que volver a obtner los tweets
write.table(
  x = df,
  file = "vanguardia.csv",
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
getwd()

##---------------------------------------------------------------