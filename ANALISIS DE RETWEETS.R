################# RETWEETS ################################
## Nombre: ANÁLISIS DE CLUSTERS
##-------------------------------------------------------------------------
## Objetivo: siguiente ejemplo muestra la búsqueda realizada en los dos meses,
## al usuario vanguardiacom en los últimos 3160 tweets, sacando información
## relevante de cada uno de los grupos obtenidos en cuanto a los retweets
############################################################

##-------------------------------------------------------------------------
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
##-------------------------------------------------------------------------

#' paso 1: Se importarlos datos de csv
vanguardia <-
  read_delim("~/vanguardia.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)
View(vanguardia)

vacios <- is.na(vanguardia$favorited)
vanguardia <- vanguardia[!vacios, ]

##-------------------------------------------------------------------------

#' paso 2: importar res$clustering de csv
clustering <- read_csv("~/clustering.csv")
View(clustering)

##-------------------------------------------------------------------------

#' paso 3: Se selecciona los retweets de cada uno de los clustering
selected1 <- which(vanguardia$retweetCount[clustering == 1] >= 20)
selected2 <- which(vanguardia$retweetCount[clustering == 2] >= 20)
selected3 <- which(vanguardia$retweetCount[clustering == 3] >= 15)
selected4 <- which(vanguardia$retweetCount[clustering == 4] >= 15)

##-------------------------------------------------------------------------

#' paso 4: Se obtiene las graficas de retweets de cada uno de los clustering

# plot clustering 1

dates <- as.Date(vanguardia$created[clustering == 1], "%d/%m/%Y")

plot(
  x = dates,
  y = vanguardia$retweetCount[clustering == 1],
  type = "l",
  col = "grey",
  
  xlab = "Date",
  ylab = "Times retweeted"
)

colors <- rainbow(10)[1:length(selected1)]

points(dates[selected1],
       vanguardia$retweetCount[clustering == 1][selected1],
       
       pch = 19,
       col = colors)

text(
  dates[selected1],
  vanguardia$retweetCount[clustering == 1][selected1],
  
  vanguardia$text[clustering == 1][selected1],
  col = colors,
  cex = .9
)


# plot clustering 2

dates <-
  as.Date(vanguardia$created[clustering == 2], format = "%d/%m/%y")

plot(
  x = dates,
  y = vanguardia$retweetCount[clustering == 2],
  type = "l",
  col = "grey",
  
  xlab = "Date",
  ylab = "Times retweeted"
)

colors <- rainbow(10)[1:length(selected2)]

points(dates[selected2],
       vanguardia$retweetCount[clustering == 2][selected2],
       
       pch = 19,
       col = colors)



# plot clustering 3

dates <-
  as.Date(vanguardia$created[clustering == 3], format = "%d/%m/%y")

plot(
  x = dates,
  y = vanguardia$retweetCount[clustering == 3],
  type = "l",
  col = "grey",
  
  xlab = "Date",
  ylab = "Times retweeted"
)

colors <- rainbow(10)[1:length(selected3)]

points(dates[selected3],
       vanguardia$retweetCount[clustering == 3][selected3],
       
       pch = 19,
       col = colors)




# plot clustering 4

dates <-
  as.Date(vanguardia$created[clustering == 4], format = "%d/%m/%y")

plot(
  x = dates,
  y = vanguardia$retweetCount[clustering == 4],
  type = "l",
  col = "grey",
  
  xlab = "Date",
  ylab = "Times retweeted"
)

colors <- rainbow(10)[1:length(selected4)]

points(dates[selected4],
       vanguardia$retweetCount[clustering == 4][selected4],
       
       pch = 19,
       col = colors)

##-------------------------------------------------------------------------



