
################# EJEMPLO K-means ####################
## Nombre: Optimizar K-Means eligiendo mejor algoritmo
##---------------------------------------------------------------
## Objetivo: comparar las 4 variaciones del algoritmos
##que son: Lloyd, Forgy, MacQueen y Hartigan-Wong.
## donde para compararlas se usa "distancia intracluster", 
##que es la suma de las distancia entre los centroide.
## El algoritmo que tenga la mayor "distancia intracluster" 
## sería el ganador ya que sería la mejor separación de grupos.
######################################################

#' Importar las librerias necesarias 

rm(list = ls())
library(cluster)
library(ggplot2)
library(factoextra)
library(tm)

#------------------------------------------------------------------------

#' PASO 1: Crea datos de ejemplo 
Clientes<-data.frame(
  Nombre=c("Juan","Pedro","Maria", "Isabel","Diego","Luis","Lucia","Francisca",
           "Alberto","Garcia","Soto","Victor","Esteban","Jose","Beto"),
  Edad=c(55,30,80,78,98,87,46,38,49,54,92,28,19,29,46),
  MontoConsumo=c(19,56,11,57,34,72,42,43,23,98,17,24,12,48,56))

#-------------------------------------------------------------------------

#' PASO 2: Crea vector con nombre de algoritmos y tabla vacía para guardar Iteraciones
Algoritmos         <-c("Hartigan-Wong","Lloyd","Forgy","MacQueen") 
CantidadAlgoritmos <-length(Algoritmos) # guarda la cantidad de algoritmos usados
Iteraciones        <-data.frame(Intraclase=numeric(),Algoritmo=character())

#-------------------------------------------------------------------------

#' PASO 3: Ejecuta k-means 10 veces en cada algoritmo
#' y guarda la Distancia Intracluster de cada iteracion en la tabla Iteraciones
for (i in 1:CantidadAlgoritmos) 
{
  for (ii in 1:10) 
  {
    Modelo      <- kmeans(Clientes[2:3],3, algorithm = Algoritmos[i])
    Iteraciones <- rbind(Iteraciones,
                         data.frame(Intraclase = Modelo$betweenss,
                                    Algoritmo = Algoritmos[i]))
  }
}

#-------------------------------------------------------------------------

#' PASO 4: Calcula la media de Distancia Intracluster en cada algoritmo
#'  e identificar Algoritmo Ganador 
Resultados       <- tapply(Iteraciones$Intraclase,Iteraciones$Algoritmo,mean) 
Resultados       <-sort(Resultados,decreasing = T)
AlgoritmoGanador <-names(Resultados[1])

#-------------------------------------------------------------------------

#' PASO 5: Ejecuta kmeans con algoritmo ganador y asigna grupo a cada cliente
KmeansOptimizado <- kmeans(Clientes[2:3],3, algorithm = AlgoritmoGanador)
Clientes$Grupo   <-KmeansOptimizado$cluster

#-------------------------------------------------------------------------

#' PASO 6: Grafica segmentacion de algoritomo ganador y luego asigna etiquetas
plot(Clientes$Edad,Clientes$MontoConsumo,col=Clientes$Grupo,cex.axis=.7,cex.lab=.7)
text(Clientes$Edad,Clientes$MontoConsumo,
     labels=Clientes$Nombre,pos=1,col=Clientes$Grupo,cex=.7)
title(main=paste("Algoritmo ganador:",AlgoritmoGanador),cex.main=.9)