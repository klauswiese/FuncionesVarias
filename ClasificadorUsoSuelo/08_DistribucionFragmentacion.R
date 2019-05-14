#Fragmentación MET Linares, Nuevo México, México
#K. Wiese Diciembre 2018
#---------------------------------------------------------

#definir dirección de trabajo
setwd("~/R/0_Linares/")

#Libraries
library(sf)
library(raster)
library(tmap)
library(dplyr)

#Data
#Cargar capas
MET <- shapefile("SHP/MET.shp")
MET_sf <- st_read("SHP/MET.shp")

#Cargar tablas
FragMetrics <- read.csv("CSV/EstParchosMET_todosPoly.csv")
FragMetrics2 <- read.csv("CSV/EstClaseMET_todosPoly.csv")

#Join data Fragments and its metrics
####################################
Fragments <- FragMetrics2 %>% left_join(MET_sf, by = "NAME")
