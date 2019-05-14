#Sismos USGS https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php
#https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv
#Datos para todo el mes 
#Mayo 2017
#########################################################################

#Definir dirección de trabajo
#setwd('~/Dropbox/R/SHINY/Sismos-USGS/')

#Librerias
library(stringr)
library(sp)
library(rvest)
library(shiny)
library(leaflet)
library(RColorBrewer)

#Extraer tabla de página INETER
sismos <- read.csv("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv")
names(sismos) <- c("Fecha.Local", "lat", "long", "Profundidad", "Magnitud", "TipoMag",
                           "nst", "gap", "dmin", "rms", "net", "id", "updated", "Lugar", "type",
                           "horizontalError", "depthError", "magError", "madNst", "status",
                           "locationSource", "magSource")

#ultimo
ultimos_sismosT <- sismos[,c(1:6, 14, 21)]
#ultimos_sismos <- sismos[,c(1:5, 14)]
ultimos_sismos <- subset(ultimos_sismosT, lat >= 10 & lat <= 18)#lat >= 12.37 & lat <= 16.897
ultimos_sismos <- subset(ultimos_sismos, long >= -92 & long <= -80)#long >= -89.51 & long <= -82.51

#Datos espaciales
ultimos_sismos_sp <- na.omit(ultimos_sismos)
coordinates(ultimos_sismos_sp) <- c("lat", "long")

