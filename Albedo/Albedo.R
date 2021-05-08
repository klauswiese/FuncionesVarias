#Code for NDVI calculus using landsat OLI K. Wiese
#http://terradue.github.io/rLandsat8/section3/toarad.html
#########################################################

#Definir directorio de trabajo
setwd("~/R/RFLaTigra/")

#Librerias
library(rgdal) 
library(raster) 
library(rLandsat8) 
library(stringi)

#Cargar capa PNLT
PNLT <- shapefile("~/Dropbox/Gislatigra/Archivos Shp/Area_PNLT_Funez-Payin_WGS84.shp")

Folder <- "~/R/RFLaTigra/LC80180502014103LGN00"
Folder2 <- data.frame(strsplit(Folder, "/"))

#Procesar imagen
product  <- Folder2[dim(Folder2)[1],] #archive name with landsat image
l <- ReadLandsat8(product)


#Radiance
##########
radiance.blue <- ToTOARadiance(landsat8=l, band="blue")
radiance.green <- ToTOARadiance(landsat8=l, band="green")
radiance.red <- ToTOARadiance(landsat8=l, band="red")
radiance.nir <- ToTOARadiance(landsat8=l, band="nir")
radiance.swir1 <- ToTOARadiance(landsat8=l, band="swir1")
radiance.swir2 <- ToTOARadiance(landsat8=l, band="swir2")


#Reflectance
############
reflectance.blue <- ToTOAReflectance(landsat8=l, band="blue")
reflectance.green <- ToTOAReflectance(landsat8=l, band="green")
reflectance.red <- ToTOAReflectance(landsat8=l, band="red")
reflectance.nir <- ToTOAReflectance(landsat8=l, band="nir")
reflectance.swir1 <- ToTOAReflectance(landsat8=l, band="swir1")
reflectance.swir2 <- ToTOAReflectance(landsat8=l, band="swir2")

#Pesos para el cálculo del albedo
#################################
#Obtenidos de http://dx.doi.org/10.1590/1807-1929/agriambi.v20n1p3-8
#Procedures for calculation of the albedo with OLI-Landsat 8 images:
#Application to the Brazilian semi-arid
Blue <- 0.3
Green <- 0.277
Red <- 0.233
NIR <- 0.143
SWIR1 <- 0.036
SWIR2 <- 0.012

#Cálculo de ALBEDO
##################
Albedo <- (Blue*reflectance.blue + Green*reflectance.green + Red*reflectance.red + NIR*reflectance.nir +
  SWIR1*reflectance.swir1 + SWIR2*reflectance.swir2)/sum(Blue, Green, Red, NIR, SWIR1, SWIR2)

#Albedo PNLT
AlbedoPNLT <- crop(Albedo, extent(PNLT))

plot(AlbedoPNLT)
plot(PNLT, add=TRUE)
