#Crear Indices Espectrales con imágenes Sentinel 2
#K. Wiese Madrid, 14 de febrero 2019
##################################################

#Definir directorio de trabajo
setwd("~/R/0_Linares/")
if(file.exists("Sentinel") == FALSE) dir.create("Sentinel")#Carpeta para guardar resultados

#libraries
library(raster)
library(sf)
source("~/R/SIG_Medina/Functions/Indices.R")#redefinir dirección

#Área de trabajo
Ainteres <- st_read("SHP/Linares32614.shp")

# map the location
tmap_mode("view")
tm_shape(Ainteres) + tm_borders(lwd=2, col="darkgreen") + tm_basemap("Esri.WorldTopoMap")

#Cargar imagen
Sentinela <- stack("Sentinel/Sentinel10m.grd")/10000

#Cálculo de índices
AVI <- AVIsentinel(Sentinela[[3]], Sentinela[[4]])#AVI
BSI <- BSIsentinel(Sentinela[[1]], Sentinela[[3]], Sentinela[[4]], Sentinela[[9]])#BSI
NDMI <- NDMIsentinel(Sentinela[[4]], Sentinela[[9]])#NDMI
NDVI <- NDVI(Sentinela[[3]], Sentinela[[4]])#NDVI
NDWI <- NDWI(Sentinela[[2]], Sentinela[[4]])#NDWI
NPCRI <- NPCRIsentinel(Sentinela[[1]], Sentinela[[3]])#NPCRI
RBI <- overlay(Sentinela[[1]], Sentinela[[4]], fun=function(x,y) (y/x))#NIR/BLUE

#Unir Índices en un stack
IndicesEspectrales <- stack(AVI, BSI, NDMI, NDVI, NDWI, NPCRI, RBI)
names(IndicesEspectrales) <- c("AVI", "BSI", "NDMI", "NDVI", "NDWI", "NPCRI", "RBI")

#Guardar resultados
writeRaster(IndicesEspectrales, file="Sentinel/IndicesEspectrales", overwrite=TRUE)
plot(IndicesEspectrales)

#Guardar el NDVI
#writeRaster(NDVI, file="Sentinel/NDVI.tif")
