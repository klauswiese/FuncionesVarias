#Cortar imágenes Sentinel 2
#K. Wiese Madrid, 14 de febrero 2019
####################################

#Definir directorio de trabajo
setwd("~/R/0_Linares/")
if(file.exists("Sentinel") == FALSE) dir.create("Sentinel")#Carpeta para guardar resultados

#libraries
library(Sentinel2)#paquete desarrollado en UPM
library(sf)
library(tmap)

#Área de trabajo
Ainteres <- st_read("SHP/Linares32614.shp")

# map the location
tmap_mode("view")
tm_shape(Ainteres) + tm_borders(lwd=2, col="darkgreen") + tm_basemap("Esri.WorldTopoMap")

#Leer imagen
Imagen <- list.files(pattern = "L2A", full.names = TRUE)
Sentinela <- readSentinel2(Imagen)
metaSentinela <- readSentinel2Meta(Imagen)

#Separar según resolución espacial
Sentinela10 <- Sentinela@TenRes
Sentinela20 <- Sentinela@TwentyRes

#Cortar cada brick
Sentinela10Crop <- crop(Sentinela10, Ainteres)
Sentinela20Crop <- crop(Sentinela20, Ainteres)

#Resampleo 
Sentinela2 <- projectRaster(Sentinela20Crop, Sentinela10Crop)

#Stack de todas las imágenes
SentinelaTotal <- stack(Sentinela10Crop, Sentinela2)
names(SentinelaTotal) <- c("B2_BLUE", "B3_GREEN", "B4_RED", "B8_NIR", 
                           "B5_VNIR1", "B6_VNIR2", "B7_VNIR3", "B8A_NIR2", "B11_SWIR1", "B12_SWIR2")

#Nombres de las bandas
writeRaster(SentinelaTotal, file="Sentinel/Sentinel10m", overwrite = TRUE)

