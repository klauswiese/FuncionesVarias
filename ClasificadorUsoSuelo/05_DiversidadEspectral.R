#Diversidad Espectral
#Linares, México para ver el Matorral Espinoso Tamaulipeco
#K. Wiese 29 de Octubre 2018
##########################################################

#Definir directorio de trabajo

#Crear carpetas para almacenar resulatdos
if(file.exists("CSV") == FALSE) dir.create("CSV")
if(file.exists("PNG") == FALSE) dir.create("PNG")
if(file.exists("TIFF") == FALSE) dir.create("TIFF")

rm(list=ls())


#Libraries
library(raster)
source("Functions/SpectralRaoMarzo2019.R")

#Cargar capas
#mem_used()
NDVI <- stack("TIFF/CorteRao.tif")[[6]]

#Spectral RAO
SpectralRAO <- spectralrao(NDVI, 
                           distance_m="euclidean", 
                           window=3, 
                           shannon=TRUE, 
                           rescale=FALSE,
                           debugging=FALSE, 
                           simplify=1, 
                           nc.cores=1)#, cluster.type="MPI"

#Datos calculados antes
RAOv <- raster("TIFF/RAO.tif")
Shannonv <- raster("TIFF/SHANNON.tif")

#Guardar resultados
#RAO entropy
RAO <- NDVI
RAO[] <- SpectralRAO[[1]]
plot(RAO)
writeRaster(RAO, file="TIFF/RAO2.tif")

#Shannon
Shannon <- NDVI
Shannon[] <- SpectralRAO[[2]]
plot(Shannon)
writeRaster(RAO, file="TIFF/Shannon2.tif")

par(mfrow=c(2,2))
plot(RAOv)
plot(RAO)
plot(Shannonv)
plot(Shannon)

par(mfrow=c(2,2))
hist(RAOv)
hist(RAO)
hist(Shannonv)
hist(Shannon)
