#Advanced Raster Analysis
#https://geoscripting-wur.github.io/AdvancedRasterAnalysis/
#http://amsantac.co/blog/en/2016/10/22/model-stacking-classification-r.html
#Linares, México para ver el Matorral Espinoso Tamaulipeco
#K. Wiese 10 de Octubre 2018
############################################################################
#.rs.restartR()

#Definir directorio de trabajo
setwd("~/R/0_Linares/")

## Librerias
library(raster)
library(pryr)

#Configurar raster
#rasterOptions(tmpdir = "~/R", chunksize = 1e+09, maxmemory = 4e+09)

#Cargar capas
covs <- stack("TIFF/Variables.tif")
#covs <- covs[[c(3:5,7:13)]]
print(object_size(covs))


# #RStoolbox
# install.packages("RStoolbox")
# library(RStoolbox)
# 
# #PCA
# covsPCA <- rasterPCA(covs)

#Cortar segun extent de el Chile
Linares <- shapefile("SHP/Linares.shp")
Linares <- spTransform(Linares, CRSobj = crs(covs))

## Cargar polígonos de entrenamiento
Entrenamiento <- shapefile("SHP/Entrenamiento7.shp")
Entrenamiento <- spTransform(Entrenamiento, CRSobj = crs(covs))

print(object_size(Entrenamiento))
mem_used()

## Inspeccionar los datos del Entrenamiento 
Entrenamiento@data
Entrenamiento$clase <- factor(Entrenamiento$class)

# La columna clase esta ordenada con un factor
Entrenamiento@data$clase
#Levels: Agua, Urbano, Suelo Desnudo, Bosque I, Agricultura I, MET,
# Agricultura II, Galeria, MET II
print(object_size(Entrenamiento))
mem_used()

str(Entrenamiento@data$clase)
#  Factor w/ 10 levels "Agricultura I",..: 3 10 10 10 10 10 10 10 10 10 ...

# Podemos convertir a entero usando la función as.numeric(), 
# la cual toma los levels del factor 
Entrenamiento@data$Code <- as.numeric(Entrenamiento@data$clase)
Entrenamiento@data

mem_used()

## Asiganr el valor de código a las celdas del raster (Donde se sobre pongan)
classes <- rasterize(Entrenamiento, covs[[5]], field='Code', progress="text")
print(object_size(classes))

mem_used()
writeRaster(classes, file="TIFF/classes11", overwrite=TRUE)
mem_used()

## Graficas
# Definir la escala de color para las clases
# Correspondientes a: "Agricultura" "Latifoliado" "Matorral" "Pino" "Suelo"
cols <- c("orange", "darkorange" , 'blue' ,"dark green" ,
          "green", "darkseagreen", "seagreen" ,"limegreen", "lemonchiffon" , 'brown', 'red' )

#cargar datos previos
#covs <- stack("TIFF/Variables.tif")
#classes <- stack("TIFF/classes.grd")
#covmasked <- crop(covs, Entrenamiento)
covmasked <- mask(covs, classes)#classes)#Muy grande 5.16 Gb
print(object_size(covmasked))
mem_used()

#Remove some images to get more usable ram
remove(covs)
remove(classes)
mem_used()

#Export covmasked
writeRaster(covmasked, file="TIFF/covmasked", overwrite=TRUE)

