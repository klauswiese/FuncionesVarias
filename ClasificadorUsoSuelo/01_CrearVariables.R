#Advanced Raster Analysis
#https://geoscripting-wur.github.io/AdvancedRasterAnalysis/
#http://amsantac.co/blog/en/2016/10/22/model-stacking-classification-r.html
#Linares, México para ver el Matorral Espinoso Tamaulipeco
#K. Wiese 10 de Octubre 2018
############################################################################

#Definir directorio de trabajo

#Crear carpetas para almacenar resulatdos
if(file.exists("CSV") == FALSE) dir.create("CSV")
if(file.exists("PNG") == FALSE) dir.create("PNG")
if(file.exists("TIFF") == FALSE) dir.create("TIFF")

rm(list=ls())

## Librerias
library(raster)
library(pryr)
library(AngleIndexes)

#Cargar capas
mem_used()
Imagenes <- stack("TIFF/ClipS2.tif")#stack("Sentinel/Sentinel10m.grd") #
names(Imagenes) <- c("B2", "B3", "B4", "B8")
B5 <- raster("TIFF/S2_B5.grd")
#SWIR <- raster('TIFF/SWIR_11') #No mejora la clasificación y además agrega errores en la clasificacion del agua
AR <- raster("TIFF/S2_AR.grd")
AR2 <- ARsentinel(Imagenes[[2]], Imagenes[[3]], Imagenes[[4]])
ANIR2 <- ANIR#raster("TIFF/S2_ANIR2.grd") 



VCFlinares <- raster("TIFF/VCFlinares.grd")
NDVI <- raster("TIFF/ndvi.tif")
RBI <- raster("TIFF/RBI.tif")
VV <- raster("TIFF/S1_VV_Linares_utm.tif")
VH <- raster("TIFF/S1_VH_Linares_utm.tif")
MDE <- raster("TIFF/MDEsrtm10m.grd")
Slope <- raster("TIFF/Slope10m.tif")
Aspect <- raster("TIFF/Aspect10m.tif")

#Cortar segun extent de Linares
Linares <- shapefile("SHP/Linares.shp")
Linares <- spTransform(Linares, CRSobj = crs(Imagenes))

#Add VCF raster to stack 
covs <- addLayer(Imagenes, VCFlinares)
names(covs[[5]]) <- "VCF"

#Add NDVI raster to stack
covs <- addLayer(covs, NDVI)
names(covs[[6]]) <- "NDVI"

#Add RBI raster to stack
covs <- addLayer(covs, RBI)
names(covs[[7]]) <- "RBI"

#Add SAR C VV raster to stack
covs <- addLayer(covs, VV)
names(covs[[8]]) <- "VV"

#Add SAR C VH raster to stack
covs <- addLayer(covs, VH)
names(covs[[9]]) <- "VH"

#Add B5 vegetation red edge raster to stack 
covs <- addLayer(covs, B5)
names(covs[[10]]) <- "B5"

#Add AR raster to stack 
covs <- addLayer(covs, AR)
names(covs[[11]]) <- "AR"

#Add ANIR2 raster to stack 
covs <- addLayer(covs, ANIR2)
names(covs[[12]]) <- "ANIR2"

#Add MDE raster to stack 
covs <- addLayer(covs, MDE)
names(covs[[13]]) <- "MDE"

#Add Slope raster to stack 
covs <- addLayer(covs, Slope)
names(covs[[14]]) <- "Slope"

#Add MDE raster to stack 
covs <- addLayer(covs, Aspect)
names(covs[[15]]) <- "Aspect"

#Nombres de la variables
Vari <- names(covs)

#Cargar capa MET
MET <- shapefile("SHP/MET.shp")

#Cortar NDVI con MET
NDVIcrop <- crop(covs[[6]], MET)
NDVImask <- mask(NDVIcrop, MET)
writeRaster(NDVImask, file="TIFF/NDVImaskMET.tif")

#Eliminar capa de imágenes
remove(Imagenes)

#Sumario de imágenes
summary(covs)

#               B2       B3          B4       B8         VCF          NDVI          RBI
# Min.          20       21       20.00       78        0.00 -6.772047e-01 2.155555e-01
# 1st Qu.      307      584      439.00     2086        8.25  2.994210e-01 3.057533e+00
# Median       489      749      770.00     2372       19.50  4.626407e-01 4.337618e+00
# 3rd Qu.      754     1052     1244.75     2715       43.25  6.945715e-01 7.878642e+00
# Max.        5785     5691     5730.00     6023      200.00  9.173789e-01 2.965000e+01
# NA's    28512504 28512504 28512504.00 28512504 28437236.00  2.851250e+07 2.851250e+07
#                    VV            VH           B5           AR        ANIR2          MDE
# Min.    -1.877280e+01 -2.759148e+01 7.818750e+01 2.335387e-01 2.752429e-01 0.000000e+00
# 1st Qu. -1.139143e+01 -1.705795e+01 1.023625e+03 7.416811e-01 1.168126e+00 2.717513e+02
# Median  -1.022566e+01 -1.547356e+01 1.253812e+03 1.828504e+00 2.204541e+00 3.497406e+02
# 3rd Qu. -9.488216e+00 -1.461565e+01 1.650812e+03 2.799210e+00 2.668046e+00 5.365004e+02
# Max.    -2.404492e-10 -3.044015e+00 5.035562e+03 3.141584e+00 3.141486e+00 2.414921e+03
# NA's     2.847756e+07  2.847756e+07 2.851250e+07 2.851250e+07 2.851250e+07 2.851250e+07
#                Slope       Aspect
# Min.    0.000000e+00 0.000000e+00
# 1st Qu. 2.978617e+00 7.867033e+01
# Median  5.146623e+00 1.766899e+02
# 3rd Qu. 1.190348e+01 2.746562e+02
# Max.    7.376750e+03 3.600000e+02
# NA's    2.856734e+07 2.856734e+07

# Histogramas de 9 variables (B,G,R,NIR, VCF, NDVI, RBI, VV, VH)
png("PNG/1_HistogramaBandas.png", width=1000)
hist(covs, col="red")
dev.off()

#Relación entre variables
png("PNG/2_RelacionVariables.png", width=2500, height =2500)
pairs(covs)
dev.off()

png("PNG/4_Variables.png",width=750)
plot(covs, main="Variables")
dev.off()

#Guardar Variables
writeRaster(covs, file="TIFF/Variables.tif", overwrite=TRUE)

