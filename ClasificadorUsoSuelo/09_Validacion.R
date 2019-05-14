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
MET_sf <- st_read("SHP/RF7clean5000/RF7clean5000.shp")
MET_sf <- filter(MET_sf, DN != -2147483648)

PuntosVal <- st_transform(st_read("SHP/0_Validacion/RabdomSample1000.gpkg.shp"), crs=crs(MET_sf))

#Graficar
cols <- c("orange", "darkorange" , 'blue' ,"dark green" ,
          "green", "seagreen", "darkseagreen", "limegreen", "lemonchiffon" , 'brown', 'red' )

tm_shape(MET_sf) + tm_polygons("DN", border.alpha = .5, palette="Reds", legend.show = FALSE,
                               max.categories = 11) + #, palette=cols
  tm_format("World", title="Cobertura Vegetal y Uso del Suelo \n Linares, Nuevo México") #, col = cols


#Union puntos de validación y clasificación
DatosVal <- st_join(PuntosVal, MET_sf, join = st_intersects) %>%
  select(c("class", "DN")) %>% na.omit()

matrizConfucion <- data.frame(matrix(table(DatosVal$class,DatosVal$DN), nrow = 11, ncol = 11))

#Clasificación
Clasificacion <- c("Agricultura I",
                   "Agricultura II",
                   "Agua",
                   "Bosque I",
                   "Galeria",
                   "Humedal",
                   "Matorral",
                   "MET",
                   "MET II",
                   "Suelo Desnudo",
                   "Urbano")

names(matrizConfucion) <- Clasificacion
row.names(matrizConfucion) <- Clasificacion

matrizConfucion

confusionMatrix(factor(DatosVal$class), factor(DatosVal$DN))
