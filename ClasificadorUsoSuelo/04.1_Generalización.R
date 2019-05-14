#GRASS GIS integration for vectorize and generalize data
#Linares, México para ver el Matorral Espinoso Tamaulipeco
#K. Wiese 05 de Diciembre 2018
############################################################################

#Definir directorio de trabajo
setwd("~/R/0_Linares/")

## Librerias
library(raster)
library(pryr)
library(rgrass7)
library(tmap)
library(maptools)
library(sf)
source("Functions/Poligonize.R")

##Load data
METclass <- raster("TIFF/RF_class.tiff_Entrenamiento7_TodasVar.grd")
METclean <- shapefile("SHP/RF7clean5000/RF7clean5000.shp")
#st_read("SHP/RF7clean5000/RF7clean5000.shp")

## Define the function
#ini <- system.time()
#METclassVec <- gdal_polygonizeR(METclass)
#system.time() - ini

#Convertir a objeto sf
#METVec <- st_as_sf(METclassVec)
#st_write(METVec, "SHP/METVec.shp")
METVec <- shapefile("SHP/METVec.shp")#1.35 GB
object_size(METVec)

#Set GRASS GIS for process within R
GRASS <- "/usr/lib/grass74" #Grass location in this computer
stopifnot(file.info(GRASS)$isdir[1]) #test to presence of GRASS GIS
GRASS

#Variables to set GRASS GIS
td <- tempdir()
MET <- "~/GRASS/MET/"
SG <- Sobj_SpatialGrid(METVec)$SG #Maptools
class(SG)

## ----init, echo = TRUE, mysize=TRUE, size='\\tiny'-----------------------
#soho <- initGRASS(gisBase=GRASS, home=td, SG=SG, override=TRUE)
METgrass <- rgrass7::initGRASS(GRASS, home = td, 
                               mapset = 'PERMANENT', 
                               SG=SG, 
                               override = TRUE)
METgrass

## Set Projection
MAPSET <- execGRASS("g.mapset", flags="p", intern=TRUE)
MAPSET
execGRASS("g.mapset", mapset="PERMANENT", flags="quiet")
execGRASS("g.proj", flags=c("p", "quiet"))
execGRASS("g.proj", proj4=proj4string(METVec), flags=c("c", "quiet"))

## Projection Info
execGRASS("g.proj", flags=c("p", "quiet"))
execGRASS("g.mapset", mapset=MAPSET, flags="quiet")

## Set resolution
execGRASS("g.region", flags="p", intern=TRUE)#[3:11]
execGRASS("g.region", flags="a", res="10")
execGRASS("g.region", flags="p", intern=TRUE)#[3:11]

## ----ogr, echo = TRUE, mysize=TRUE, size='\\tiny'------------------------
fl <- c("overwrite", "quiet")
writeVECT(METVec, vname="METVec", v.in.ogr_flags=fl)#"bbo" Duro casi una hora

# v.clean input=RF_class_entrenamiento7_v4_sin1_2_6@PERMANENT 
# output=RF_class_5000 
# tool=rmarea 
# thres=5000 
# type=point,line,area

#Limpiar los polínos menores a media hectárea
clean <- execGRASS("v.clean", input="METVec" ,tool="rmarea", threshold=5000, 
                   type="area", output="METs")#"bbo"


#Exportar resultados
execGRASS("v.out.ogr", input="METs", output="/home/wiese/R/0_Linares/SHP/METcleanR.gpkg", format="GPKG")
