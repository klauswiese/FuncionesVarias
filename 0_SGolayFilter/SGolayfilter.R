#Función para filtrado de imágenes usando el algoritmo S. Golay
#Mayo 2019
###############################################################

#Definri directorio de trabajo
setwd("~/R/0_SGolayFilter")


#Librerias
library(raster)
rasterOptions(progress = "text")
library(prospectr)
library(parallel)
library(snow)

#Cargar datos
imagen <- stack("prueba2")

# Calculate the number of cores
no_cores <- detectCores() - 2

#Function for filtering time series. Savitzky-Golay. m=differentiation order,p=polynomial order,w=window size (odd).
SpatialSGfilter <- function (TimeSeries, m, p, w, cores=2){
  beginCluster(cores, type = "SOCK")
  cl <- getCluster()
  clusterExport(cl, list('TimeSeries', 'm', 'p', 'w'), envir = environment())
  filt <- clusterR(TimeSeries,calc,args=list(fun=function(x) prospectr::savitzkyGolay(x,m,p,w)))
  endCluster()
  return(filt)
}

ini <- Sys.time()
imagefilt3 <- SpatialSGfilter(TimeSeries=imagen,m=2,p=3,w=5,cores=1)
print(Sys.time() - ini)

writeRaster(imagenfilt, filename="imagenfilt.tif")












