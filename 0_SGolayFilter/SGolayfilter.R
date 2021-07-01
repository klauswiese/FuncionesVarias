#Función para filtrado de imágenes usando el algoritmo S. Golay
#Mayo 2019
###############################################################

#Definir directorio de trabajo
#Librerias
library(raster)
rasterOptions(progress = "text")
library(prospectr)
library(parallel)
library(snow)

#Cargar datos
imagen <- stack("prueba2")

# Número de núcleos, dejado dos para otros procesos
no_cores <- detectCores() - 2

# Función Savitzky-Golay para filtrar series de tiempo 
# m = differentiation order,p = polynomial order,w = window size (odd)
# m = orden de diferenciación, p = orden polinomial, w = tamano de ventana (impar)

SpatialSGfilter <- function (TimeSeries, m, p, w, cores=2){
  beginCluster(cores, type = "SOCK") #socket para un sólo ordenador, pues es un 40% más rápido así. Se debe cambiar si se tiene un arreglo de ordenadores
  cl <- getCluster()
  clusterExport(cl, list('TimeSeries', 'm', 'p', 'w'), envir = environment())
  filt <- clusterR(TimeSeries,calc,args=list(fun=function(x) prospectr::savitzkyGolay(x,m,p,w)))
  endCluster()
  return(filt)
}

#prueba de función
ini <- Sys.time()
imagefilt3 <- SpatialSGfilter(TimeSeries=imagen,m=2,p=3,w=5,cores=1)
print(Sys.time() - ini)

#Guardar resultado
writeRaster(imagenfilt, filename="imagenfilt.tif")

