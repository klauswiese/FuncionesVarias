#http://remote-sensing.eu/unsupervised-classification-with-r/
#K. Wiese Octubre 2018
#############################################################

#Definir directorio de trabajo

#Libraries
library(raster)  
library(cluster)
library(randomForest)
library(pryr)

#Cargar imagen
Imagenes <- stack("TIFF/ClipImagenes.grd")
print(object_size(Imagenes))

#Cortar segun extent de Linares
Linares <- shapefile("SHP/Linares.shp")
Linares <- spTransform(Linares, CRSobj = crs(Imagenes))

## returns the values of the raster dataset and write them in a matrix. 
v <- getValues(Imagenes)
print(object_size(v))
#i <- which(!is.na(v))
v <- na.omit(v)
print(object_size(v))
mem_used()

#The first classification method is the well-known k-means method. It separates n 
#observations into  k clusters. Each observation belongs to the cluster with the 
#nearest mean.

## kmeans classification 
E <- kmeans(v, 12, iter.max = 10, nstart = 10)
kmeans_raster <- raster(image)
kmeans_raster[i] <- E$cluster
plot(kmeans_raster)

#The second classification method is called clara (Clustering for Large Applications). 
#It work by clustering only a sample of the dataset and then assigns all object in the 
#dataset to the clusters.

## clara classification 
clus <- clara(v,4,samples=500,metric="manhattan",pamLike=T)
clara_raster <- raster(Imagen)
clara_raster[i] <- clus$clustering
plot(clara_raster)


## unsupervised randomForest classification using kmeans
vx <- v[sample(nrow(v), 500),]
rf <- randomForest(vx)
rf_prox <- randomForest(vx,ntree = 1000, proximity = TRUE)$proximity

E_rf <- kmeans(rf_prox, 12, iter.max = 100, nstart = 10)
rf <- randomForest(vx,as.factor(E_rf$cluster),ntree = 500)
rf_raster<- predict(Imagen,rf)
plot(rf_raster)
