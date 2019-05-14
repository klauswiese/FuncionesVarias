#Landscape ecology with R
#http://jwhollister.com/r_landscape_tutorial/tutorial.html
#https://rpubs.com/dgolicher/9458
#https://www.rforge.net/doc/packages/SDMTools/ClassStat.html
#K. Wiese Septiembre 2018
#---------------------------------------------------------

#definir dirección de trabajo
setwd("~/R/0_Linares/")

#Librerias
rm(list = ls())
library(rgdal)
library(rgeos)
library(spdep)
library(raster)
library(SDMTools)
library(tmap)
library(sf)

#Cargar capas
MET <- shapefile("SHP/MET.shp")
MET_sf <- st_read("SHP/MET.shp")
dim(MET_sf)
#Simple feature collection with 3924 features and 2 fields

#plot with tmap
#tm_shape(nf_sf)  + tm_polygons("cat") + tm_borders()

#1. Área y perímetro
####################
#Area
areas <- gArea(MET, byid = T)

#Perimetro
edge <- gBoundary(MET, byid = T)
perims <- gLength(edge, byid = T)

#Unir en data frame
d <- data.frame(id = names(areas), areas, perims)
d <- d[order(d$areas, decreasing = T), ]
head(d)

#2. Exploración de datos
########################

#Histogramas
hist(d$areas, col = "grey")#Crudo
hist(log10(d$areas), col = "grey")#Transformado

#Sumarios
summary(d$perims)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 320     620     880    2720    1640  823700  

summary(d$areas)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 5100     8000    14200   140197    37100 95411400 

summary(d$areas/10000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.51    0.80    1.42   14.02    3.71 9541.14 

#3. Calculado métricas de forma
###############################
d$shape <- d$perims/(2 * pi * sqrt(d$areas/pi))
head(d)

#4. Core area metrics
#####################
cores <- gBuffer(MET, width = -100, byid = TRUE)

#4.1 Cores statistics
c_area <- gArea(cores, byid = T)
c_area <- data.frame(id = names(c_area), c_area)
d <- merge(d, c_area, all = T)
d$corepercent <- (d$c_area/d$areas) * 100
d$edgepercent <- 100 - d$corepercent
head(d)

#dir.create("CSV")
write.csv(d, file="CSV/MET_Metricas.csv")

#5. Analizando conectividad
###########################
mat <- gDistance(MET, byid = TRUE)
write.csv(mat, file="CSV/MET_MatrizDistancia.csv")

#Definir distancia crítica
##########################
#If we set the distance for movement between patches to 1km we can look at the network of 
#linked patches.

centroids <- coordinates(MET)
mat[mat > 1000] <- 0
aa <- mat2listw(mat)
sink("TXT/MatrixFragResults.txt")
str(summary(aa, zero.policy = T))
sink()

dir.create("PNG")
png("PNG/MET_Conectividad.png", width=750)
plot(MET, col = "darkgreen", main="Conectividad Matorral Espinoso Tamaulipeco \n Linares, Nuevo León")
axis(1)
axis(2)
axis(4)
box()
grid()
plot(aa, centroids, add = T, pch = 21, cex = 0.2)
dev.off()

#RASTER
#######

X <- (extent(MET)[2] - extent(MET)[1])/10
Y <- (extent(MET)[4] - extent(MET)[3])/10

b <- bbox(MET)
r <- raster(nrow = Y, ncol = X, xmn = b[1, 1], xmx = b[1, 2], ymn = b[2,1], ymx = b[2, 2])
rMET <- rasterize(MET, r)

dir.create("TIFF")
writeRaster(rMET, file = "TIFF/METrasterFrag.tif", overwrite=TRUE)
j <- raster("TIFF/METrasterFrag.tif")
plot(j)
plot(MET, add=T)
png("PNG/PoligonosRasterMET.png", width=1000)
plot(rMET)
dev.off()

#Reclasificar cada polígono
m <- c(1, 3924, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(rMET, rclmat)

#Análisis de parchos con SDMTools
ps.data <- PatchStat(rc)
ps.dataRMET <- PatchStat(rMET)#Correcto con una sola clase
cl.data <- ClassStat(rc)#Correcto con una clase y muchos parchos
cl.dataRMET <- ClassStat(rMET)


write.csv(ps.data, file="CSV/EstParchosMET.csv")
write.csv(cl.data, file="CSV/EstClaseMET.csv")
write.csv(ps.dataRMET, file="CSV/EstParchosMET_todosPoly.csv")
write.csv(cl.dataRMET, file="CSV/EstClaseMET_todosPoly.csv")

#Plot with tmap
###############
data(World, land, metro)

pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")
tm_shape(land, ylim = c(-88,88)) +
  tm_raster("cover_cls", palette = pal8, title = "Global Land Cover") 


#RAO
RAO <- raster("TIFF/RAO.tif")
rao <- tm_shape(RAO) +
  tm_raster("RAO", palette = "Greens", title = "RAO")
tmap_save(rao, "PNG/Rao.png")

#SHANNON
Shannon <- raster("TIFF/SHANNON.tif")
SHA <- tm_shape(Shannon) +
  tm_raster("SHANNON", palette = "Reds", title = "Shannon")
tmap_save(SHA, "PNG/Shannon.png")

