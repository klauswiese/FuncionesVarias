#OTSU algorithm
#Auto thresholding
#K. Wiese 23 de abril 2019
#https://stackoverflow.com/questions/51116495/auto-thresholding-on-r-raster-object
##################################################################################

#set working directory
setwd("~/R/OTSU/")

#Library
library(raster)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage", version = "3.8")
library(EBImage)

#Data
NDVI <- raster("~/R/0_Linares2/TIFF/NDVIfraccion.tif")
range <- range(NDVI[])  # assuming values in the matrix range from 0 to 1
levels <- 256L
breaks <- seq(range[1], range[2], length.out = levels + 1)
h <- hist.default(NDVI[], breaks = breaks, plot = FALSE)
counts <- as.double(h$counts)
mids <- as.double(h$mids)
len <- length(counts)
w1 <- cumsum(counts)
w2 <- w1[len] + counts - w1
cm <- counts * mids
m1 <- cumsum(cm)
m2 <- m1[len] + cm - m1
var <- w1 * w2 * (m2/w2 - m1/w1)^2
maxi <- which(var == max(var, na.rm = TRUE))
(mids[maxi[1]] + mids[maxi[length(maxi)]])/2

#Gráfico
par(mfrow=c(2,1))
plot(NDVI > 0.4884159)
plot(NDVI)
