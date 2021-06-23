#Advanced Raster Analysis Mixed with "Hanna Meyer" Geostat 2018 Work
#Linares, México para ver el Matorral Espinoso Tamaulipeco
#K. Wiese 10 Diciembre 2018
############################################################################

#Definir directorio de trabajo
setwd("~/R/0_Linares/")

rm(list=ls())
## Librerias
library(raster)
library(pryr)
library(caret)
library(mapview)
library(sf)
library(doParallel)

## Graficas
# Definir la escala de color para las clases
# Correspondientes a: "Agricultura" "Latifoliado" "Matorral" "Pino" "Suelo"
cols <- c("orange", "darkorange" , 'blue' ,"dark green" ,
          "green", "seagreen", "darkseagreen", "limegreen", "lemonchiffon" , 'brown', 'red' )

## Add a customized legend
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


#Cargar datos
Linares <- shapefile("SHP/Linares.shp")
covmasked <- raster("TIFF/Variables.tif")
Linares <- spTransform(Linares, CRSobj = crs(covmasked))

#Salvar los valores que usaremos en el entrenamiento de Random Forest
valuetable <- read.csv("CSV/Previous/ValueTable_entrenamiento7_TodasVar.csv")
valuetable <- valuetable[,-1]
# data.frame':	249974 obs. of  16 variables:
# $ B2    : int  808 870 837 781 728 700 646 859 777 661 ...
# $ B3    : int  1005 1098 1088 1046 924 862 861 1088 1015 884 ...
# $ B4    : int  1360 1435 1425 1369 1251 1205 1178 1392 1336 1208 ...
# $ B8    : int  2013 2049 2086 2010 1927 1912 1897 2021 2012 1924 ...
# $ VCF   : num  21.9 24.3 26.8 29 30.9 ...
# $ NDVI  : num  0.194 0.176 0.188 0.19 0.213 ...
# $ RBI   : num  2.49 2.36 2.49 2.57 2.65 ...
# $ VV    : num  -13.3 -13.7 -13.7 -13.3 -12.8 ...
# $ VH    : num  -21.3 -20.9 -20.3 -20.2 -19.5 ...
# $ B5    : num  1619 1652 1654 1627 1590 ...
# $ AR    : num  3.11 3.11 3.09 3.09 3.08 ...
# $ ANIR2 : num  3.03 3.04 3.01 3.03 3.03 ...
# $ MDE   : num  252 251 251 251 251 ...
# $ Slope : num  4.65 5.65 4.02 3.36 3.4 ...
# $ Aspect: num  348 338 310 270 278 ...
# $ class : int  1 1 1 1 1 1 1 1 1 1 ...


#Split data
set.seed(100)
trainids <- createDataPartition(valuetable$class,list=FALSE,p=0.3)
trainDat <- valuetable[trainids,]
testDat <- valuetable[-trainids,]

#Visualize relations
boxplot(trainDat$AR~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$ANIR2~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$B2~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$B3~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$B4~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$B8~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$B5~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$VCF~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$NDVI~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$RBI~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$VV~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$VH~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$B5~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$MDE~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$Slope~trainDat$class,las=2, names=Clasificacion, col=cols)
boxplot(trainDat$Aspect~trainDat$class,las=2, names=Clasificacion, col=cols)

#Scatterplot for varibles
#Spectral data
featurePlot(x = trainDat[, c("B2","B3","B4","B5","B8")], 
            y = factor(trainDat$class), 
            plot = "pairs",
            auto.key = list(columns = 5))

#Angular data
featurePlot(x = trainDat[, c("AR","ANIR2")], 
            y = factor(trainDat$class), 
            plot = "pairs",
            auto.key = list(columns = 2))

#Radar data
featurePlot(x = trainDat[, c("VV","VH")], 
            y = factor(trainDat$class), 
            plot = "pairs",
            auto.key = list(columns = 2))

#Index Data
featurePlot(x = trainDat[, c("VCF","RBI", "NDVI")], 
            y = factor(trainDat$class), 
            plot = "pairs",
            auto.key = list(columns = 3))

#MDE Data
featurePlot(x = trainDat[, c("MDE","Slope", "Aspect")], 
            y = factor(trainDat$class), 
            plot = "pairs",
            auto.key = list(columns = 3))

#Spectral vs Angular vs MDE
featurePlot(x = trainDat[, c("MDE","B8", "AR")], 
            y = factor(trainDat$class), 
            plot = "pairs",
            auto.key = list(columns = 3))

#Modelo
predictors <- c("B2","B3","B4","B5", "B8",
                "VCF", "NDVI", "RBI", 
                "AR", "ANIR2",
                "VV", "VH",
                "MDE", "Slope", "Aspect")

response <- "class"

### Random forest model training
#Set to work in various cores
cl <- makePSOCKcluster(5)#Select 5 cores
registerDoParallel(cl)#register the parallel operation

#Training the machine learning model
model <- train(trainDat[,predictors],trainDat[,response],method="rf",
               trControl=trainControl(method="cv"),importance=TRUE)#, allowParallel=TRUE

stopCluster(cl)#11.30 - 16.25

print(model)
# Random Forest 
# 
# 74994 samples
# 15 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 67494, 67494, 67496, 67494, 67494, 67495, ... 
# Resampling results across tuning parameters:
#   
#   mtry  RMSE       Rsquared   MAE      
# 2    0.6704508  0.9619651  0.2852828
# 8    0.5138130  0.9770111  0.1647227
# 15    0.5364342  0.9745020  0.1418243
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was mtry = 8.



#Modelo y validación cruzada
plot(model)

#Importancia de variables por cada clase
plot(varImp(model))

#Predicción
covmasked <- stack("TIFF/Variables.tif")
names(covmasked) <- names(valuetable[1:15])
prediction <- predict(covmasked, model)

#plot de cobertura
spplot(prediction,col.regions=cols, names.attr=Clasificacion)

#Model validation
pred_valid <- predict(model,testDat)
table(testDat$class,pred_valid)
