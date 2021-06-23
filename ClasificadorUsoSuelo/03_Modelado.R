#Advanced Raster Analysis
#https://geoscripting-wur.github.io/AdvancedRasterAnalysis/
#http://amsantac.co/blog/en/2016/10/22/model-stacking-classification-r.html
#Linares, México para ver el Matorral Espinoso Tamaulipeco
#K. Wiese 10 de Octubre 2018
############################################################################

#Definir directorio de trabajo

## Librerias
library(raster)
library(pryr)

## Graficas
# Definir la escala de color para las clases
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

#Datos que no logre guardar antes
#cargar datos previos
covs <- stack("TIFF/Variables.tif")
covs <- covs[[c(3:5,7:13)]]
classes <- stack("TIFF/classes11.grd")
# 
covmasked <- mask(covs, classes)#Muy grande 4.3 Gb
print(object_size(covmasked))

mem_used()

#Cargar datos
Linares <- shapefile("SHP/Linares.shp")
Linares <- spTransform(Linares, CRSobj = crs(covmasked))

## Combine this new brick with the classes layer to make our input training dataset
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
object_size(trainingbrick)
mem_used()
remove(covmasked)
#plot(trainingbrick)
writeRaster(trainingbrick, file="TIFF/TrainingBrick_entre7_TodasVar", overwrite=TRUE)

#trainingbrick <- stack("TIFF/TrainingBrick.grd")
## Extraer todos los valores en una matriz
valuetable <- getValues(trainingbrick) #Resultado con 5.68 GB
mem_used()

##remove trainigbrick in order to make space for valuetable
remove(trainingbrick)
mem_used()

#Convertir a data frame
valuetable <- as.data.frame(valuetable)
mem_used()

## Eliminar los na
valuetable <- na.omit(valuetable)
#valuetable2 <- round(valuetable, 3)
mem_used()

head(valuetable, n = 10)
valuetable$class <- factor(valuetable$class, levels = c(1:11))

#Ahora tenemos una tabla de referencia que contiene, por cada una de las cinco clases definidas
#todos los valores conocidos ór cada covariable. Visulicemos la distribucipo de algunas de estas
#covariables para cada clase. Para hacer esto más fácil, creemos cinco diferentes data.frames
#uno para cada clase. Esto sólo para poder graficarlos, no usaremos estos en la clasificación
val_agriI <- subset(valuetable, class == 1)
val_agriII <- subset(valuetable, class == 2)
val_agua <- subset(valuetable, class == 3)
val_bosque <- subset(valuetable, class == 4)
val_galeria <- subset(valuetable, class == 5)
val_Humedal <- subset(valuetable, class == 6)
val_Matorral <- subset(valuetable, class == 7)
val_MET <- subset(valuetable, class == 8)
val_METII <- subset(valuetable, class == 9)
val_suelo <- subset(valuetable, class == 10)
val_urbano <- subset(valuetable, class == 11)

## 1. NDVI
 png("PNG/6_ClasesNDVI.png", width = 1000, height = 1000)
 par(mfrow = c(4, 3))
 hist(val_agriI$NDVI, main = "Agricultura I", xlab = "NDVI", col = "orange", xlim = c(-1, 1), ylim=c(0,12000))
 hist(val_agriII$NDVI, main = "Agricultura II", xlab = "NDVI", col = "darkorange", xlim = c(-1, 1), ylim=c(0,8000))
 hist(val_agua$NDVI, main = "Agua", xlab = "NDVI", col = "blue", xlim = c(-1, 1), ylim=c(0,8000))
 hist(val_bosque$NDVI, main = "Bosque Pino y Quercus", xlab = "NDVI", xlim = c(-1, 1),col = "darkgreen", ylim=c(0,12000))
 hist(val_galeria$NDVI, main = "Bosque de Galeria", xlab = "NDVI", col = "green", xlim = c(-1, 1), ylim=c(0,1000))
 hist(val_Humedal$NDVI, main = "Humedal", xlab = "NDVI", xlim = c(-1, 1), col=cols[6], ylim=c(0,2000))
 hist(val_Matorral$NDVI, main = "Matorral Premontano", xlab = "NDVI", xlim = c(-1, 1), col=cols[7], ylim=c(0,2000))
 hist(val_MET$NDVI, main = "Matorral Espinoso Tamaulipeco", xlab = "NDVI", col = "springgreen1", xlim = c(-1, 1), ylim=c(0,8000))
 hist(val_METII$NDVI, main = "Matorral Espinoso Tamaulipeco II", xlab = "NDVI", col = "yellowgreen", xlim = c(-1, 1), ylim=c(0,8000))
 hist(val_suelo$NDVI, main = "Suelo Desnudo", xlab = "NDVI", xlim = c(-1, 1),col = "gray", ylim=c(0,8000))
 hist(val_urbano$NDVI, main = "Urbano", xlab = "NDVI", xlim = c(-1, 1), col = "red", ylim=c(0,2000))
 dev.off()

 ## 2. VCF
 png("PNG/7_ClasesVCF.png", width = 1000, height = 1000)
 par(mfrow = c(4, 3))
 hist(val_agriI$VCF, main = "Agricultura I", xlab = "VCF", col = "orange", xlim = c(0, 100), ylim=c(0,12000))
 hist(val_agriII$VCF, main = "Agricultura II", xlab = "VCF", col = "darkorange", xlim = c(0, 100), ylim=c(0,8000))
 hist(val_agua$VCF, main = "Agua", xlab = "VCF", col = "blue", xlim = c(0, 100), ylim=c(0,8000))
 hist(val_bosque$VCF, main = "Bosque Pino y Quercus", xlab = "VCF", xlim = c(0, 100),col = "darkgreen", ylim=c(0,12000))
 hist(val_galeria$VCF, main = "Bosque de Galeria", xlab = "VCF", col = "green", xlim = c(0, 100), ylim=c(0,1000))
 hist(val_Humedal$VCF, main = "Humedal", xlab = "VCF", xlim = c(0, 100), col=cols[6], ylim=c(0,2000))
 hist(val_Matorral$VCF, main = "Matorral Premontano", xlab = "VCF", xlim = c(0, 100), col=cols[7], ylim=c(0,2000))
 hist(val_MET$VCF, main = "Matorral Espinoso Tamaulipeco", xlab = "VCF", col = "springgreen1", xlim = c(0, 100), ylim=c(0,8000))
 hist(val_METII$VCF, main = "Matorral Espinoso Tamaulipeco II", xlab = "VCF", col = "yellowgreen", xlim = c(0, 100), ylim=c(0,8000))
 hist(val_suelo$VCF, main = "Suelo Desnudo", xlab = "VCF", xlim = c(0, 100),col = "gray", ylim=c(0,8000))
 hist(val_urbano$VCF, main = "Urbano", xlab = "VCF", xlim = c(0, 100), col = "red", ylim=c(0,2000))
 dev.off()
 
 
 ## 1. VV
 png("PNG/6.2_ClasesVV.png", width = 750, height = 750)
 par(mfrow = c(3, 3))
 hist(val_agriI$Variables.8, main = "Agricultura I", xlab = "Radar VV", col = "orange", xlim = c(-20, 0), ylim=c(0,12000))
 hist(val_agriII$Variables.8, main = "Agricultura II", xlab = "Radar VV", col = "darkorange", xlim = c(-20, 0), ylim=c(0,8000))
 hist(val_agua$Variables.8, main = "Agua", xlab = "Radar VV", col = "blue", xlim = c(-20, 0), ylim=c(0,1000))
 hist(val_bosque$Variables.8, main = "Bosque Pino y Quercus", xlab = "Radar VV", xlim = c(-20, 0),col = "darkgreen", ylim=c(0,18000))
 hist(val_galeria$Variables.8, main = "Bosque de Galeria", xlab = "Radar VV", col = "green", xlim = c(-20, 0), ylim=c(0,700))
 hist(val_MET$Variables.8, main = "Matorral Espinoso Tamaulipeco", xlab = "Radar VV", col = "springgreen1", xlim = c(-20, 0), ylim=c(0,8000))
 hist(val_METII$Variables.8, main = "Matorral Espinoso Tamaulipeco II", xlab = "Radar VV", col = "yellowgreen", xlim = c(-20, 0), ylim=c(0,8000))
 hist(val_suelo$Variables.8, main = "Suelo Desnudo", xlab = "Radar VV", xlim = c(-20, 0),col = "gray", ylim=c(0,12000))
 hist(val_urbano$Variables.8, main = "Urbano", xlab = "Radar VV", xlim = c(-20, 0), col = "red", ylim=c(0,2000))
 dev.off()
 
## 3. Banda 2 and 3 (Dispersogramas)
png("DispersogramaB3xB4.png", width=750)
plot(Variables.2 ~ Variables.4, data = val_agriI, pch = 1, col = cols[1], xlim = c(0, 10000), ylim = c(0, 10000))
points(Variables.2 ~ Variables.4, data = val_agriII, pch = "+", col = cols[2])
points(Variables.2 ~ Variables.4, data = val_agua, pch = "*", col = cols[3])
points(Variables.2 ~ Variables.4, data = val_bosque, pch = ",", col = cols[4])
points(Variables.2 ~ Variables.4, data = val_galeria, pch = ",", col = cols[5])
points(Variables.2 ~ Variables.4, data = val_MET, pch = ",", col = cols[6])
points(Variables.2 ~ Variables.4, data = val_METII, pch = ",", col = cols[7])
points(Variables.2 ~ Variables.4, data = val_suelo, pch = ",", col = cols[8])
points(Variables.2 ~ Variables.4, data = val_urbano, pch = ",", col = cols[9])
grid()
legend("topright", legend=Clasificacion, fill=cols, bg="white")
dev.off()

## 3. Banda 2 and 3 (Dispersogramas)
png("DispersogramaTreeCoverxNDVI.png", width=750)
plot(Variables.5 ~ Variables.6, data = val_agriI, pch = 1, col = cols[1], xlim = c(-1, 1), ylim = c(0, 100))
points(Variables.5 ~ Variables.6, data = val_agriII, pch = "+", col = cols[2])
points(Variables.5 ~ Variables.6, data = val_agua, pch = "*", col = cols[3])
points(Variables.5 ~ Variables.6, data = val_bosque, pch = ",", col = cols[4])
points(Variables.5 ~ Variables.6, data = val_galeria, pch = ",", col = cols[5])
points(Variables.5 ~ Variables.6, data = val_MET, pch = ",", col = cols[6])
points(Variables.5 ~ Variables.6, data = val_METII, pch = ",", col = cols[7])
points(Variables.5 ~ Variables.6, data = val_suelo, pch = ",", col = cols[8])
points(Variables.5 ~ Variables.6, data = val_urbano, pch = ",", col = cols[9])
grid()
legend("topright", legend=Clasificacion, fill=cols, bg="white")
dev.off()


## 3. VV vs NIR (Dispersogramas)
png("DispersogramaVVxNIR.png", width=750)
plot(Variables.8 ~ Variables.4, data = val_agriI, pch = 1, col = cols[1], xlim = c(0, 6000), ylim = c(-20, 0))
points(Variables.8 ~ Variables.4, data = val_agriII, pch = "+", col = cols[2])
points(Variables.8 ~ Variables.4, data = val_agua, pch = "*", col = cols[3])
points(Variables.8 ~ Variables.4, data = val_bosque, pch = ",", col = cols[4])
points(Variables.8 ~ Variables.4, data = val_galeria, pch = ",", col = cols[5])
points(Variables.8 ~ Variables.4, data = val_MET, pch = ",", col = cols[6])
points(Variables.8 ~ Variables.4, data = val_METII, pch = ",", col = cols[7])
points(Variables.8 ~ Variables.4, data = val_suelo, pch = ",", col = cols[8])
points(Variables.8 ~ Variables.4, data = val_urbano, pch = ",", col = cols[9])
grid()
legend("topright", legend=Clasificacion, fill=cols, bg="white")
dev.off()


#Salvar los valores que usaremos en el entrenamiento de Random Forest
write.csv(valuetable, file="CSV/ValueTable_entrenamiento7_TodasVar.csv")

#valuetable <- read.csv("CSV/ValueTable_entrenamiento5_ANIR2.csv")
#########################################################################################
#RANDOM FOREST
## Construir un modelo random forest
# Covariables (X) se encuentran en las columnas 1:13 de la tabla valuetable
# Las clases de entrenamiento (Y) se encuentran en la columna de nombre 'class' de la tabla valuetable
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
valuetable <- read.csv("CSV/Previous/ValueTable_entrenamiento7_TodasVar.csv")
#valuetable <- round(valuetable,2)
#valuetable <- valuetable[,2:12]

library(randomForest)
modelRF <- randomForest(x=valuetable[ ,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)], y=valuetable$class,#,12
                        importance = TRUE, type="classification")

## Inspeccionar la estructua y los nombres de los elementos en el modelo resutante
modelRF
class(modelRF)
str(modelRF)
names(modelRF)
ModeloError <- 1 - modelRF$err.rate[500,1]
print(paste("Model Accuracy = ", ModeloError))
#where the i-th element is the (OOB) error rate for all trees up to the i-th.
#one can plot it and check if it is the same as the OOB in the plot method defined for 
#rf models:

#Gráfico del árbol de desición
#https://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree
reprtree:::plot.getTree(modelRF)


png("PNG/6_ErrorRate.png", width=750)
par(mfrow = c(2,1))
plot(modelRF$err.rate[,1], type = "l", main="Tasa de Error")
plot(modelRF)
dev.off()
## Inspeccionar la matriz de confusión del el analisís del error OOB
modelRF$confusion

# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("Agricultura I", "Agricultura II", "Agua",
                                  "Bosque I", "Galeria", "Humedal" ,"Matorral", "MET", "MET II",
                                  "Suelo Desnudo", "Urbano", "class.error")
rownames(modelRF$confusion) <- c("Agricultura I", "Agricultura II", "Agua",
                                 "Bosque I", "Galeria", "Humedal", "Matorral", "MET", "MET II",
                                 "Suelo Desnudo", "Urbano")
modelRF$confusion
write.csv(modelRF$confusion, file="CSV/Confusion_entrenamiento7_TodasVar.csv")

#Validación con caret
folds <- sample(rep(1:5, length.out = nrow(valuetable)), size = nrow(valuetable), replace = F)
print(table(folds))

#Data frames containing the predictions and real values
CV_rf <- lapply(1:5, function(x){ #5 corresponds to the number of folds defined earlier
  model <- randomForest(class ~ ., data = valuetable[folds != x,])
  preds <- predict(modelRF,  valuetable[folds == x,], type="response")
  return(data.frame(preds, real = valuetable$class[folds == x]))
})

#Convert list of data frames to a data frame:
CV_rf <- do.call(rbind, CV_rf)

#Check accuracy
library(caret)
caret::confusionMatrix(CV_rf$preds, CV_rf$real) 

#Ya que definimos importance=TRUE, podemos obtener información sobre la importancia estadística 
#de cada covariable, esta puede visualizarse usando el comando varImpPlot() 

#Gráficos de importancia, estudiar más
png("PNG/7_ImportanciaCovariables_entrenamiento7.png", width=750)
varImpPlot(modelRF)
dev.off()

## Double-check layer and column names to make sure they match
#identical(names(covs),names(valuetable)[1:7])

covs <- stack("TIFF/Variables.tif")
covs <- covs[[c(3:5,7:13)]]
#names(covs) <- names(valuetable)
## Predict land cover using the RF model
predLC <- predict(covs, model=modelRF, na.rm=TRUE, 
                  progress="text", 
                  file="TIFF/RF_class.tiff_Entrenamiento7_TodasVar", 
                  overwrite=TRUE)

graphics.off()
plot(predLC, col=cols)
plot(Linares, add = TRUE)
