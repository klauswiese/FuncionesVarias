#Script para identificar la relación de las variables (B2, B3.... VCF, NDVI)
#con los diferentes usos del suelo y coberturas vegetales (Pino, Latifoliado, Matorral y Cultivos)
#Madrid 5 de Febrero 2019
##################################################################################################

#Librerias
library(caret)

#Relación de variables
boxplot(valuetable$B2~valuetable$clase,las=2)
boxplot(valuetable$B3~valuetable$clase,las=2)
boxplot(valuetable$B4~valuetable$clase,las=2)
boxplot(valuetable$B5~valuetable$clase,las=2)
boxplot(valuetable$B8~valuetable$clase,las=2)
boxplot(valuetable$NDVI~valuetable$clase,las=2)
boxplot(valuetable$VCF~valuetable$clase,las=2)
boxplot(valuetable$VV~valuetable$clase,las=2)
boxplot(valuetable$VH~valuetable$clase,las=2)
boxplot(valuetable$AR~valuetable$clase,las=2)
boxplot(valuetable$ANIR~valuetable$clase,las=2)
boxplot(valuetable$MDE~valuetable$clase,las=2)
boxplot(valuetable$Slope~valuetable$clase,las=2)


#Dispersogramas
featurePlot(x = valuetable[, c("B2","B3","B4","B5","B8")], 
            y = factor(valuetable$clase), 
            plot = "pairs",
            auto.key = list(columns = 5))

featurePlot(x = valuetable[, c("NDVI","AR","ANIR","VCF", "VV", "VH")], 
            y = factor(valuetable$clase), 
            plot = "pairs",
            auto.key = list(columns = 6))

featurePlot(x = valuetable[, c("MDE","Slope")], 
            y = factor(valuetable$clase), 
            plot = "pairs",#density
            auto.key = list(columns = 2))
