#Cálculo del área de cada categoria
#K. Wiese 2 de Noviembre 2018
###################################

#Definir directorio de trabajo
setwd("~/R/0_Linares/")

rm(list=ls())

#Cargar datos
Coberturas <- read.csv("CSV/Previous/RF_7clean10000Areas.csv")
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
cols <- c("orange", "darkorange" , 'blue' ,"dark green" ,
          "green", "seagreen", "darkseagreen", "limegreen", "lemonchiffon" , 'brown', 'red' )


#Calcular áreas por DN
AreaCoberturas <- tapply(Coberturas$Area_km2, Coberturas$DN, sum)
AreaCoberturas <- data.frame(AreaCoberturas[2:12])
row.names(AreaCoberturas) <- Clasificacion
names(AreaCoberturas) <- "Area Km"
AreaCoberturas$Porcentaje <- round(AreaCoberturas$`Area Km`*100/sum(AreaCoberturas$`Area Km`),2)

#Gráfico
png(file="PNG/8_PorcentajesCoberturas.png", width=1000, height = 750)
barplot(AreaCoberturas$Porcentaje, 
        col=cols, 
        names.arg = Clasificacion, 
        las=1,
        ylim=c(0,45),
        main="Área Total de Cobertura Vegetal y Usos de Suelo \n Linares, Nuevo León",
        xlab="Cobertura Vegetal y Uso del Suelo",
        ylab="Porcentaje de Área")
grid(col="black")
box(col="black")
dev.off()

#Reclasificar datos para agrupar
AreaCoberturas$Reclass <- c(1,1,2,3,3,4,5,6,7,8,9)
nombresReclass <- c("Agricultura", "Agua", "Bosque", 
                    "Humedal", "Matorral Submontano", 
                    "MET", "MET II", "Suelo Desnudo", "Urbano")

AreaReclass <- tapply(AreaCoberturas$Porcentaje, AreaCoberturas$Reclass, sum)


#Gráfico
png(file="PNG/9_PorcentajesCoberturasReclass.png", width=1000, height = 750)
barplot(AreaReclass, 
        col=cols[-c(2,4)], 
        names.arg = nombresReclass, 
        las=1,
        ylim=c(0,45),
        main="Área Total de Cobertura Vegetal y Usos de Suelo \n Linares, Nuevo León",
        xlab="Cobertura Vegetal y Uso del Suelo",
        ylab="Porcentaje de Área")
grid(col="black")
box(col="black")
dev.off()
