#Arbol de la clasificación randomForest
#######################################


#Instalar paquete para la visualización
#######################################
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}

for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

data("iris")

#Correr el modelo
model <- randomForest(Species ~ ., 
                      data=iris, 
                      importance=TRUE, 
                      ntree=500, 
                      do.trace=100)#, mtry = 3

reprtree:::plot.getTree(model)

