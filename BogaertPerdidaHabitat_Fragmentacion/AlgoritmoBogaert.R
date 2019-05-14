#Crear algoritmo de tipo de Fragmentación y/o pérdida de hábitat
# J. BOGAERT, R. CEULEMANS, D. SALVADOR-VAN EYSENRODE, 2004
#Decision Tree Algorithm for Detection of Spatial Processes in Landscape Transformation.
#Environmental Managemet, Vol. 33, No. 1, pp. 62–73
#K. Wiese
########################################################################################

#Definir directorio de trabajo
#setwd("~/R/0_Fragmentacion/")

#Antes (Momento inicial)
n0 <- 2     #Número de parchos 
a0 <- 1250  #Área 
p0 <- 1200  #Perimetro

#Despues (Momento Posterior)
n1 <- 3
a1 <- 1100
p1 <- 1300


Transformation <- function(n0,n1,a0,a1,p0,p1){
  if(n0 == n1){
    #if(a1 == a0 && p1 == p0) LSP<-"No Change" else
    if(a1 == a0 && p1 != p0) LSP<-"Deformation" else
    if(a1 == a0 && p1 == p0) LSP<-"Shift" else
    if(a1 < a0 && p1 > p0) LSP<-"Perforation" else
    if(a1 < a0 && p1 <= p0) LSP<-"Shrinkage" else
    if(a1 > a0) LSP<-"Enlargement"} else
            
  if(n1 > n0){
    if(a1 > a0) LSP<-"Creation" else
    if(a1 >= (0.95*a0)) LSP<-"Disection" else
    if(a1 < (0.95*a0)) LSP<-"Fragmentation"} else      

  if(n1 < n0){
    if(a1 < a0) LSP<-"Attrition" else
    if(a1 >= a0) LSP<-"Aggregation" else
    LSP<-"No Identificado"}
  return(LSP)
}

print(Transformation(n0, n1, a0, a1, p0, p1))
