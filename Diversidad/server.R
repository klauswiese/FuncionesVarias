#Librerias
library(shiny)
library(vegan)
library(BiodiversityR)
library(xtable)

#Inicia configuración de shiny
shinyServer(function(input, output,session) {

##################
#1. Crear objetos#
##################
#---------------------------------------------------------------------------
#Crea datos, en objeto tipo reactive, para graficar una curva de acumulación 
#de especies
  
  Curva <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data <- read.csv(input$datafile$datapath)
    Nombres <- data[,1]
    data <- data[,2:dim(data)[2]]
    row.names(data) <- Nombres
    data <- specaccum(data)
  })
  
#Crea datos, en objeto tipo reactive, para graficar diversidad de Shannon-Wiener
#para cada undiad de muestreo
#-------------------------------------------------------------------------------
    Shanon <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data1 <- read.csv(input$datafile$datapath)
    Nombres <- data1[,1]
    data1 <- data1[,2:dim(data1)[2]]
    row.names(data1) <- Nombres
    data <- apply(data1, 1, diversity)
  })

#Crea datos, en objeto tipo reactive, para graficar diversidad de Simpson
#para cada undiad de muestreo
#-------------------------------------------------------------------------------
  Simpson <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data1 <- read.csv(input$datafile$datapath)
    Nombres <- data1[,1]
    data1 <- data1[,2:dim(data1)[2]]
    row.names(data1) <- Nombres
    data <- apply(data1, 1, function(x) diversity(x, "simpson"))
  })
  
#Crea datos, en objeto tipo reactive, para graficar diversidad de Inversa de Simpson
#para cada undiad de muestreo
#-----------------------------------------------------------------------------------
  SimpsonInv <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data1 <- read.csv(input$datafile$datapath)
    Nombres <- data1[,1]
    data1 <- data1[,2:dim(data1)[2]]
    row.names(data1) <- Nombres
    data <- apply(data1, 1, function(x) diversity(x, "invsimpson"))
  })
  
#Crea datos, en objeto tipo reactive, para graficar diversidad de Berger-Parker
#para cada undiad de muestreo
#-------------------------------------------------------------------------------
  
  Berger <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data1 <- read.csv(input$datafile$datapath)
    Nombres <- data1[,1]
    data1 <- data1[,2:dim(data1)[2]]
    row.names(data1) <- Nombres
    
    data2 <- data.frame(Berger=NA)
    for (i in 1:dim(data1)[1]) {
      data2[i,] <- diversityresult(data1[i,], index = "Berger", method="pooled")
    }
    data2
  })

#Crea datos, en objeto tipo reactive, para graficar diversidad de Log Alpha
#para cada undiad de muestreo
#---------------------------------------------------------------------------  
  Logalpha <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data1 <- read.csv(input$datafile$datapath)
    Nombres <- data1[,1]
    data1 <- data1[,2:dim(data1)[2]]
    row.names(data1) <- Nombres
    
    data2 <- data.frame(Logalpha=NA)
    for (i in 1:dim(data1)[1]) {
      data2[i,] <- diversityresult(data1[i,], 
                                   index = "Logalpha", 
                                   method="pooled")
    }
    data2
  })
  
#Crea datos, en objeto tipo reactive, para graficar diversidad de Evenness de Shannon
#para cada undiad de muestreo
#------------------------------------------------------------------------------------
  Jevenness <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data1 <- read.csv(input$datafile$datapath)
    Nombres <- data1[,1]
    data1 <- data1[,2:dim(data1)[2]]
    row.names(data1) <- Nombres
    
    data2 <- data.frame(Jevenness=NA)
    for (i in 1:dim(data1)[1]) {
      data2[i,] <- diversityresult(data1[i,], 
                                   index = "Jevenness", 
                                   method="pooled")
    }
    data2
  })

#Crea datos, en objeto tipo reactive, para la tabla de los estimadores de riqueza
#con su error estándar
#--------------------------------------------------------------------------------

Riqueza <-reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data <- read.csv(input$datafile$datapath)
    Nombres <- data[,1]
    data <- data[,2:dim(data)[2]]
    row.names(data) <- Nombres
    data <- specpool(data)
    names(data) <- c("# de Especies", 
                     "Chao", 
                     "Error Estándar Chao", 
                     "Jackknife 1", 
                     "Error Estándar Jackknife 1", 
                     "Jackknife 2",
                     "Bootstrap", 
                     "Error Estándar Bootstrap", 
                     "Número de Unidades de Muestreo")
    row.names(data) <- "Todos"
    data
  })

#Crea datos, en objeto tipo reactive, para la tabla de los índices de diversidad
#global
#--------------------------------------------------------------------------------
  Diversidad <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data <- read.csv(input$datafile$datapath)
    Nombres <- data[,1]
    data <- data[,2:dim(data)[2]]
    row.names(data) <- Nombres
    Riqueza <- specnumber(colSums(data))
    
    Shannon <- diversity(rowSums(data), 
                         "shannon")
    
    Simpson <- diversity(rowSums(data), 
                         "simpson")
    
    Simpson_Inverso <- diversity(rowSums(data), 
                                 "invsimpson")
    
    Logalpha <- diversityresult(data, 
                                index = "Logalpha", 
                                method="pooled")
    
    Berger_Parker <- diversityresult(data, 
                                     index = "Berger", 
                                     method="pooled")
    
    Igualdad_Shannon <- diversityresult(data, 
                                        index = "Jevenness", 
                                        method="pooled")
    
    Igualdad_Shannon_2 <- diversityresult(data, 
                                          index = "Eevenness", 
                                          method="pooled")
    
    data <- cbind(Riqueza, 
                  Shannon, 
                  Simpson, 
                  Simpson_Inverso, 
                  Logalpha, 
                  Berger_Parker,
                  Igualdad_Shannon, 
                  Igualdad_Shannon_2)
    row.names(data) <- "Piscina General (pooled)"
    data
  })

###################
#2. Gráficar datos#
###################
  
#Grafica la curva de acumulación de especies, asignando el objeto a la variables 
#Acumulacion
#-------------------------------------------------------------------------------
  output$Acumulacion <- renderPlot({
     if(!is.null(Curva()))
  plot(Curva(), 
       col="red", 
       xlab="Esfuerzo", 
       ylab="# de Especies", 
       main="Curva de Acumulación de Especies", 
       cex.lab=1.5, 
       cex=1.5, 
       cex.main=2)
  grid(col="black")
  box()
   })

#Graficá la diversidad de Shannon para cada unidad de muestreo, 
#asignando el objeto a la variable Dshannon
#--------------------------------------------------------------
  output$Dshannon <- renderPlot({
    if(!is.null(Shanon()))
      plot(Shanon(), 
           type="b", 
           col="darkgreen", 
           pch=16, 
           lty=2, 
           lwd=2, 
           main = "Índice de Diversidad de Shannon-Wiener para cada Unidad de Muestreo",
           xlab = "Unidad de Muestreo", 
           ylab="H'", 
           cex.lab=1.5, 
           cex=1.5, 
           cex.main=2)
    grid(col="black")
    box()
  })
  
#Graficá la diversidad de Simpson para cada unidad de muestreo, 
#asignando el objeto a la variable Dsimpson
#--------------------------------------------------------------
  output$Dsimpson <- renderPlot({
    if(!is.null(Simpson()))
      plot(Simpson(), 
           type="b", 
           col="orange", 
           pch=16, 
           lty=2, 
           lwd=2, 
           main = "Índice de Diversidad Simpson para cada Unidad de Muestreo",
           xlab = "Unidad de Muestreo", 
           ylab="Índice de Simpson", 
           cex.lab=1.5, 
           cex=1.5, 
           cex.main=2)
    grid(col="black")
    box()
  })
  
#Graficá la diversidad inversa de Simpson para cada unidad de muestreo, 
#asignando el objeto a la variable Dinvsimpson
#----------------------------------------------------------------------
  output$Dinvsimpson <- renderPlot({
    if(!is.null(SimpsonInv()))
      plot(SimpsonInv(), 
           type="b", 
           col="seagreen", 
           pch=16, 
           lty=2, 
           lwd=2, 
           main = "Índice de Diversidad Simpson Inverso para cada Unidad de Muestreo",
           xlab = "Unidad de Muestreo", 
           ylab="Índice Simpson Inverso", 
           cex.lab=1.5, 
           cex=1.5, 
           cex.main=2)
    grid(col="black")
    box()
  })
  
#Graficá la diversidad de Berger-Parker para cada unidad de muestreo, 
#asignando el objeto a la variable Dberger
#--------------------------------------------------------------------
  output$Dberger <- renderPlot({
    if(!is.null(Berger()))
      plot(Berger()$Berger, 
           type="b", 
           col="darkblue", 
           pch=16, 
           lty=2, 
           lwd=2, 
           main = "Índice de Diversidad Berger-Parker para cada Unidad de Muestreo",
           xlab = "Unidad de Muestreo", 
           ylab="Índice Berger-Parker", 
           cex.lab=1.5, 
           cex=1.5, 
           cex.main=2)
    grid(col="black")
    box()
  })
  
#Graficá la diversidad de Log Alpha para cada unidad de muestreo, 
#asignando el objeto a la variable DLogalpha
#--------------------------------------------------------------

  output$Dlogalpha <- renderPlot({
    if(!is.null(Logalpha()))
      plot(Logalpha()$Logalpha, 
           type="b", 
           col="yellowgreen", 
           pch=16, 
           lty=2, 
           lwd=2, 
           main = "Serie Logarítmica de Diversidad Alpha para cada Unidad de Muestreo",
           xlab = "Unidad de Muestreo", 
           ylab="Índice log alpha", 
           cex.lab=1.5, 
           cex=1.5, 
           cex.main=2)
    grid(col="black")
    box()
  })

#Graficá la diversidad de Jevenness para cada unidad de muestreo, 
#asignando el objeto a la variable DJevenness
#--------------------------------------------------------------
  
  output$Djevenness <- renderPlot({
    if(!is.null(Jevenness()))
      plot(Jevenness()$Jevenness, 
           type="b", 
           col="tomato", 
           pch=16, 
           lty=2, 
           lwd=2, 
           main = "Índice de Igualdad (evenness) de Shannon para cada Unidad de Muestreo",
           xlab = "Unidad de Muestreo", 
           ylab="Índice de Igualdad", 
           cex.lab=1.5, 
           cex=1.5, 
           cex.main=2)
    grid(col="black")
    box()
  })

#Tabla de los estimadores globales de riqueza, asignando el objeto a la variable 
#table
#-------------------------------------------------------------------------------

  output$Estimadores <- renderTable(bordered=TRUE, spacing = "l",{#align = "c", 
    if(!is.null(Riqueza()))
      Riqueza()
  }, caption = "ESTIMADORES DE RIQUEZA (RESULTADOS GLOBALES)",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

#Tabla de los índices de diversidad globales, asignando el objeto a la variable 
#Diversidad
#-------------------------------------------------------------------------------

  output$Diversidad <- renderTable(bordered=TRUE, spacing = "l",{# align = "c",
    if(!is.null(Diversidad()))
      Diversidad()
  }, caption = "ÍNDICES DE DIVERSIDAD (RESULTADOS GLOBALES)",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  
})
