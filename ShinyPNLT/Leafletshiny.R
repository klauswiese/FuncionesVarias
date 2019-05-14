#Leaflet K. Wiese 1 Junio 2017
#https://www.r-bloggers.com/leaflet-interactive-web-maps-with-r/
#https://rstudio.github.io/leaflet/shiny.html
#---------------------------------------------------------------

#Librerias
library(shiny)
library(leaflet)
library(raster)
library(rgeos)

#Cargar Shapefile
PNLT <- shapefile("PNLT.shp")

#Configuración interfaz visual en página web
#-------------------------------------------

ui <- bootstrapPage(
  navbarPage("Evaluación de Ubicación en PNLT",
    tabPanel("Mapa", 
        sidebarLayout(
          sidebarPanel(
            fileInput('datafile', 'Seleccione Archivo CSV',accept=c('csv', 'comma-separated-values','.csv')),
            h2("¿Dónde Esta El Punto?"),
            p("Identifica si tu coordenada esta dentro del Parque Nacional La Tigra"),
            code('Simbología'),
            br(),
            img(src = "Fuera.png", height = 30),
            "= Fuera de PNLT",
            br(),
            img(src = "Amortiguamiento.png", height = 30),
            "= En la Zona de Amortiguamiento",
            br(),
            img(src = "Nucleo.png", height = 30),
            "= En la Zona Núcleo",
            br()
          ),
          mainPanel(
        tags$head(includeCSS("www/styles.css")),
        leafletOutput("LaTigra"))
        )),
        
    
    
    tabPanel("Tabla de Resultados", 
             sidebarLayout(
               sidebarPanel(
                 #fileInput('datafile', 'Seleccione Archivo CSV',accept=c('csv', 'comma-separated-values','.csv')),
                 h2("¿Dónde Esta El Punto?"),
                 p("Identifica si tu coordenada esta dentro del Parque Nacional La Tigra"),
                 code('Parque Nacional La Tigra'),
                 br(),
                 #img(src = "Amortiguamiento.png", width = 50),
                 #br(),
                 #img(src = "Fuera.png", height = 50),
                 br(),
                 br(),
                 "Creado por ", 
                 span("MHN, UNAH", style = "color:darkgreen"),
                 br(),
                 br(),
                 br(),
                 downloadLink("Descargar", "Descargar Datos")
               ),
               mainPanel(
                 tableOutput('Tabla'))
             ))
 ))

#Configuración de la información a desplegar
#Crear tablas, gráficos y mapas
#-------------------------------------------
server <- function(input, output, session) {
  
  ##################
  #1. Crear objetos#
  ##################
  #---------------------------------------------------------------------------
  #Análiza si las coordenas esta dentro o fuera del PNLT
  
  PuntosTabla <- reactive({
    if (is.null(input$datafile))
      return(NULL)                
    Data<-read.csv(input$datafile$datapath)

    Puntos <- data.frame(Data$X, Data$Y)
    if(Puntos$Data.X[1] < 50000){
      
      Puntos   <- SpatialPoints(Puntos,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
      Puntos <- spTransform(Puntos, CRS(proj4string(PNLT)))
      #Puntos$Id <- Data$Id
      Puntos$Nombre <- Data$Nombre
      
      #Ver si esta dentro de PNLT
      Res <- over(Puntos, as(PNLT, "SpatialPolygons"))
      Resultado <- ifelse(Res == 1, "Dentro de PNLT (Amortiguamiento)", 
                          ifelse(Res == 2, "Dentro de PNLT (Núcleo)" ,"Fuera de PNLT"))
      ppp <- is.na(Resultado)
      Resultado[ppp] <- "Fuera de PNLT"
      
      Puntos$Resultado <- Resultado
      Puntos2 <- as.data.frame(Puntos)
      names(Puntos2) <- c("Nombre", "Resultado", "Longitud", "Latitud")
      Puntos2
    } else {
      
      Puntos   <- SpatialPoints(Puntos, proj4string=CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"))
      #XX <- Puntos$Data.X
      #YY <- Puntos$Data.Y
      Puntos <- spTransform(Puntos, CRS(proj4string(PNLT)))
      #Puntos$Id <- Data$Id
      Puntos$Nombre <- Data$Nombre
      
      #Ver si esta dentro de PNLT
      Res <- over(Puntos, as(PNLT, "SpatialPolygons"))
      Resultado <- ifelse(Res == 1, "Dentro de PNLT (Amortiguamiento)", 
                          ifelse(Res == 2, "Dentro de PNLT (Núcleo)" ,"Fuera de PNLT"))
      ppp <- is.na(Resultado)
      Resultado[ppp] <- "Fuera de PNLT"
      
      Puntos$Resultado <- Resultado
      #Puntos$Coor_X <- XX
      #Puntos$Coor_Y <- YY
      Puntos2 <- as.data.frame(Puntos)
      names(Puntos2) <- c("Nombre", "Resultado", "Longitud", "Latitud")
      Puntos2  
     }
  })

#################
#2. Crear Iconos#
#################
  
  
  leafIcons <- reactive({ 
    icons(iconUrl = ifelse(PuntosTabla()$Resultado == "Dentro de PNLT (Amortiguamiento)","www/Amortiguamiento.png" ,#"http://www.iconsdb.com/icons/preview/orange/leaf-3-xxl.png" 
                           ifelse(PuntosTabla()$Resultado == "Dentro de PNLT (Núcleo)", "www/Nucleo.png" ,#"http://www.iconsdb.com/icons/preview/soylent-red/leaf-3-xxl.png"
                                  "www/Fuera.png")#"http://www.iconsdb.com/icons/preview/guacamole-green/leaf-3-xxl.png"
    ),
    iconWidth = 25, iconHeight = 25,
    iconAnchorX = 25, iconAnchorY = 25)  
  })
################
#3. Crear Mapas#
################
  
  
 output$LaTigra <- renderLeaflet({
  if(!is.null(PuntosTabla()))
    
    leaflet(data = PNLT) %>% addProviderTiles("Esri.WorldImagery",group="Visible") %>%
      addProviderTiles("Esri.NatGeoWorldMap",group="NatGeoWM")%>%
      addProviderTiles("CartoDB.Positron",group = "CartoDB") %>%
      addProviderTiles("OpenTopoMap",group = "Topo") %>%
      addProviderTiles("Stamen.Toner",group = "Toner") %>%
      # Layers control
      addLayersControl(position="bottomleft",
                       baseGroups = c("Visible","OSM","NatGeoWM","Topo", "Toner"),#
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addPolygons(fillColor = c("orange", "green"), stroke = TRUE, col="orange", weight = 1) %>%
      addMarkers(data = PuntosTabla(), lat=~Latitud, lng=~Longitud, popup=paste(PuntosTabla()$Nombre, PuntosTabla()$Resultado ,sep="<br/>"), icon = leafIcons())%>%
      addLegend("topright", colors=c("orange", "green"), labels=c("Zona de Amortiguamiento", "Zona Núcleo"))
  })
  
  
  #Tabla de los índices de diversidad globales, asignando el objeto a la variable 
  #Tabla
  #-------------------------------------------------------------------------------
  
output$Tabla <- renderTable(bordered=TRUE, spacing = "l",{# align = "c",
    if(!is.null(PuntosTabla()))
      PuntosTabla()
  }, caption = "RESULTADO DEL ANÁLISIS PARA CADA PUNTO",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))


output$Descargar <- downloadHandler(
  filename = function() {
    paste("Ubicacion_PNLT_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(PuntosTabla(), file)
  }
)

}

#Ejecutar la aplicación
shinyApp(ui, server)