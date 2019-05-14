shinyServer(function(input, output) {

    filteredData <- reactive({
      ultimos_sismos<-na.omit(ultimos_sismos)
      ultimos_sismos[ultimos_sismos$Magnitud >= input$range[1] & ultimos_sismos$Magnitud <= input$range[2],]
    })
    
    colorpal <- reactive({
      colorNumeric(input$colors, ultimos_sismos$Magnitud)
    })
    
    output$map <- renderLeaflet({
      ii<-which.max(ultimos_sismos_sp@data$Magnitud)
      leaflet(ultimos_sismos_sp) %>% 
        setView(lng = ultimos_sismos_sp@coords[ii,2], 
                lat = ultimos_sismos_sp@coords[ii,1], zoom = 7) %>%
        addProviderTiles("OpenTopoMap",group = "Topo") %>%
        addProviderTiles("Esri.NatGeoWorldMap",group="NatGeoWM")%>%
        addProviderTiles("CartoDB.Positron") %>%
        addProviderTiles("Esri.WorldImagery",group="Visible") %>%
        # Layers control
        addLayersControl(position="bottomleft",
                         baseGroups = c("Visible", "OSM", "Topo", "NatGeoWM"),#
                         options = layersControlOptions(collapsed = FALSE))
    })
    
    observe({
      pal <- colorpal()
      
      Fecha<-filteredData()$'Fecha.Local'
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        addCircles(radius = ~10000, weight = 2, color = "#777777",#10^Magnitud/2
                   fillColor = ~pal(Magnitud), fillOpacity = 0.8, 
                   popup = ~paste(sep = "<br/>",
                                  paste("<b>Lugar:",Lugar),
                                  paste("<b>Fecha:",Fecha),
                                  paste("<b>Profundidad:",Profundidad, "km"),
                                  paste("<b>Fuente de Localizacion:",locationSource),
                                  paste("<b>Magnitud:",Magnitud, TipoMag)
                                  )
        )
    })
    
    observe({
      proxy <- leafletProxy("map", data = ultimos_sismos)

      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~Magnitud
        )
      }
    })
  })