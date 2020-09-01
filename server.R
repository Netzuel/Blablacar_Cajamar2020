packages <- c("shiny", "shinythemes", "shinyWidgets", "shinyanimate", "leaflet", "lubridate", "osrm", "chron", "sp", "shinyjs", "shinycssloaders","plotly","shinyalert","highcharter","raster","DT","geosphere")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#library(shiny)
#library(shinythemes)
#library(shinyWidgets)
#library(shinyanimate)
#library(leaflet)
#library(tidyverse)
#library(lubridate)
#library(osrm)
#library(chron)
#library(sp)
#library(shinyjs)
#library(shinycssloaders)
#library(plotly)
#library(shinyalert)
#library(highcharter)
#library(raster)
#library(DT)
#library(geosphere)

server <- function(input, output, session){
  observe(addScrollAnim(session, "tiempo", 'rollIn'))
  observe(addScrollAnim(session, "num_viajes", 'rollIn'))
  observe(addScrollAnim(session, "espyport", 'rollIn'))

  datos_blablacar <- reactive({
    datos <- load("data/datos_blablacar.RData")
    datos <- eval(parse(text = datos))
    return(datos)
  })
  lugares <- reactive({municipios()[,1]})
  lugares_2 <- reactive({
    unique((datos_blablacar() %>% filter(ORIGEN == input$origen) %>% arrange(DESTINO))[,4][[1]])
  })
  precio <- reactive({
    req(input$destino)
    precio_km <- datos_blablacar() %>% filter(ORIGEN == input$origen) %>% filter(DESTINO == input$destino)
    precio_km <- precio_km[,5][[1]]
    if(sum(is.na(precio_km)) == length(precio_km)){
      precio_medio <- mean(datos_blablacar()[,5][[1]], na.rm = TRUE)
    }
    else{
      precio_medio <- mean(precio_km, na.rm = TRUE)
    }
    return(precio_medio)
  })
  output$origen_in <- renderUI({
    selectInput("origen","Origen:",choices = lugares(), selected = "Madrid")
  })
  output$destino_in <- renderUI({
    validate(
      need(input$origen != "", "")
    )
    selectInput("destino","Destino:",choices = lugares_2(),selected=lugares_2()[which(str_detect(lugares_2(), "Valencia"))])
  })
  
  rutas <- reactive({
    osrm::osrmRoute(src=municipios()[lugares() == as.character(input$origen),7:6],dst=municipios()[lugares() == as.character(input$destino),7:6],returnclass="sp")
  })
  
  output$ruta <- renderLeaflet({
    m <- leaflet(data=sp::spTransform(rutas(),CRS("+proj=longlat +datum=WGS84"))) %>%
      addTiles() %>%
      addPolylines(weight=4,opacity=0.7,smoothFactor = 1,popup = paste("<b>",lugares()[lugares()==as.character(input$origen)],"-",lugares()[lugares()==as.character(input$destino)],"</b>","<br>","Tiempo estimado:",times(rutas()@data$duration/60/24),"<br>","Distancia:",round(rutas()@data$distance,2),"km","<br>","Precio estimado:", round(precio()*round(rutas()@data$distance,2),2),"€"))
    m
  })
  pueblosESP <- reactive({
    nombre_datos<-load("data/geoESP.RData")
    datos <- eval(parse(text = nombre_datos))
    return(datos)
  })
  pueblosPOR <- reactive({
    nombre_datos<-load("data/geoPOR.RData")
    datos <- eval(parse(text = nombre_datos))
    return(datos)
  })
  ranking <- reactive({
    nombre_datos<-load("data/ranking.RData")
    datos <- eval(parse(text = nombre_datos))
    return(datos)
  })
  municipios <- reactive({
    nombre_datos<-load("data/cajamar_municipios.RData")
    datos <- eval(parse(text = nombre_datos))
    return(datos)
  })
  lugares_2 <- reactive({
    unique((datos_blablacar() %>% filter(ORIGEN == input$origen) %>% arrange(DESTINO))[,4][[1]])
  })
  
  dias <- reactive({return(unique(ranking()$Dia))})
  dias.malos <- reactive({
    fechas <- seq.Date(min(dias()),max(dias()),by="day")
    return(fechas[!fechas %in% dias()])
  })
  
  output$fecha_rank <- renderUI(
    dateInput("dia_rank", "Día a consultar:",
              min = min(dias()),
              max  = max(dias()),
              value=dias()[1],
              datesdisabled=dias.malos(),
              language="es")
  )
  
  ranking_dia <- reactive({
    return(ranking() %>% filter(Dia==input$dia_rank))
  })
  ranking_filtrados <- reactive({
    datos <- ranking_dia() %>% dplyr::select(Municipios,Rank) %>% dplyr::mutate(Rank=round(Rank,4)) %>% dplyr::arrange(desc(Rank))
    datos$Rank <- as.numeric(datos$Rank)
    return(datos)
  })
  
  output$ranklist <- DT::renderDT(
    ranking_filtrados()
  )
  
  output$rank_abs <- renderLeaflet({
    
    joinrank <- pueblosESP()@data %>% inner_join(ranking_dia(),by=c("NAME_4"="Municipios")) %>% dplyr::select(NAME_4,Rank) %>% rename(nombre=NAME_4)
    
    joinrank2 <- pueblosPOR()@data %>% inner_join(ranking_dia(),by=c("NAME_2"="Municipios")) %>% dplyr::select(NAME_2,Rank) %>% rename(nombre=NAME_2)
    
    new <- pueblosESP()[pueblosESP()$NAME_4 %in% joinrank$nombre,]
    
    new2 <- pueblosPOR()[pueblosPOR()$NAME_2 %in% joinrank2$nombre,]
    
    new@data <- joinrank
    
    new2@data <- joinrank2
    
    todo <- rbind(new,new2)
    
    bins <- c(0.15, 0.175,0.2, 0.25, 0.3, 0.4, 10)
    colfunc <- colorRampPalette(c("#ED1A20","#EDCA1A", "#00B660"))
    pal <- colorBin(colfunc(length(todo$Rank)), domain = todo$Rank,bins=bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Rank %g",
      todo$nombre, todo$Rank
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet(data=todo) %>% addTiles() %>% addPolygons(fillColor=~pal(Rank),
                                                           weight =0.9,
                                                           opacity = 1,
                                                           color = "white",
                                                           dashArray = "1",
                                                           fillOpacity = 0.7,
                                                           highlight = highlightOptions(
                                                             weight = 2,
                                                             color = "white",
                                                             dashArray = "",
                                                             fillOpacity = 0.6,
                                                             bringToFront = TRUE),
                                                           label = labels,
                                                           labelOptions = labelOptions(
                                                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                                             textsize = "12px",
                                                             direction = "auto")) %>%
      addLegend(pal = pal, values = ~Rank, opacity = 0.7, title = NULL,
                position = "bottomright") %>% addProviderTiles(providers$CartoDB.Positron)
    m
  })
  
  
  output$fecha1 <- renderUI(
    dateInput("dia1", "Día 1:",
              min = min(dias()),
              max  = max(dias()),
              value=dias()[1],
              datesdisabled=dias.malos(),
              language="es")
  )
  output$fecha2 <- renderUI(
    dateInput("dia2", "Día 2:",
              min = min(dias()),
              max  = max(dias()),
              value=dias()[2],
              datesdisabled=dias.malos(),
              language="es")
  )
  
  output$rank <- renderLeaflet({
    rank1 <- ranking() %>% filter(Dia==input$dia2)
    rank2 <- ranking() %>% filter(Dia==input$dia1)
    
    matcheado <- rank1 %>% inner_join(rank2,by="Municipios",suffix=c("1","2")) %>% dplyr::select(Municipios,Rank1,Rank2) %>% mutate(evol = ((Rank1-Rank2)/Rank2)*100)
    
    joinrank <- pueblosESP()@data %>% inner_join(matcheado,by=c("NAME_4"="Municipios")) %>% dplyr::select(NAME_4,evol) %>% rename(nombre=NAME_4)
    joinrank2 <- pueblosPOR()@data %>% inner_join(matcheado,by=c("NAME_2"="Municipios")) %>% dplyr::select(NAME_2,evol) %>% rename(nombre=NAME_2)
    
    new <- pueblosESP()[pueblosESP()$NAME_4 %in% joinrank$nombre,]
    new2 <- pueblosPOR()[pueblosPOR()$NAME_2 %in% joinrank2$nombre,]
    
    new@data <- joinrank
    new2@data <- joinrank2
    
    new <- raster::aggregate(new,by=c("nombre","evol"))
    new2 <- raster::aggregate(new2,by=c("nombre","evol"))
    
    todo <- rbind(new,new2)
    
    bins <- c(-Inf,-100,-50, 0,50, 100,Inf)
    colfunc <- colorRampPalette(c("#ED1A20","#EDCA1A", "#00B660"))
    pal <- colorBin(colfunc(length(todo$evol)), domain = todo$evol,bins=bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Rank variation: %g %%",
      todo$nombre, round(todo$evol,2)
    ) %>% lapply(htmltools::HTML)
    
    m <-leaflet(data=todo) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -3.7025600, lat = 40.4165000, zoom = 5) %>%
      addPolygons(fillColor=~pal(evol),
                  weight =0.9,
                  opacity = 1,
                  color = "white",
                  dashArray = "1",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.6,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto"),
                  layerId=~nombre) %>%
      addLegend(pal = pal, values = ~evol, opacity = 0.7, title = NULL,
                position = "bottomright",labFormat=labelFormat(suffix="%"))
    m
  })
  observeEvent(input$rank_shape_click, { # update the location selectInput on map clicks
    p <- input$rank_shape_click
    print(p)
  })
  
  output$timerank <- renderHighchart({
    req(input$rank_shape_click)
    rank.filt <- ranking() %>% filter(Municipios==input$rank_shape_click$id & Dia >= input$dia1 & Dia <= input$dia2) %>% arrange(Dia)
    
    x <- c("Rank \b")
    y <- sprintf("{point.%s:.4f}", c("Rank"))
    tltip <- tooltip_table(x, y)
    hc <- hchart(rank.filt,"line",hcaes(Dia,Rank)) %>% hc_title(
      text = paste("Evolución del Rank para <span style=\"color:#1A54ED\"><b>",input$rank_shape_click$id ,"</b></span>"),
      useHTML = TRUE) %>% hc_add_theme(hc_theme_elementary()) %>%
      hc_xAxis(title=NULL) %>%
      hc_yAxis(title = list(text = "Rank"),
               minorTickInterval = "auto",
               minorGridLineDashStyle = "LongDashDotDot",
               showFirstLabel = FALSE,
               showLastLabel = T) %>%
      hc_tooltip(pointFormat = tltip,useHTML = TRUE) %>% hc_yAxis(min=0.15,max = 1.1*max(rank.filt$Rank)) %>%
      hc_plotOptions(line = list(color = "#1A54ED",lineWidth=1.4,
                                 marker = list(
                                   fillColor = "white",
                                   lineWidth = 1,
                                   lineColor = NULL
                                 ))) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_chart(backgroundColor="#F8F5F0")
    hc
  })
  
  output$output_1 <- renderUI({
    req(input$destino)
    validate(
      need(input$origen != "", "")
    )
    variables <- names(datos_blablacar())[5:11]
    names(variables) <- c("Precio", "Asientos ofertados", "Asientos confirmados", "Viajes ofertados", "Viajes confirmados", "Ofertantes", "Ofertantes nuevos")
    selectInput("variable_1", paste("Variable a representar para los viajes", input$origen, "-->", input$destino), choices = variables, selected = variables[4], width = "65%")
  })
  output$bivariante_1 <- renderPlotly({
    if(input$variable_3 == "IMP_KM"){
      datos_filtrados <- datos_blablacar()
      datos_filtrados <- datos_filtrados %>% group_by(DIA) %>% summarize(variable = mean(get(input$variable_3)*100, na.rm = TRUE))
      plot <- plot_ly( text = ~paste0(" Día: ", DIA, paste("<br>", "Precio medio / km"), ": ", round(variable, 2), " cént/km"), data = datos_filtrados, x = ~DIA, y = ~variable,
                       marker = list(size = 3,
                                     color = 'rgb(128,128,128)')) %>% layout(plot_bgcolor='rgb(248,245,240)') %>% 
        layout(paper_bgcolor='rgb(248,245,240)') %>% layout(title = 'Precio medio / km',
                                                            yaxis = list(title = "cént/km",
                                                                         tickmode = "array"),
                                                            xaxis = list(title = "Día"))
      return(plot)
    }
    else{
      datos_filtrados <- datos_blablacar()
      indice <- which(names(datos_filtrados) == input$variable_3)
      nombre_1 <- names(datos_filtrados)[indice-1]
      nombre_2 <- names(datos_filtrados)[indice]
      datos_filtrados <- datos_filtrados %>% dplyr::select(c(DIA, nombre_1, nombre_2))
      datos_filtrados <- datos_filtrados %>% dplyr::mutate(total = get(nombre_1)+get(nombre_2))
      datos_filtrados <- datos_filtrados %>% dplyr::group_by(DIA) %>% dplyr::mutate(suma_1 = sum(get(nombre_2)), suma_2 = sum(total)) %>% dplyr::summarize(variable = mean(suma_1/suma_2))
      if(nombre_2 == "OFERTANTES_NUEVOS"){
        tipo <- "bar"
      }
      else{
        tipo <- "scatter"
      }
      if(nombre_2 == "ASIENTOS_CONFIRMADOS"){
        nombre_variable <- "asientos confirmados"
      }
      else if(nombre_2 == "VIAJES_CONFIRMADOS"){
        nombre_variable <- "viajes confirmados"
      }
      else{
        nombre_variable <- "ofertantes nuevos"
      }
      plot <- plot_ly(type = tipo, text = ~paste0(" Día: ", DIA, paste("<br>", "Tasa:"), round(variable, 2)), data = datos_filtrados, x = ~DIA, y = ~variable*100,
                      marker = list(size = 3,
                                    color = 'rgb(128,128,128)')) %>% layout(plot_bgcolor='rgb(248,245,240)') %>% 
        layout(paper_bgcolor='rgb(248,245,240)') %>% layout(title = paste("Tasa de los", nombre_variable),
                                                            yaxis = list(title = "%",
                                                                         tickmode = "array"),
                                                            xaxis = list(title = "Día"))
      return(plot)
    }
  })
  
  output$output_2 <- renderUI({
    req(input$destino)
    validate(
      need(input$origen != "", "")
    )
    variables <- names(datos_blablacar())[c(7, 9, 11)]
    names(variables) <- c("Asientos confirmados", "Viajes confirmados", "Ofertantes nuevos")
    selectInput("variable_2", paste("Variable a visualizar para los viajes"), choices = variables, selected = variables[1], width = "65%")
  })
  output$output_3 <- renderUI({
    variables <- names(datos_blablacar())[c(5,7,9,11)]
    names(variables) <- c("Precio medio", "Tasa de asientos confirmados", "Tasa de viajes confirmados", "Tasa de ofertantes nuevos")
    selectInput("variable_3", paste("Selecciona una variable:"), choices = variables, selected = variables[1], width = "65%")
  })
  output$output_4 <- renderUI({
    sliderInput("slider_1", "Límite superior:", min = 0, max = max(tabla()[,2]), value = ,max(tabla()[,2]))
  })
  
  
  output$output_fecha_1 <- renderUI({
    req(input$destino)
    validate(
      need(input$origen != "", "")
    )
    filtrados <- datos_blablacar() %>% filter(ORIGEN == input$origen & DESTINO == input$destino)
    fechas <- filtrados[,1][[1]]
    selectInput("fechas_1", "Selecciona un día:", choices = fechas, selected = fechas[1], width = "50%")
  })
  datos_filtrados <- reactive({
    datos_filtrados <- datos_blablacar() %>% dplyr::filter(ORIGEN == input$origen & DESTINO == input$destino)
    datos_filtrados <- datos_filtrados %>% dplyr::arrange(DIA)
    datos_filtrados <- datos_filtrados %>% dplyr::select(DIA, ORIGEN, DESTINO, input$variable_1)
    return(datos_filtrados)
  })
  output$output_8 <- renderUI({
    req(input$destino)
    validate(
      need(input$origen != "", "")
    )
    fechas <- unique(datos_blablacar()[,1][[1]])
    dateRangeInput("fecha_biv_2", label = NULL, start = fechas[1], end = fechas[length(fechas)], min = fechas[1],
                   max = fechas[length(fechas)], format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "es", separator = " a ", width = "40%")
  })
  output$bivariante_2 <- renderPlotly({
    req(input$origen)
    validate(
      need(input$origen != "", "")
    )
    nombre_variable <- names(datos_filtrados())[4]
    fechas <- input$fecha_biv_2
    if(nombre_variable == "IMP_KM"){
      nombre_variable <- "Precio por km"
    }
    else if(nombre_variable == "ASIENTOS_OFERTADOS"){
      nombre_variable <- "Asientos ofertados"
    }
    else if(nombre_variable == "ASIENTOS_CONFIRMADOS"){
      nombre_variable <- "Asientos confirmados"
    }
    else if(nombre_variable == "VIAJES_OFERTADOS"){
      nombre_variable <- "Viajes ofertados"
    }
    else if(nombre_variable == "VIAJES_CONFIRMADOS"){
      nombre_variable <- "Viajes confirmados"
    }
    else if(nombre_variable == "OFERTANTES"){
      nombre_variable <- "Ofertantes"
    }
    else{
      nombre_variable <- "Ofertantes nuevos"
    }
    if(input$variable_1 %in% c("ASIENTOS_OFERTADOS", "ASIENTOS_CONFIRMADOS", "VIAJES_OFERTADOS", "VIAJES_CONFIRMADOS", "OFERTANTES", "OFERTANTES_NUEVOS")){
      if((max(datos_filtrados()[,4][[1]])-min(datos_filtrados()[,4][[1]])) >= 5){
        secuencia <- seq(from = min(datos_filtrados()[,4][[1]]), to = max(datos_filtrados()[,4][[1]]), by = trunc((max(datos_filtrados()[,4][[1]])-min(datos_filtrados()[,4][[1]]))/5))
      }
      else{
        secuencia <- seq(from = min(datos_filtrados()[,4][[1]]), to = max(datos_filtrados()[,4][[1]]))
      }
      plot <- plot_ly(
        text = ~paste0(" Día: ", DIA, paste("<br>", nombre_variable), ": ", round(get(input$variable_1), 2)),
        datos_filtrados(),
        x = ~DIA,
        y = ~get(input$variable_1),
        type = "bar",
        marker = list(color = 'rgb(128,128,128)',
                      line = list(color = 'rgb(128,128,128)',
                                  width = 1.5))
      ) %>%
        layout(title = paste("Serie temporal de la variable", str_to_lower(nombre_variable)),
               yaxis = list(title = nombre_variable,
                            ticktext = secuencia, 
                            tickvals = secuencia,
                            tickmode = "array"),
               xaxis = list(title = "Día", range = c(fechas[1],fechas[2]))) %>% layout(plot_bgcolor='rgb(248,245,240)') %>% 
        layout(paper_bgcolor='rgb(248,245,240)')
    }
    else{
      distancia <- rutas()@data$distance
      plot <- plot_ly(text = ~paste0(" Día: ", DIA, paste("<br>", nombre_variable), ": ", round(get(input$variable_1)*distancia, 2), " €"), data = datos_filtrados(), x = ~DIA, y = ~get(input$variable_1)*distancia,
                      marker = list(size = 5, color = 'rgb(128,128,128)',
                                    line = list(color = 'rgb(128,128,128)',
                                                width = 0.2))) %>%
        layout(title = "Precio",
               yaxis = list(title = "€"),
               xaxis = list(title = "Día", range = c(fechas[1],fechas[2]))) %>% layout(plot_bgcolor='rgb(248,245,240)') %>% 
        layout(paper_bgcolor='rgb(248,245,240)')
    }
    return(plot)
  })
  
  output$bivariante_3 <- renderPlotly({
    req(input$origen)
    fecha <- input$fechas_1
    filtrados <- datos_blablacar() %>% filter(ORIGEN == input$origen & DESTINO == input$destino) %>% filter(DIA == fecha)
    cuenta_1 <-  filtrados[,which(names(filtrados) == input$variable_2)][[1]]
    cuenta_2 <- filtrados[,which(names(filtrados) == input$variable_2)-1][[1]]
    df <- as.data.frame(matrix(ncol = 2, nrow = 2))
    colnames(df) <- c("Clase", "Proporcion")
    if(input$variable_2 == "ASIENTOS_CONFIRMADOS"){
      df$Clase <- c("Asientos confirmados", "Asientos no confirmados")
    }
    else if(input$variable_2 == "VIAJES_CONFIRMADOS"){
      df$Clase <- c("Viajes confirmados", "Viajes no confirmados")
    }
    else{
      df$Clase <- c("Ofertantes nuevos", "Ofertantes antiguos")
    }
    df$Proporcion <- c(cuenta_1, cuenta_2-cuenta_1)
    plot <- plot_ly(data = df, labels = ~Clase, values = ~Proporcion, marker = list(colors = c('#696969', '#C0C0C0'))) %>% add_pie(hole = 0.6) %>%
      layout(title = paste("Proporciones de los", str_to_lower(df$Clase[1]), "para el día", fecha),  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% layout(paper_bgcolor='#F8F5F0')
  })
  tabla <- reactive({
    tabla <- table(factor(municipios()[,8]))
    paises <- names(tabla)
    valores <- c()
    for(i in seq_along(tabla)){
      valores <- c(valores, tabla[i][[1]])
    }
    tabla <- as.data.frame(matrix(ncol = 2, nrow = length(valores)))
    tabla$V1 <- paises
    tabla$V2 <- valores
    colnames(tabla) <- c("Paises", "Valores")
    tabla[c(5,14,20,22,26,27,30,32),1] <- c("Bélgica", "Hungría", "México", "Países Bajos", "Rumanía", "República Checa", "Eslovenia", "Turquía")
    return(tabla)
  })
  output$bivariante_4 <- renderPlotly({
    tabla <- tabla()
    plot <- plot_ly(data = tabla, x = ~Paises, y = ~Valores, type = "bar",
                    marker = list(color = 'rgb(128,128,128)',
                                  line = list(color = 'rgb(255,255,255)',
                                              width = 1.5))) %>% 
      layout(title = "Lugares según país",
             xaxis = list(title = "País", tickangle = 60),
             yaxis = list(title = "", range = c(0,input$slider_1))) %>% layout(plot_bgcolor='rgb(248,245,240)') %>% 
      layout(paper_bgcolor='rgb(248,245,240)')
  })
  output$heatmap_1 <- renderPlotly({
    tabla <- datos_blablacar() %>% group_by(DIA) %>% summarize(mean_impkm = mean(IMP_KM, na.rm = TRUE), mean_asientosof = mean(ASIENTOS_OFERTADOS, na.rm = TRUE), mean_asientoscon = mean(ASIENTOS_CONFIRMADOS, na.rm = TRUE), mean_viajesof = mean(VIAJES_OFERTADOS, na.rm = TRUE), mean_viajescon = mean(VIAJES_CONFIRMADOS, na.rm = TRUE), mean_of = mean(OFERTANTES, na.rm = TRUE), mean_of_nuevos = mean(OFERTANTES_NUEVOS, na.rm = TRUE))
    tabla <- tabla %>% dplyr::select(-DIA)
    correlacion <- cor(tabla)
    nombres <- c("Precio/km", "Asientos ofertados", "Asientos confirmados", "Viajes ofertados", "Viajes confirmados", "Ofertantes", "Ofertantes nuevos")
    plot <- plot_ly(
      x = nombres, y = nombres,
      z = correlacion, type = "heatmap", colors = "Greys"
    ) %>% layout(plot_bgcolor='rgb(248,245,240)') %>% layout(paper_bgcolor='rgb(248,245,240)') %>%
      layout(title = "",
             xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "")) %>% layout(plot_bgcolor='rgb(248,245,240)')
  })
  
  observeEvent(input$variable_1, {
    if(input$variable_1 == "IMP_KM"){
      if(sum(is.na(datos_filtrados()[,4][[1]])) == length(datos_filtrados()[,4][[1]])){
        showNotification(paste("¡No hay ningún dato disponible para el precio!"), duration = 3, type = "message")
        shinyjs::hide("bivariante_2")
      }
      else{
        shinyjs::show("bivariante_2")
      }
    }
    else if(input$variable_1 %in% c("ASIENTOS_OFERTADOS", "ASIENTOS_CONFIRMADOS", "VIAJES_OFERTADOS", "VIAJES_CONFIRMADOS", "OFERTANTES", "OFERTANTES_NUEVOS")){
      if(sum(datos_filtrados()[,4][[1]] == 0) == length(datos_filtrados()[,4][[1]])){
        showNotification(paste("¡Todos los datos son cero!"), duration = 3, type = "message")
        shinyjs::hide("bivariante_2")
      }
      else{
        shinyjs::show("bivariante_2")
      }
    }
  })
  
  observeEvent(input$origen, {
    showNotification(paste("Nota: Las visualizaciones pueden demorarse en cargar dependiendo del dispositivo utilizado a causa del tamaño de la base de datos que se ha usado."), duration = 7, type = "message")
  })
  
  pueblos <- reactive({municipios() %>% dplyr::select(Nombre,long=Longitud,lat=Latitud)})
  output$univ <- renderUI(selectInput("select_lugar","Lugar",choices = lugares(),selected="Valencia/València"))
  datos_univ <- reactive({
    data <- datos_blablacar()[datos_blablacar()$ORIGEN==input$select_lugar | datos_blablacar()$DESTINO==input$select_lugar,c(1,3:4,7,9)]
    return(data)
  })
  output$mapa_conexiones <- renderPlot({
    a <- unique(datos_univ()[,2])
    b <- unique(datos_univ()[,3])
    conexiones <- unique(c(a$ORIGEN,b$DESTINO))
    coord.conex <- na.omit(pueblos()[pueblos()$Nombre%in%conexiones,2:3])
    
    maps::map("world",
              col="#808080", fill=TRUE, bg="#F8F5F0", lwd=0.000001,
              mar=rep(0,4),myborder=0,xlim = c(-30, 59),ylim = c(20, 80)
    )
    inter <- geosphere::gcIntermediate(pueblos()[pueblos()$Nombre==input$select_lugar,2:3],coord.conex,n=6,addStartEnd=TRUE, breakAtDateLine=F)
    for (i in 1:length(inter)){
      lines(inter[[i]], col="#00DFE6", lwd=0.2)
    }
  })
  
  output$fecha1_univ <- renderUI(
    dateInput("dia1_univ", "Día 1:",
              min = min(dias()),
              max  = max(dias()),
              value=dias()[1],
              language="es")
  )
  output$fecha2_univ <- renderUI(
    dateInput("dia2_univ", "Día 2:",
              min = min(dias()),
              max  = max(dias()),
              value=dias()[2],
              language="es")
  )
  
  output$treemap <- renderHighchart({
    conexiones <- datos_univ() %>% dplyr::filter(DIA >= input$dia1_univ & DIA <= input$dia2_univ)
    summ <- conexiones %>% group_by(ORIGEN,DESTINO) %>% summarise(viajes=sum(VIAJES_CONFIRMADOS),asientos=sum(ASIENTOS_CONFIRMADOS))
    
    origen <- head(summ %>% filter(ORIGEN==input$select_lugar) %>% arrange(desc(viajes)),30)
    destino <- head(summ %>% filter(DESTINO==input$select_lugar) %>% arrange(desc(viajes)),30)
    
    if (input$origen_destino =="Origen"){
      hchart(origen, "treemap", hcaes(x = DESTINO, value = asientos, color=viajes)) %>%
        hc_title(text = paste("Principales destinos con <span style=\"color:#1A54ED\"><b>",input$select_lugar ,"</b></span> como origen</br><center><font size='2'>área = pasajeros y color = viajes</font></center>"),
                 useHTML = TRUE) %>%
        hc_chart(backgroundColor="#F8F5F0")
    } else{
      hchart(destino, "treemap", hcaes(x = ORIGEN, value = asientos, color=viajes)) %>%
        hc_title(text = paste("Principales orígenes con <span style=\"color:#1A54ED\"><b>",input$select_lugar ,"</b></span> como destino</br><center><font size='2'>área = pasajeros y color = viajes</font></center>"),
                 useHTML = TRUE) %>%
        hc_chart(backgroundColor="#F8F5F0")
    }
  })
  
  trafico <- reactive({
    conexiones <- datos_univ() %>% dplyr::filter(DIA >= input$dia1_univ & DIA <= input$dia2_univ)
    summ <- conexiones %>% group_by(ORIGEN,DESTINO) %>% summarise(viajes=sum(VIAJES_CONFIRMADOS),asientos=sum(ASIENTOS_CONFIRMADOS))
    return(sum(summ$viajes,na.rm=T)+sum(summ$asientos,na.rm=T))
  })
  output$output_6 <- renderUI({
    variables <- names(datos_blablacar())[5:11]
    names(variables) <- c("Precio / km", "Asientos ofertados", "Asientos confirmados", "Viajes ofertados", "Viajes confirmados", "Ofertantes", "Ofertantes nuevos")
    selectInput("variable_4", paste("Selecciona una variable:"), choices = variables, selected = variables[1], width = "65%")
  })
  output$output_7 <- renderUI({
    variable <- datos_blablacar()[,which(names(datos_blablacar()) == input$variable_4)][[1]]
    if(input$variable_4 == "IMP_KM"){
      nombre <- "Precio / km"
    }
    else if(input$variable_4 == "ASIENTOS_OFERTADOS"){
      nombre <- "Asientos ofertados"
    }
    else if(input$variable_4 == "ASIENTOS_CONFIRMADOS"){
      nombre <- "Asientos confirmados"
    }
    else if(input$variable_4 == "VIAJES_OFERTADOS"){
      nombre <- "Viajes ofertados"
    }
    else if(input$variable_4 == "VIAJES_CONFIRMADOS"){
      nombre <- "Viajes confirmados"
    }
    else if(input$variable_4 == "OFERTANTES"){
      nombre <- "Ofertantes"
    }
    else{
      nombre <- "Ofertantes nuevos"
    }
    
    if(input$variable_4 == "ASIENTOS_OFERTADOS"){
      texto <- tags$div(tags$h4(paste("Resumen breve de la variable:", nombre)), tags$br(),
                        tags$ol(
                          tags$li(paste("Contiene", sum(is.na(variable)), "NA's en total.")), 
                          tags$li(paste("Presenta una media de", round(mean(variable, na.rm = TRUE),2), "así como una mediana igual a", round(median(variable, na.rm = TRUE),2) , "para aquellos valores disponibles.")), 
                          tags$li(paste("Presenta un máximo de", round(max(variable, na.rm = TRUE),2), "así como un mínimo de", round(min(variable, na.rm = TRUE),2)), "."),
                          tags$h5("¡Cuidado!: Parece que hay valores negativos para esta variable siendo que parece ser una característica que no puede admitir tales valores.", style = "color: red")
                        ))
    }
    else{
      texto <- tags$div(tags$h4(paste("Resumen breve de la variable:", nombre)), tags$br(),
                        tags$ol(
                          tags$li(paste("Contiene", sum(is.na(variable)), "NA's en total.")), 
                          tags$li(paste("Presenta una media de", round(mean(variable, na.rm = TRUE),2), "así como una mediana igual a", round(median(variable, na.rm = TRUE),2) , "para aquellos valores disponibles.")), 
                          tags$li(paste("Presenta un máximo de", round(max(variable, na.rm = TRUE),2), "así como un mínimo de", round(min(variable, na.rm = TRUE),2)), ".")
                        ))
    }
  })
  
  output$trafico <- renderText({
    print(paste(as.character(trafico())," personas"))
  })
  output$impacto <- renderText({
    poblacion <- municipios()[municipios()$Nombre==input$select_lugar,4]
    factorr <- trafico()/poblacion*100
    print(paste(as.character(round(factorr,2)),"%"))
  })
  
}