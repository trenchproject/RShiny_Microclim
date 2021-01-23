source("R/SCAN.R", local = TRUE)
source("R/ERA5.R", local = TRUE)
source("R/GLDAS.R", local = TRUE)
source("R/GRIDMET.R", local = TRUE)
source("R/NOAA NCDC.R", local = TRUE)
source("R/microclimUS.R", local = TRUE)
source("R/microclim.R", local = TRUE)
source("R/USCRN.R", local = TRUE)
source("cicerone.R", local= TRUE)


grabAnyData <- function(methods, inputVar, loc, month) {
  if (methods == "SCAN") {
    data <- grabSCAN(inputVar, loc, month)
  } else if (methods == "ERA5") {
    data <- grabERA(inputVar, loc, month)
  } else if (methods == "GLDAS") {
    data <- grabGLDAS(inputVar, loc, month)
  } else if (methods == "GRIDMET") {
    data <- grabGRID(inputVar, loc, month)
  } else if (methods == "NOAA_NCDC") {
    data <- grabNOAA(inputVar, loc, month)
  } else if (methods == "microclimUS") {
    data <- grabmicroUS(inputVar, loc, month)
  } else if (methods == "microclim") {
    data <- grabmicro(inputVar, loc, month)
  } else if (methods == "USCRN") {
    data <- grabUSCRN(inputVar, loc, month)
  }
  return (data)
}

grabMapData <- function(methods, inputVar, month, date) {
  if (methods == "ERA5") {
    data <- mapERA(inputVar, month, date)
  } else if (methods == "GLDAS") {
    data <- mapGLDAS(inputVar, month, date)
  } else if (methods == "GRIDMET") {
    data <- mapGRID(inputVar, month, date)
  } else if (methods == "microclimUS") {
    data <- mapmicroUS(inputVar, month, date)
  } else if (methods == "microclim") {
    data <- mapmicro(inputVar, month)
  }
  return (data)
}

variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Precipitation", "Relative humidity", "Soil moisture")

varsDf <- data.frame(row.names = c(variables, "Tmin"),
                     "SCAN" = c(18, 3, 22, 9, 8, 5, 6, NA, 4),
                     "ERA5" = c(4, 3, 5, 6, 1, 7, NA, NA, 8),
                     "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "Lwnet_tavg", "Wind_f_inst", "Rainf_f_tavg", "Qair_f_inst", "SoilMoi40_100cm_inst", "Tmin"),
                     "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", "prcp", NA, NA, "tmin"),
                     "NOAA_NCDC" = c(NA, "TMAX", NA, NA, NA, "PRCP", NA, NA, "TMIN"),
                     "microclimUS" = c("soil0cm_0pctShade", "TA200cm", "soil100cm_0pctShade", "SOLR", NA, NA, "RH200cm", "moist100cm_0pctShade", "Tmin"),
                     "microclim" = c("D0cm_soil_0", "TA120cm", "D100cm_soil_0", "SOLR", "V1cm", NA, "RH120cm", NA, "Tmin"),
                     "USCRN" = c("SURFACE_TEMPERATURE", "AIR_TEMPERATURE", NA, "SOLAR_RADIATION", "WIND_1_5", "PRECIPITATION", "RELATIVE_HUMIDITY", NA, NA))

nameDf <- data.frame(row.names = variables, 
                     "SCAN" = c("Daily average soil temperature 2 in below ground", "Daily Tmax and Tmin", "Daily average soil temperature 1 m below ground", "Daily average solar radiation", "Daily average wind speed", "Precipitation increment", "Hourly mean humidity", NA),
                     "ERA5" = c("Hourly skin temperature", "Hourly air temperature 2 m above ground", "Hourly soil temperature 28-100 cm below ground", "Hourly surface net solar radiation", "Hourly wind speed 10 m above ground", "Total precipitation", NA, NA),
                     "GLDAS" = c("3-hourly average surface skin temperature", "3-hourly average air temperature", "3-hourly average soil temperature 40-100 cm below ground", "3-hourly net longwave radiation flux", "3-hourly average wind speed", "Total precipitation", NA, "3-hourly average soil moisture 40-100 cm below ground"),
                     "GRIDMET" = c(NA, "Daily Tmax and Tmin", NA, "Daily mean shortwave radiation at surface", "Daily mean wind speed", "Daily precipitation amount", NA, NA),
                     "NOAA_NCDC" = c(NA, "Daily Tmax and Tmin", NA, NA, NA, "Precipitation", NA, NA),
                     "microclimUS" = c("Hourly surface temperature (0% shade)", "Hourly air temperature 2 m above ground", "Hourly soil temperature 1 m below ground (0 % shade)", "Hourly solar radiation (horizontal ground)", NA, NA, "Hourly relative humidity 2 m above ground", "Hourly soil moisture 1 m below ground (0 % shade)"),
                     "microclim" = c("Substrate temperature (soil surface 0 % shade)", "Air temperature 1.2 m above ground", "Soil temperature 1 m below ground", "Solar radiation", "Wind speed 1 cm above ground", NA, "Relative humidity 1.2 m above ground", NA),
                     "USCRN" = c("Sub-hourly infrared surface temperature", "Sub-hourly air temperature", NA, "Average global solar radiation received", "Wind speed 1.5 m above ground", "Sub-hourly precipitation", "Sub-hourly relative humidity"))
methods <- colnames(varsDf)


shinyServer <- function(input, output, session) {
  
  observeEvent(input$tour1, guide1$init()$start())
  
  observeEvent(input$reset1, {
    reset("page")
  })
  
  observeEvent(input$tour2, guide2$init()$start())
  
  observeEvent(input$reset2, {
    reset("page")
  })
  
  
  output$methodsOutput <- renderUI({
    index <- which(!is.na(varsDf[input$var, ]))
    pickerInput("methods", "Datasets", choices = methods[index], selected = methods[index][c(1, 2)], multiple = T,
                options = list(style = "btn-success", `actions-box` = TRUE))

  })
  
  output$info <- renderText({
    if (input$loc == "WA") {
      station <- "Lind #1 (-118.57°, 47°)"
      loc <- "Adams county, WA 1640ft"
    } else if (input$loc == "CO") {
      station <- "Nunn #1 (-104.73°, 40.87°)"
      loc <- "Weld county, CO 5900ft"
    } else if (input$loc == "PR") {
      station <- "Maricao Forest (-67°, 18.15°)"
      loc <- "Mayaguez, Puerto Rico 2450ft"
    }
    
    month <- ifelse(input$season == 1, "January", "July")
    
    text <- ""
    for (method in input$methods) {
      text <- paste0(text, "<br><b>", method, ":</b> ", nameDf[input$var, method])
    }
    HTML("<b><u>Data showing</u></b>",
         text,
         "<br><br><b>Station name:</b> ", station, 
         "<br><b>Location:</b> ", loc,
         "<br><b>Time:</b> ", month, "1st - 31st, 2017")
  })
  
  
  output$plot <- renderPlotly({
    validate(
      need(input$methods, "Select datasets")
    )
    
    if (input$var == "Wind speed") {
      unit <- "(m/s)"
    } else if (input$var == "Radiation") {
      unit <- HTML("(W/m<sup>2</sup>)")
    } else if (input$var == "Precipitation") {
      unit <- "(mm)"
    } else if (input$var %in% c("Relative humidity", "Soil moisture")){
      unit <- "(%)"
    } else {
      unit <- "(°C)"
    } 
    
    colors <- c('#b35806', '#542788', '#8073ac', '#e08214', '#b2abd2', '#fdb863', '#fee0b6', '#d8daeb')
    p <- plot_ly() %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = paste(input$var, unit)))
    i = 0
    for (method in input$methods) {
      i = i + 1
      inputVar <- varsDf[input$var, method]
      df <- grabAnyData(method, inputVar, input$loc, input$season)
      p <- p %>% add_lines(x = df$Date, y = df$Data, name = method, line = list(color = colors[i]))
    }
    
    # Adding Tmin when Air temperature is selected
    if (input$var == "Air temperature") {
      i = 0
      for (method in input$methods) {
        i = i + 1
        inputVar <- varsDf["Tmin", method]
        if (method %in% c("GRIDMET", "NOAA_NCDC")) {
          df <- grabAnyData(method, inputVar, input$loc, input$season)
          p <- p %>%
            add_lines(x = df$Date, y = df$Data, name = paste(method, "Tmin"), line = list(color = colors[i]))
        }
      }
    }
    
    # if (input$var == "Suraface temperature" && input$methods %in% "GRIDMET") {
    #   df <- grabAnyData("GRIDMET", "tmin", input$loc, input$season)
    #   p <- p %>%
    #     add_lines(x = df$Date, y = df$Data, name = paste(method, "Tmin"), line = list(color = colors[i]))
    #   
    # }
    
    p
    
  })
  
  
  output$minimap <- renderLeaflet({
    
    x = c(-117.53, -104.7552, -66.98880)
    y = c(47.42, 40.8066, 18.15110)
    text = c("Spokane, WA", "Nunn, CO", "Maricao forest, Puerto Rico")
    names(x) = names(y) = names(text) = c("WA", "CO", "PR")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(lng = x[input$loc], lat = y[input$loc], popup = HTML(text[input$loc])) %>%
      setView(lng = -97.5, lat = 39, zoom = 2.5)
  })
  
  #______________________________________________________________________________________
  
  # -109, -102, 37, 41
  
  output$mapMethodsOutput1 <- renderUI({
    # USCRN, NOAA, 
    index <- c(which(is.na(varsDf[input$mapVar, ])), 1, 5, 8)
    pickerInput("mapMethods1", "Dataset 1", choices = methods[c(-index)], selected = methods[c(-index)][1], 
                options = list(style = "btn-success", `actions-box` = TRUE))
  })
  
  output$mapMethodsOutput2 <- renderUI({
    index <- c(which(is.na(varsDf[input$mapVar, ])), 1, 5, 8)
    pickerInput("mapMethods2", "Dataset 2", choices = methods[c(-index)], selected = methods[c(-index)][2], 
                options = list(style = "btn-success", `actions-box` = TRUE))
  })

  
  rasterData1 <- reactive({
    validate(
      need(input$mapMethods1, "")
    )
    
    inputVar1 <- varsDf[input$mapVar, input$mapMethods1]
    rasterData1 <- grabMapData(input$mapMethods1, inputVar1, input$month, as.numeric(input$date))
    
  })
  
  rasterData2 <- reactive({
    validate(
      need(input$mapMethods2, "")
    )

    inputVar2 <- varsDf[input$mapVar, input$mapMethods2]
    rasterData2 <- grabMapData(input$mapMethods2, inputVar2, input$month, as.numeric(input$date))

  })
  
  rasterdif <- reactive({
    raster1 <- rasterData1()
    raster2 <- rasterData2()
    
    if (res(raster1)[1] > res(raster2)[1]) {
      rasterdif <- abs(resample(raster2, raster1) - raster1)
    } else {
      rasterdif <- abs(resample(raster1, raster2) - raster2)
    }
    rasterdif
  })
  
  
  output$mymap <- renderLeaflet({
    raster1 <- rasterData1()
    raster2 <- rasterData2()
    
    if (input$mapVar == "Wind speed") {
      unit <- "(m/s)"
    } else if (input$mapVar == "Radiation") {
      unit <- HTML("(W/m<sup>2</sup>)")
    } else {
      unit <- "(°C)"
    }
    
    min <- min(minValue(raster1), minValue(raster2))
    max <- max(maxValue(raster1), maxValue(raster2))

    pal <- colorNumeric(palette = viridis(5),
                        domain = c(min, max),
                        na.color = "transparent")
    
    paldif <- colorNumeric(palette = viridis(5),
                           domain = c(minValue(rasterdif()), maxValue(rasterdif())),
                           na.color = "transparent")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(x = raster1, colors = pal, group = input$mapMethods1, opacity = 1) %>%
      addRasterImage(x = raster2, colors = pal, group = input$mapMethods2, opacity = 1) %>%
      addRasterImage(x = rasterdif(), colors = paldif, group = "Difference", opacity = 1) %>%
      setView(lng = -105.5, lat = 39, zoom = 6) %>%
      addLayersControl(baseGroups = c(input$mapMethods1, input$mapMethods2, "Difference")) %>%
      addLegend(pal = pal,
                opacity = 1,
                values = c(min, max),
                position = "bottomright",
                title = paste(input$mapVar, unit))
  })
  
  observeEvent(input$mymap_groups, {
    

    raster1 <- rasterData1()
    raster2 <- rasterData2()
    
    if (input$mapVar == "Wind speed") {
      unit <- "(m/s)"
    } else if (input$mapVar == "Radiation") {
      unit <- HTML("(W/m<sup>2</sup>)")
    } else {
      unit <- "(°C)"
    }
    
    min <- min(minValue(raster1), minValue(raster2))
    max <- max(maxValue(raster1), maxValue(raster2))
    
    pal <- colorNumeric(palette = viridis(5),
                        domain = c(min, max),
                        na.color = "transparent")
    
    paldif <- colorNumeric(palette = viridis(5),
                           domain = c(minValue(rasterdif()), maxValue(rasterdif())),
                           na.color = "transparent")
    
    if (input$mymap_groups == "Difference") {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = paldif, 
                  opacity = 1,
                  values = c(minValue(rasterdif()), maxValue(rasterdif())),
                  group = "Difference",
                  position = "bottomright",
                  title = paste("Difference", unit))
    } else {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = pal,
                  opacity = 1,
                  values = c(min, max),
                  position = "bottomright",
                  title = paste(input$mapVar, unit))
    }
  })

}
