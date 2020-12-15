source("R/SCAN.R", local = TRUE)
source("R/ERA5.R", local = TRUE)
source("R/GLDAS.R", local = TRUE)
source("R/GRIDMET.R", local = TRUE)
source("R/NOAA NCDC.R", local = TRUE)
source("R/microclimUS.R", local = TRUE)
source("R/microclim.R", local = TRUE)
source("R/USCRN.R", local = TRUE)


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

grabMapData <- function(methods, inputVar, month, date, hour) {
  if (methods == "ERA5") {
    data <- mapERA(inputVar, month, date, hour)
  } else if (methods == "GLDAS") {
    data <- mapGLDAS(inputVar, month, date, hour)
  } else if (methods == "GRIDMET") {
    data <- mapGRID(inputVar, month, date)
  } else if (methods == "microclimUS") {
    data <- mapmicroUS(inputVar, month, date, hour)
  } else if (methods == "microclim") {
    data <- mapmicro(inputVar, month, hour)
  }
  return (data)
}

variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Snow")

varsDf <- data.frame(row.names = c(variables, "Tmin"), 
                     "SCAN" = c(18, 3, 22, 9, 8, NA, 4),
                     "ERA5" = c(4, 3, 5, 6, 1, NA, NA),
                     "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "Lwnet_tavg", "Wind_f_inst", NA, NA),
                     "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", NA, "tmin"),
                     "NOAA_NCDC" = c(NA, "TMAX", NA, NA, NA, "SNOW", "TMIN"),
                     "microclima" = c(NA, NA, NA, NA, NA, NA, NA),
                     "SNODAS" = c(NA, NA, NA, NA, NA, NA, NA),
                     "microclimUS" = c("soil0cm_0pctShade", "TA200cm", "soil100cm_0pctShade", "SOLR", NA, NA, NA),
                     "microclim" = c("D0cm_soil_0", "TA120cm", "D100cm_soil_0", "SOLR", "V1cm", NA, NA),
                     "USCRN" = c("SURFACE_TEMPERATURE", "AIR_TEMPERATURE", NA, "SOLAR_RADIATION", "WIND_1_5", NA, NA))

nameDf <- data.frame(row.names = variables, 
                     "SCAN" = c("Daily average soil temperature 2 in below ground", "Daily Tmax and Tmin", "Daily average soil temperature 1 m below ground", "Daily average solar radiation", "Daily average wind speed", NA),
                     "ERA5" = c("Hourly skin temperature", "Hourly air temperature 2 m above ground", "Hourly soil temperature 28-100 cm below ground", "Hourly surface net solar radiation", "Hourly wind speed 10 m above ground", NA),
                     "GLDAS" = c("3-hourly average surface skin temperature", "3-hourly average air temperature", "3-hourly soil temperature 40-100 cm below ground", "3-hourly net longwave radiation flux", "3-hourly average wind speed", NA),
                     "GRIDMET" = c(NA, "Daily Tmax and Tmin", NA, "Daily mean shortwave radiation at surface", "Daily mean wind speed", NA),
                     "NOAA_NCDC" = c(NA, "Daily Tmax and Tmin", NA, NA, NA, "SNOW"),
                     "microclima" = c(NA, NA, NA, NA, NA, NA),
                     "SNODAS" = c(NA, NA, NA, NA, NA, NA),
                     "microclimUS" = c("Hourly surface temperature (0% shade)", "Hourly air temperature 2 m above ground", "Hourly soil temperature 1 m below ground (0 % shade)", "Hourly solar radiation (horizontal ground)", NA, NA),
                     "microclim" = c("Substrate temperature (soil surface 0 % shade)", "Air temperature 1.2 m above ground", "Soil temperature 1 m below ground", "Solar radiation", "Wind speed 1 cm above ground", NA),
                     "USCRN" = c("Average infrared surface temperature", "Air temperature", NA, "Average global solar radiation received", "Wind speed 1.5 m above ground", NA))

methods <- colnames(varsDf)


shinyServer <- function(input, output, session) {
  
  output$methodsOutput <- renderUI({
    index <- which(is.na(varsDf[input$var, ]))
    pickerInput("methods", "Methods", choices = methods[-index], selected = methods[-index][c(1, 2)], multiple = T,
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
         "<br><b>Time:</b> ", month, "1st - 31st")
  })
  
  
  output$plot <- renderPlotly({
    validate(
      need(input$methods, "Select methods")
    )
    
    if (input$var == "Wind speed") {
      unit <- "(m/s)"
    } else if (input$var == "Radiation") {
      unit <- HTML("(W/m<sup>2</sup>)")
    } else if (input$var == "Snow") {
      unit <- "(m)"
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
        if (!is.na(inputVar)) {
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
  
  #______________________________________________________________________________________
  
  # -109, -102, 37, 41
  
  output$mapMethodsOutput1 <- renderUI({
    index <- c(which(is.na(varsDf[input$mapVar, ])), 1, 5, 6, 7, 10)
    pickerInput("mapMethods1", "Method 1", choices = methods[c(-index)], selected = methods[c(-index)][1], 
                options = list(style = "btn-success", `actions-box` = TRUE))
  })
  
  output$mapMethodsOutput2 <- renderUI({
    index <- c(which(is.na(varsDf[input$mapVar, ])), 1, 5, 6, 7, 10)
    pickerInput("mapMethods2", "Method 2", choices = methods[c(-index)], selected = methods[c(-index)][2], 
                options = list(style = "btn-success", `actions-box` = TRUE))
  })

  
  rasterData1 <- reactive({
    validate(
      need(input$mapMethods1, "")
    )
    
    inputVar1 <- varsDf[input$mapVar, input$mapMethods1]
    rasterData1 <- grabMapData(input$mapMethods1, inputVar1, input$month, as.numeric(input$date), as.numeric(input$hour))
    
  })
  
  rasterData2 <- reactive({
    validate(
      need(input$mapMethods2, "")
    )
    
    inputVar2 <- varsDf[input$mapVar, input$mapMethods2]
    rasterData2 <- grabMapData(input$mapMethods2, inputVar2, input$month, as.numeric(input$date), as.numeric(input$hour))
    
  })
  
  
  output$map1 <- renderLeaflet({
    raster <- rasterData1()
    
    if (input$mapVar == "Wind speed") {
      unit <- "(m/s)"
    } else if (input$mapVar == "Radiation") {
      unit <- HTML("(W/m<sup>2</sup>)")
    } else {
      unit <- "(°C)"
    }
    
    pal <- colorNumeric(palette = viridis(5),
                        domain = c(min(minValue(raster), minValue(rasterData2())), max(maxValue(raster), maxValue(rasterData2()))),
                        na.color = "transparent"
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(x = raster, colors = pal, group = "Climatic variable", opacity = 1) %>%
      setView(lng = -105.5, lat = 39, zoom = 6) %>%
      addLegend(pal = pal,
                opacity = 1,
                values = c(minValue(raster), maxValue(raster)),
                position = "bottomright",
                title = paste(input$mapVar), unit)
  })
  
  
  output$map2 <- renderLeaflet({

    raster <- rasterData2()
    
    pal <- colorNumeric(palette = viridis(5),
                        domain = c(min(minValue(rasterData1()), minValue(raster)), max(maxValue(rasterData1()), maxValue(raster))),
                        na.color = "transparent"
    )
    
    if (input$mapVar == "Wind speed") {
      unit <- "(m/s)"
    } else if (input$mapVar == "Radiation") {
      unit <- HTML("(W/m<sup>2</sup>)")
    } else {
      unit <- "(°C)"
    }
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(x = raster, colors = pal, group = "Climatic variable", opacity = 1) %>%
      setView(lng = -105.5, lat = 39, zoom = 6) %>%
      addLegend(pal = pal,
                opacity = 1,
                values = c(minValue(raster), maxValue(raster)),
                position = "bottomright",
                title = paste(input$mapVar), unit)
  })
}
