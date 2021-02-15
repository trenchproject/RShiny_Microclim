source("R/SCAN.R", local = TRUE)
source("R/ERA5.R", local = TRUE)
source("R/GLDAS.R", local = TRUE)
source("R/GRIDMET.R", local = TRUE)
source("R/NOAA NCDC.R", local = TRUE)
source("R/microclimUS.R", local = TRUE)
source("R/microclim.R", local = TRUE)
source("R/SNODAS.R", local = TRUE)
#source("R/USCRN.R", local = TRUE)
source("R/NicheMapR.R", local = TRUE)
source("cicerone.R", local= TRUE)
source("functions.R", local = TRUE)
library(TrenchR)

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
  # } else if (methods == "USCRN") {
  #   data <- grabUSCRN(inputVar, loc, month)
  } else if (methods == "SNODAS") {
    data <- grabSNODAS(inputVar, loc, month)
  } else if (methods == "NicheMapR") {
    data <- grabNicheR(inputVar, loc, month)
  }
  return (data)
}

# grabMapData <- function(methods, inputVar, month, date) {
#   if (methods == "ERA5") {
#     data <- mapERA(inputVar, month, date)
#   } else if (methods == "GLDAS") {
#     data <- mapGLDAS(inputVar, month, date)
#   } else if (methods == "GRIDMET") {
#     data <- mapGRID(inputVar, month, date)
#   } else if (methods == "microclimUS") {
#     data <- mapmicroUS(inputVar, month, date)
#   } else if (methods == "microclim") {
#     data <- mapmicro(inputVar, month)
#   }
#   return (data)
# }

grabMapData <- function(methods, inputVar, month) {
  if (methods == "SCAN") {
    data <- mapSCAN(inputVar, month)
  } else if (methods == "ERA5") {
    data <- mapERA(inputVar, month)
  } else if (methods == "GLDAS") {
    data <- mapGLDAS(inputVar, month)
  } else if (methods == "GRIDMET") {
    data <- mapGRID(inputVar, month)
  } else if (methods == "NOAA_NCDC") {
    data <- mapGRID(inputVar, month)
  } else if (methods == "microclimUS") {
    data <- mapmicroUS(inputVar, month) 
  } else if (methods == "microclim") {
    data <- mapmicro(inputVar, month)
  }
  return (data)
}

variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Precipitation", "Relative humidity", "Soil moisture", "Snow Depth")

varsDf <- data.frame(row.names = c(variables, "Tmin"),
                     "SCAN" = c(18, 3, 22, 7, 10, 5, 6, 17, NA, 4),
                     "ERA5" = c(4, 3, 6, 7, 1, 8, NA, NA, 5, 9),
                     "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "SWdown_f_tavg", "Wind_f_inst", "Rainf_f_tavg", "Qair_f_inst", "SoilMoi40_100cm_inst", "SnowDepth_inst", "Tmin"),
                     "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", "prcp", NA, NA, NA, "tmin"),
                     "NOAA_NCDC" = c(NA, "TMAX", NA, NA, NA, "PRCP", NA, NA, "SNWD", "TMIN"),
                     "microclimUS" = c("soil0cm_0pctShade", "TA200cm", "soil100cm_0pctShade", "SOLR", NA, NA, "RH200cm", "moist100cm_0pctShade", NA, "Tmin"),
                     "microclim" = c("D0cm_soil_0", "TA120cm", "D100cm_soil_0", "SOLR", "V1cm", NA, "RH120cm", NA, NA, "Tmin"),
                     #"USCRN" = c("SURFACE_TEMPERATURE", "AIR_TEMPERATURE", NA, "SOLAR_RADIATION", "WIND_1_5", "PRECIPITATION", "RELATIVE_HUMIDITY", NA, NA, NA),
                     "SNODAS" = c(NA, NA, NA, NA, NA, NA, NA, NA, "SNOWH", NA),
                     "NicheMapR" = c("D0cm", "TAREF", "D100cm", "SOLR", "VREF", NA, "RH", NA, "SNOWDEP", NA))

nameDf <- data.frame(row.names = variables, 
                     "SCAN" = c("Hourly average soil temperature 2 in below ground", "Hourly maximum air temperature", "Hourly average soil temperature 1 m below ground", "Hourly average solar radiation", "Hourly average wind speed", "Precipitation increment", "Hourly average humidity", "Hourly average xsoil moisture 1 m below ground", NA),
                     "ERA5" = c("Hourly skin temperature", "Hourly air temperature 2 m above ground", "Hourly soil temperature 28-100 cm below ground", "Hourly surface net solar radiation", "Hourly wind speed 10 m above ground", "Total precipitation", NA, NA, "Hourly snow depth"),
                     "GLDAS" = c("3-hourly average surface skin temperature", "3-hourly average air temperature", "3-hourly average soil temperature 40-100 cm below ground", "3-hourly net longwave radiation flux", "3-hourly average wind speed", "Total precipitation", "3-hourly relative humidity", "3-hourly average soil moisture 40-100 cm below ground", "3-hourly snow depth"),
                     "GRIDMET" = c(NA, "Daily Tmax and Tmin", NA, "Daily mean shortwave radiation at surface", "Daily mean wind speed", "Daily precipitation amount", NA, NA, NA),
                     "NOAA_NCDC" = c(NA, "Daily Tmax and Tmin", NA, NA, NA, "Daily precipitation", NA, NA, "Daily snow Depth"),
                     "microclimUS" = c("Hourly surface temperature (0% shade)", "Hourly air temperature 2 m above ground", "Hourly soil temperature 1 m below ground (0 % shade)", "Hourly solar radiation (horizontal ground)", NA, NA, "Hourly relative humidity 2 m above ground", "Hourly soil moisture 1 m below ground (0 % shade)", NA),
                     "microclim" = c("Substrate temperature (soil surface 0 % shade)", "Air temperature 1.2 m above ground", "Soil temperature 1 m below ground", "Solar radiation", "Wind speed 1 cm above ground", NA, "Relative humidity 1.2 m above ground", NA, NA),
                     #"USCRN" = c("Sub-hourly infrared surface temperature", "Sub-hourly air temperature", NA, "Average global solar radiation received", "Wind speed 1.5 m above ground", "Sub-hourly precipitation", "Sub-hourly relative humidity", NA, NA),
                     "SNODAS" = c(NA, NA, NA, NA, NA, NA, NA, NA, "Snow depth"),
                     "NicheMapR" = c("Hourly soil temperature at 0cm", "Hourly air temperature 2 m above ground", "Hourly soil temperature 100 cm below ground", "Hourly solar radiation, unshaded", "Hourly wind speed 2 m above ground", NA, "Hourly relative humidity 2 m above ground", NA, "Hourly predicted snow depth"))

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
  
  
  #___________________________________________________________________________________
  
  
  output$datasetOutput <- renderText({
    
    validate(
      need(input$spaCov, "Select spatial coverage"),
      need(input$tempRes, "Select temporal resolution")
    )
    
    dataTable <- readxl::read_xlsx("DatasetTable.xlsx") %>% as.data.frame() %>%
      filter(TempCovStart <= input$tempCov_start | is.na(TempCovStart)) %>%
      filter(TempCovEnd >= input$tempCov_end | is.na(TempCovEnd)) %>%
      filter(SpatCov %in% input$spaCov & TempRes %in% input$tempRes)
    
    for (var in input$varTable) {
      dataTable <- dataTable[dataTable[, var] == "T", ]
    }
    
    text <- ""
    for (i in 1 : nrow(dataTable)) {
      text <- paste0(text, dataTable$Dataset[i], ": ", dataTable$Text[i], "<br>")
    }
    
    HTML(text)
  })
  
  
  #___________________________________________________________________________________
  # Temporal comparison
  
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
    } else if (input$var %in% c("Precipitation", "Snow Depth")) {
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
      
      if (!is.na(inputVar)) {
        if (input$loc != "PR" || !method %in% c("GRIDMET", "microclimUS", "USCRN")) {  # Won't run when PR and the three datasets that don't have data for PR are selected 
          df <- grabAnyData(method, inputVar, input$loc, input$season)
          p <- p %>% add_lines(x = df$Date, y = df$Data, name = method, line = list(color = colors[i]))
        }
      }
    }
    
    # Adding Tmin when Air temperature is selected
    if (input$var == "Air temperature") {
      i = 0
      for (method in input$methods) {
        i = i + 1
        inputVar <- varsDf["Tmin", method]
        if (method %in% c("GRIDMET", "NOAA_NCDC")) { # gridMET and NOAA NCDC have daily Tmax and Tmin
          if (input$loc != "PR" || !method == "GRIDMET") { # gridMET doesn't have data for PR
            df <- grabAnyData(method, inputVar, input$loc, input$season)
            p <- p %>%
              add_lines(x = df$Date, y = df$Data, name = paste(method, "Tmin"), line = list(color = colors[i]))
          }
        }
      }
    }
    
    p
    
  })
  
  
  output$datasetComparison <- renderUI({
    validate(
      need(input$methods, "")
    )

    checkboxGroupButtons("statsOption", "Select two datasets to see their relatedness", choices = input$methods, status = "success", 
                         checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })
  
  output$stats <- renderText({
    validate(
      need(length(input$statsOption) == 2, "Select two datasets\n\n\n")
    )
    # Have to figure out what to do with 3-hourly and daily values. take the average?
    
    # hourly: SCAN, ERA5, microclimUS, NicheMapR
    # 3-hourly: GLDAS
    # daily: gridMET, NOAA NCDC, SNODAS
    # sub-hourly: USCRN
    
    df1 <- grabAnyData(input$statsOption[1], varsDf[input$var, input$statsOption[1]], input$loc, input$season)
    df2 <- grabAnyData(input$statsOption[2], varsDf[input$var, input$statsOption[2]], input$loc, input$season)
    
    if (input$statsOption[1] %in% c("GRIDMET", "NOAA_NCDC", "SNODAS") || input$statsOption[2] %in% c("GRIDMET", "NOAA_NCDC", "SNODAS")) {
      df1$Date <- as.Date(df1$Date)
      df1 <- aggregate(df1$Data, by = list(df1$Date), mean) %>% set_colnames(c("Date", "Data"))
      df2$Date <- as.Date(df2$Date) 
      df2 <- aggregate(df2$Data, by = list(df2$Date), mean) %>% set_colnames(c("Date", "Data"))
    }
    
    # Will have to work on
    # } else if (input$statsOption[1] == "GLDAS" || input$statsOption[2] == "GLDAS") {
    #   
    # }

    colnames(df1)[colnames(df1) == "Data"] <- "Data1"
    colnames(df2)[colnames(df2) == "Data"] <- "Data2"
    
    setDT(df1)
    setDT(df2)
    
    merge <- df1[df2, on = "Date"] %>% 
      na.omit() %>% 
      as.data.frame()
    
    data1 <- merge$Data1
    data2 <- merge$Data2
    
    PCC <- cor.test(x = data1, y = data2, method = "pearson") # Pearson correlation coefficient
    bias <- abs((sum(data1) - sum(data2)) / length(data1))
    RMSE <- sum((data1 - data2)^2) / length(data1) # Root mean square error
      
    HTML("<b>Pearson correlation coefficient:</b> ", signif(unname(PCC$estimate), digits = 2),
         "<br><b>Bias:</b> ", round(bias, digits = 2),
         "<br><b>RMSE:</b> ", round(RMSE, digits = 2))

  })
  
  
  output$statsTable <- renderUI({
    validate(
      need(length(input$statsOption) == 2, "Select two datasets\n\n\n")
    )
    # Have to figure out what to do with 3-hourly and daily values. take the average?
    
    # hourly: SCAN, ERA5, microclimUS, NicheMapR
    # 3-hourly: GLDAS
    # daily: gridMET, NOAA NCDC, SNODAS
    # sub-hourly: USCRN
    
    df1 <- grabAnyData(input$statsOption[1], varsDf[input$var, input$statsOption[1]], input$loc, input$season)
    df2 <- grabAnyData(input$statsOption[2], varsDf[input$var, input$statsOption[2]], input$loc, input$season)
    
    if (input$statsOption[1] %in% c("GRIDMET", "NOAA_NCDC", "SNODAS") || input$statsOption[2] %in% c("GRIDMET", "NOAA_NCDC", "SNODAS")) {
      df1$Date <- as.Date(df1$Date)
      df1 <- aggregate(df1$Data, by = list(df1$Date), mean) %>% set_colnames(c("Date", "Data"))
      df2$Date <- as.Date(df2$Date) 
      df2 <- aggregate(df2$Data, by = list(df2$Date), mean) %>% set_colnames(c("Date", "Data"))
    }
    
    # Will have to work on
    # } else if (input$statsOption[1] == "GLDAS" || input$statsOption[2] == "GLDAS") {
    #   
    # }
    
    colnames(df1)[colnames(df1) == "Data"] <- "Data1"
    colnames(df2)[colnames(df2) == "Data"] <- "Data2"
    
    setDT(df1)
    setDT(df2)
    
    merge <- df1[df2, on = "Date"] %>% 
      na.omit() %>% 
      as.data.frame()
    
    data1 <- merge$Data1
    data2 <- merge$Data2
    
    plot_correlation()
  })
  
  
  output$minimap <- renderLeaflet({
    
    x = c(-118.5657, -104.7552, -66.98880)
    y = c(47.0022, 40.8066, 18.15110)
    text = c("Lind, WA", "Nunn, CO", "Maricao forest, Puerto Rico")
    names(x) = names(y) = names(text) = c("WA", "CO", "PR")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(lng = x[input$loc], lat = y[input$loc], popup = HTML(text[input$loc])) %>%
      setView(lng = -97.5, lat = 39, zoom = 2.5)
  })
  
  #______________________________________________________________________________________
  # Spatial comparison
  
  # -109, -102, 37, 41
  
  output$mapMethodsOutput <- renderUI({
    # USCRN, NOAA, 
    # index <- c(which(is.na(varsDf[input$mapVar, ])), 1, 5, 8)
    pickerInput("mapMethods", "Dataset to compare", choices = methods[-1], selected = methods[-1][1], # all the dataset except for SCAN
                options = list(style = "btn-success", `actions-box` = TRUE))
  })
  
  # output$mapMethodsOutput2 <- renderUI({
  #   # index <- c(which(is.na(varsDf[input$mapVar, ])), 1, 5, 8)
  #   pickerInput("mapMethods2", "Dataset 2", choices = methods, selected = methods[2], 
  #               options = list(style = "btn-success", `actions-box` = TRUE))
  # })

  
  statsTable <- reactive({
    
    validate(
      need(input$mapMethods, "")
    )
    
    stations <- readxl::read_xlsx("SCAN_stations.xlsx") %>% as.data.frame()
    

    SCAN <- grabMapData("SCAN", varsDf[input$mapVar, "SCAN"], input$month)
    # SCAN <- grabMapData("SCAN", 3, 1)
    if (input$mapMethods %in% c("GRIDMET", "NOAA_NCDC", "SNODAS")) {
      SCAN$Date <- as.Date(SCAN$Date)
      SCAN <- aggregate(list(SCAN[, c(-1, -2)]), by = list(SCAN$Date), mean) %>% # first two columns are date and hour
        set_colnames(c("Date", stations$Station))
      
    }
    
    inputVar <- varsDf[input$mapVar, input$mapMethods]
    
    mapDf <- grabMapData(input$mapMethods, inputVar, input$month)
    # mapDf <- grabMapData("ERA5", 3, 1)
    
    stats <- cbind(stations, 
                   "Bias" = NA,
                   "RMSE" = NA)
    for (station in stations$Station) {
      merged <- merge(SCAN[, c("Date", station)], mapDf[, c("Date", station)], by = "Date") %>%
        set_colnames(c("Date", "Data1", "Data2")) %>%
        na.omit()

      bias <- abs((sum(merged$Data1) - sum(merged$Data2)) / nrow(merged))
      stats[stats$Station == station, "Bias"] <- bias
      
      RMSE <- sum((merged$Data1 - merged$Data2)^2) / nrow(merged) # Root mean square error
      stats[stats$Station == station, "RMSE"] <- RMSE
    }

    stats
  })
  
  output$mymap <- renderLeaflet({
    
    validate(
      need(statsTable(), "")
    )
  
    stats <- statsTable()
    
    maxRawBias <- max(stats$Bias)
    
    maxRawRMSE <- max(stats$RMSE)
    
    roundUp <- function (percentile, category = "B") {
      if (category == "B") {
        return (ceiling(maxRawBias * percentile * 10) / 10)
      } else if (category == "R") {
        return (ceiling(maxRawRMSE * percentile * 10) / 10)
      }
    }

    stats$BiasCat <- cut(stats$Bias,
                      c(0, roundUp(0.25), roundUp(0.5), roundUp(0.75), roundUp(1)), include.lowest = T,
                      labels = c(paste0("0 - ", roundUp(0.25)), paste0(roundUp(0.25), " - ", roundUp(0.5)), paste0(roundUp(0.5), " - ", roundUp(0.75)), paste0(roundUp(0.75), " - ", roundUp(1))))

    biasCol <- colorFactor(palette = c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c'), stats$BiasCat)
    
    
    stats$RMSECat <- cut(stats$RMSE,
                         c(0, roundUp(0.25, "R"), roundUp(0.5, "R"), roundUp(0.75, "R"), roundUp(1, "R")), include.lowest = T,
                         labels = c(paste0("0 - ", roundUp(0.25, "R")), paste0(roundUp(0.25, "R"), " - ", roundUp(0.5, "R")), paste0(roundUp(0.5, "R"), " - ", roundUp(0.75, "R")), paste0(roundUp(0.75, "R"), " - ", roundUp(1, "R"))))
    
    rmseCol <- colorFactor(palette = c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c'), stats$RMSECat)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(data = stats, lng = ~Lon, lat = ~Lat,
                       color = ~biasCol(stats$BiasCat),
                       stroke = FALSE, 
                       radius = 7, 
                       fillOpacity = 1, 
                       group = "Bias",
                       popup = paste0("Bias:", round(stats$Bias, digits = 2))) %>%
      addCircleMarkers(data = stats, lng = ~Lon, lat = ~Lat,
                       color = ~rmseCol(stats$RMSECat),
                       stroke = FALSE,
                       radius = 7, 
                       fillOpacity = 1, 
                       group = "RMSE",
                       popup = paste0("RMSE:", round(stats$RMSE, digits = 2))) %>%
      addLayersControl(baseGroups = c("Bias", "RMSE")) %>%
      addLegend(pal = biasCol,
                opacity = 1,
                values = stats$BiasCat,
                position = "bottomright",
                title = "Bias")
  })
  
  observeEvent(input$mymap_groups, {
    
    stats <- statsTable()
    
    maxRawBias <- max(stats$Bias)
    
    maxRawRMSE <- max(stats$RMSE)
    
    roundUp <- function (percentile, category = "B") {
      if (category == "B") {
        return (ceiling(maxRawBias * percentile * 10) / 10)
      } else if (category == "R") {
        return (ceiling(maxRawRMSE * percentile * 10) / 10)
      }
    }
    
    stats$BiasCat <- cut(stats$Bias,
                         c(0, roundUp(0.25), roundUp(0.5), roundUp(0.75), roundUp(1)), include.lowest = T,
                         labels = c(paste0("0 - ", roundUp(0.25)), paste0(roundUp(0.25), " - ", roundUp(0.5)), paste0(roundUp(0.5), " - ", roundUp(0.75)), paste0(roundUp(0.75), " - ", roundUp(1))))
    
    biasCol <- colorFactor(palette = c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c'), stats$BiasCat)
    
    
    stats$RMSECat <- cut(stats$RMSE,
                         c(0, roundUp(0.25, "R"), roundUp(0.5, "R"), roundUp(0.75, "R"), roundUp(1, "R")), include.lowest = T,
                         labels = c(paste0("0 - ", roundUp(0.25, "R")), paste0(roundUp(0.25, "R"), " - ", roundUp(0.5, "R")), paste0(roundUp(0.5, "R"), " - ", roundUp(0.75, "R")), paste0(roundUp(0.75, "R"), " - ", roundUp(1, "R"))))
    
    rmseCol <- colorFactor(palette = c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c'), stats$RMSECat)
    
    
    if (input$mymap_groups == "Bias") {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = biasCol, 
                  opacity = 1,
                  values = stats$BiasCat,
                  group = "Bias legend",
                  position = "bottomright",
                  title = "Bias")
    } else { # RMSE
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = rmseCol,
                  opacity = 1,
                  values = stats$RMSECat,
                  group = "RMSE legend",
                  position = "bottomright",
                  title = "RMSE")
    }
  })
  # rasterData1 <- reactive({
  #   validate(
  #     need(input$mapMethods1, "")
  #   )
  #   
  #   inputVar1 <- varsDf[input$mapVar, input$mapMethods1]
  #   rasterData1 <- grabMapData(input$mapMethods1, inputVar1, input$month, as.numeric(input$date))
  #   
  # })
  # 
  # rasterData2 <- reactive({
  #   validate(
  #     need(input$mapMethods2, "")
  #   )
  # 
  #   inputVar2 <- varsDf[input$mapVar, input$mapMethods2]
  #   rasterData2 <- grabMapData(input$mapMethods2, inputVar2, input$month, as.numeric(input$date))
  # 
  # })
  # 
  # rasterdif <- reactive({
  #   raster1 <- rasterData1()
  #   raster2 <- rasterData2()
  #   
  #   if (res(raster1)[1] > res(raster2)[1]) {
  #     rasterdif <- abs(resample(raster2, raster1) - raster1)
  #   } else {
  #     rasterdif <- abs(resample(raster1, raster2) - raster2)
  #   }
  #   rasterdif
  # })
  # 
  # 
  # output$mymap <- renderLeaflet({
  #   raster1 <- rasterData1()
  #   raster2 <- rasterData2()
  #   
  #   if (input$mapVar == "Wind speed") {
  #     unit <- "(m/s)"
  #   } else if (input$mapVar == "Radiation") {
  #     unit <- HTML("(W/m<sup>2</sup>)")
  #   } else {
  #     unit <- "(°C)"
  #   }
  #   
  #   min <- min(minValue(raster1), minValue(raster2))
  #   max <- max(maxValue(raster1), maxValue(raster2))
  # 
  #   pal <- colorNumeric(palette = viridis(5),
  #                       domain = c(min, max),
  #                       na.color = "transparent")
  #   
  #   paldif <- colorNumeric(palette = viridis(5),
  #                          domain = c(minValue(rasterdif()), maxValue(rasterdif())),
  #                          na.color = "transparent")
  #   
  #   leaflet() %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addRasterImage(x = raster1, colors = pal, group = input$mapMethods1, opacity = 1) %>%
  #     addRasterImage(x = raster2, colors = pal, group = input$mapMethods2, opacity = 1) %>%
  #     addRasterImage(x = rasterdif(), colors = paldif, group = "Difference", opacity = 1) %>%
  #     setView(lng = -105.5, lat = 39, zoom = 6) %>%
  #     addLayersControl(baseGroups = c(input$mapMethods1, input$mapMethods2, "Difference")) %>%
  #     addLegend(pal = pal,
  #               opacity = 1,
  #               values = c(min, max),
  #               position = "bottomright",
  #               title = paste(input$mapVar, unit))
  # })
  # 
  # observeEvent(input$mymap_groups, {
  #   
  #   raster1 <- rasterData1()
  #   raster2 <- rasterData2()
  #   
  #   if (input$mapVar == "Wind speed") {
  #     unit <- "(m/s)"
  #   } else if (input$mapVar == "Radiation") {
  #     unit <- HTML("(W/m<sup>2</sup>)")
  #   } else {
  #     unit <- "(°C)"
  #   }
  #   
  #   min <- min(minValue(raster1), minValue(raster2))
  #   max <- max(maxValue(raster1), maxValue(raster2))
  #   
  #   pal <- colorNumeric(palette = viridis(5),
  #                       domain = c(min, max),
  #                       na.color = "transparent")
  #   
  #   paldif <- colorNumeric(palette = viridis(5),
  #                          domain = c(minValue(rasterdif()), maxValue(rasterdif())),
  #                          na.color = "transparent")
  #   
  #   if (input$mymap_groups == "Difference") {
  #     leafletProxy('mymap') %>% clearControls() %>%
  #       addLegend(pal = paldif, 
  #                 opacity = 1,
  #                 values = c(minValue(rasterdif()), maxValue(rasterdif())),
  #                 group = "Difference",
  #                 position = "bottomright",
  #                 title = paste("Difference", unit))
  #   } else {
  #     leafletProxy('mymap') %>% clearControls() %>%
  #       addLegend(pal = pal,
  #                 opacity = 1,
  #                 values = c(min, max),
  #                 position = "bottomright",
  #                 title = paste(input$mapVar, unit))
  #   }
  # })

  # observeEvent(input$mymap_groups, {
  #   
  #   raster1 <- rasterData1()
  #   raster2 <- rasterData2()
  #   
  #   if (input$mapVar == "Wind speed") {
  #     unit <- "(m/s)"
  #   } else if (input$mapVar == "Radiation") {
  #     unit <- HTML("(W/m<sup>2</sup>)")
  #   } else {
  #     unit <- "(°C)"
  #   }
  #   
  #   min <- min(minValue(raster1), minValue(raster2))
  #   max <- max(maxValue(raster1), maxValue(raster2))
  #   
  #   pal <- colorNumeric(palette = viridis(5),
  #                       domain = c(min, max),
  #                       na.color = "transparent")
  #   
  #   paldif <- colorNumeric(palette = viridis(5),
  #                          domain = c(minValue(rasterdif()), maxValue(rasterdif())),
  #                          na.color = "transparent")
  #   
  #   if (input$mymap_groups == "Difference") {
  #     leafletProxy('mymap') %>% clearControls() %>%
  #       addLegend(pal = paldif, 
  #                 opacity = 1,
  #                 values = c(minValue(rasterdif()), maxValue(rasterdif())),
  #                 group = "Difference",
  #                 position = "bottomright",
  #                 title = paste("Difference", unit))
  #   } else {
  #     leafletProxy('mymap') %>% clearControls() %>%
  #       addLegend(pal = pal,
  #                 opacity = 1,
  #                 values = c(min, max),
  #                 position = "bottomright",
  #                 title = paste(input$mapVar, unit))
  #   }
  # })
  
  
  # ---------------------- operative temperature -------------------------------
  
  # Dataset methods selector
  output$methodsOutput3 <- renderUI({
    pickerInput("methods3", "Datasets", choices = methods[-8], selected = methods[c(1)], multiple = T,
                options = list(style = "btn-success", `actions-box` = TRUE))
  })
  
  # Rendering selected location/season data
  output$info3 <- renderText({
    if (input$loc3 == "WA") {
      station3 <- "Lind #1 (-118.57°, 47°)"
      loc3 <- "Adams county, WA 1640ft"
    } else if (input$loc3 == "CO") {
      station3 <- "Nunn #1 (-104.73°, 40.87°)"
      loc3 <- "Weld county, CO 5900ft"
    } else if (input$loc3 == "PR") {
      station3 <- "Maricao Forest (-67°, 18.15°)"
      loc3 <- "Mayaguez, Puerto Rico 2450ft"
    }
    
    month3 <- ifelse(input$season3 == 1, "January", "July")
    
    
    HTML("<br><br><b>Station name:</b> ", station3, 
         "<br><b>Location:</b> ", loc3,
         "<br><b>Time:</b> ", month3, "1st - 31st, 2017")
  })
  
  
  # Rendering plot
  output$plot3 <- renderPlotly({
    validate(
      need(input$methods3, "Select datasets")
    )
    
    colors <- c('#b35806', '#542788', '#8073ac', '#e08214', '#b2abd2', '#fdb863', '#fee0b6', '#d8daeb')
    p <- plot_ly() %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = paste("Operative temperature (degK)")))
    
    # For each selected method
    i = 0
    for (method in input$methods3) {
      i = i + 1
      
      # Get variable name/location
      aTemp <- varsDf["Air temperature", method]
      sTemp <- varsDf["Surface temperature", method]
      radiation<- varsDf["Radiation", method]
      
      if (is.na(aTemp)) aTemp = T_a
      else {
        if (input$loc3 != "PR" || !method %in% c("GRIDMET", "microclimUS", "USCRN")) { 
          aTemp <- grabAnyData(method, aTemp, input$loc3, input$season3)
          aTemp$Data = aTemp$Data + 273.15 # C to K
        }
      }
      
      if (is.na(sTemp)) sTemp$Data = array(T_g, dim=c(length(aTemp$Data)))
      else {
        if (input$loc3 != "PR" || !method %in% c("GRIDMET", "microclimUS", "USCRN")) { 
          sTemp <- grabAnyData(method, sTemp, input$loc3, input$season3)
          sTemp$Data = sTemp$Data + 273.15 # C to K
        }
      }
      
      if (is.na(radiation)) radiation$Data = array(Qabs, dim=c(length(aTemp$Data)))
      else {
        if (input$loc3 != "PR" || !method %in% c("GRIDMET", "microclimUS", "USCRN")) { 
          radiation <- grabAnyData(method, radiation, input$loc3, input$season3)
        }
      }
      
      # method data stored in aTemp, sTemp, radiation
      
      op_temp = array(0, dim=c(length(aTemp$Data)))
      for(i in 1:length(aTemp$Data)){
        if(is.na(sTemp$Data[i]) || is.na(aTemp$Data[i]) || is.na(radiation$Data[i])) op_temp[i] = NA
        else op_temp[i] = Tb_Gates(A, D, psa_dir, psa_ref, psa_air, psa_g, sTemp$Data[i], 
                              aTemp$Data[i], radiation$Data[i], epsilon, H_L, ef, K)
      }
      
      p <- p %>% add_lines(x = aTemp$Date, y = op_temp, name = method, line = list(color = colors[i]))
      
      

    } 

    p
    
  })
  
}
