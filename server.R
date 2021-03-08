# Sourcing R files
source("R/SCAN.R", local = TRUE)
source("R/ERA5.R", local = TRUE)
source("R/GLDAS.R", local = TRUE)
source("R/GRIDMET.R", local = TRUE)
source("R/NOAA NCDC.R", local = TRUE)
source("R/microclimUS.R", local = TRUE)
source("R/microclim.R", local = TRUE)
source("R/SNODAS.R", local = TRUE)
source("R/USCRN.R", local = TRUE)
source("R/NicheMapR.R", local = TRUE)
source("cicerone.R", local= TRUE)
source("functions.R", local = TRUE)


variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Precipitation", "Relative humidity", "Soil moisture", "Snow Depth")

varsDf <- data.frame(row.names = c(variables, "Tmin"),
                     "SCAN" = c(18, 3, 22, 7, 10, 5, 6, 17, NA, 4),
                     "ERA5" = c(4, 3, 6, 7, 1, 8, NA, NA, 5, 9),
                     "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "SWdown_f_tavg", "Wind_f_inst", "Rainf_f_tavg", "Qair_f_inst", "SoilMoi40_100cm_inst", "SnowDepth_inst", "Tmin"),
                     "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", "prcp", NA, NA, NA, "tmin"),
                     "NOAA_NCDC" = c(NA, "TMAX", NA, NA, NA, "PRCP", NA, NA, "SNWD", "TMIN"),
                     "microclimUS" = c("soil0cm_0pctShade", "TA200cm", "soil100cm_0pctShade", "SOLR", NA, NA, "RH200cm", "moist100cm_0pctShade", NA, "Tmin"),
                     "microclim" = c("D0cm_soil_0", "TA120cm", "D100cm_soil_0", "SOLR", "V1cm", NA, "RH120cm", NA, NA, "Tmin"),
                     #"USCRN" = c("SUR_TEMP", "T_MAX", NA, "SOLARAD", NA, NA, NA, NA, NA, NA),
                     "USCRN" = c("SURFACE_TEMPERATURE", "AIR_TEMPERATURE", NA, "SOLAR_RADIATION", "WIND_1_5", "PRECIPITATION", "RELATIVE_HUMIDITY", NA, NA, NA),
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
                     "USCRN" = c("Sub-hourly infrared surface temperature", "Sub-hourly air temperature", NA, "Average global solar radiation received", "Wind speed 1.5 m above ground", "Sub-hourly precipitation", "Sub-hourly relative humidity", NA, NA),
                     "SNODAS" = c(NA, NA, NA, NA, NA, NA, NA, NA, "Snow depth"),
                     "NicheMapR" = c("Hourly soil temperature at 0cm", "Hourly air temperature 2 m above ground", "Hourly soil temperature 100 cm below ground", "Hourly solar radiation, unshaded", "Hourly wind speed 2 m above ground", NA, "Hourly relative humidity 2 m above ground", NA, "Hourly predicted snow depth"))

datasets <- colnames(varsDf)


shinyServer <- function(input, output, session) {
  
  #____________________________________________________________________________
  # Guided tour
  observeEvent(input$tour1, guide1$init()$start())
  
  observeEvent(input$reset1, {
    reset("page")
  })
  
  observeEvent(input$tour2, guide2$init()$start())
  
  observeEvent(input$reset2, {
    reset("page")
  })
  
  
  #____________________________________________________________________________
  # Data selection
  
  output$mytable <- DT::renderDataTable({
    
    validate(
      need(input$tempRes, "Select temporal resolution")
    )
    
    dataTable <- readxl::read_xlsx("DatasetTable.xlsx") %>% as.data.frame() %>%
      filter(TempCovStart <= input$tempCov_start | is.na(TempCovStart)) %>%
      filter(TempCovEnd >= input$tempCov_end | is.na(TempCovEnd)) 
    
    if (length(input$tempRes) == 1 && input$tempRes == "3-hourly") { # USCRN has TempRes of "Sub-hourly, Hourly, Daily"
      dataTable <- filter(dataTable, TempRes %in% "3-hourly")
    } else {
      dataTable <- filter(dataTable, TempRes %in% c(input$tempRes, "Sub-hourly, Hourly, Daily"))
    }
    
    if (input$spaCov == "Outside of US") { # When "US" is selected, datasets of global spatial coverage are listed as well
      dataTable <- filter(dataTable, SpatCov != "US")
    }
    
    for (var in input$varTable) {
      dataTable <- dataTable[dataTable[, var] == "T", ]
    }
    
    if (nrow(dataTable) > 0) {
      dataTable$TempCovRange <- paste0(dataTable$TempCovStart, "-", dataTable$TempCovEnd)
    } else {
      dataTable <- cbind(dataTable, data.frame("TempCovRange" = character(0)))
    }
    dataTable[dataTable == "NA-NA"] <- "Varies"
    dataTable[dataTable == "T"] <- as.character(icon("ok", lib = "glyphicon"))
    dataTable[dataTable == "F"] <- as.character(icon("remove", lib = "glyphicon"))
    
    dataTable <- dataTable[, c("Dataset", "TempCovRange", "TempRes", "SpatCov", "SpatRes", colnames(dataTable)[7:15])] %>%
      set_colnames(c("Dataset", "Temporal coverage", "Temporal resolution", "Spatial coverage", "Spatial resolution", "Air temp", "Surface temp", "Soil temp", "Radiation", "Wind speed", "Precipitation", "Humidity", "Soil moist", "Snow depth"))
    
    datatable(dataTable, escape = F)

  })


  
  #___________________________________________________________________________________
  # Temporal comparison
  
  output$datasetsOutput <- renderUI({
    
    index <- which(!is.na(varsDf[input$var, ]))
    
    pickerInput("datasets", "Datasets", 
                choices = datasets[index][datasets[index] != "USCRN"],  # USCRN shouldn't be an option
                selected = datasets[index][datasets[index] != "USCRN"][c(1, 2)], 
                multiple = T,
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
    for (method in input$datasets) {
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
      need(input$datasets, "Select datasets")
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
    p <- plot_ly()

    i = 0
    for (method in input$datasets) {
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
      for (method in input$datasets) {
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
    
    p %>% layout(xaxis = list(title = "Date"),
                 yaxis = list(title = paste(input$var, unit)))
    
  })
  
  # Stats for temporal comparison
  output$datasetComparison <- renderUI({
    validate(
      need(input$datasets, "")
    )

    checkboxGroupButtons("statsOption", "Select two datasets to see their relatedness", choices = input$datasets, status = "success", 
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
  
  
  # output$statsTable <- renderUI({
  #   validate(
  #     need(length(input$statsOption) == 2, "Select two datasets\n\n\n")
  #   )
  #   # Have to figure out what to do with 3-hourly and daily values. take the average?
  #   
  #   # hourly: SCAN, ERA5, microclimUS, NicheMapR
  #   # 3-hourly: GLDAS
  #   # daily: gridMET, NOAA NCDC, SNODAS
  #   # sub-hourly: USCRN
  #   
  #   df1 <- grabAnyData(input$statsOption[1], varsDf[input$var, input$statsOption[1]], input$loc, input$season)
  #   df2 <- grabAnyData(input$statsOption[2], varsDf[input$var, input$statsOption[2]], input$loc, input$season)
  #   
  #   if (input$statsOption[1] %in% c("GRIDMET", "NOAA_NCDC", "SNODAS") || input$statsOption[2] %in% c("GRIDMET", "NOAA_NCDC", "SNODAS")) {
  #     df1$Date <- as.Date(df1$Date)
  #     df1 <- aggregate(df1$Data, by = list(df1$Date), mean) %>% set_colnames(c("Date", "Data"))
  #     df2$Date <- as.Date(df2$Date) 
  #     df2 <- aggregate(df2$Data, by = list(df2$Date), mean) %>% set_colnames(c("Date", "Data"))
  #   }
  #   
  #   # Will have to work on
  #   # } else if (input$statsOption[1] == "GLDAS" || input$statsOption[2] == "GLDAS") {
  #   #   
  #   # }
  #   
  #   colnames(df1)[colnames(df1) == "Data"] <- "Data1"
  #   colnames(df2)[colnames(df2) == "Data"] <- "Data2"
  #   
  #   setDT(df1)
  #   setDT(df2)
  #   
  #   merge <- df1[df2, on = "Date"] %>% 
  #     na.omit() %>% 
  #     as.data.frame()
  #   
  #   data1 <- merge$Data1
  #   data2 <- merge$Data2
  #   
  #   plot_correlation()
  # })
  
  
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
  
  output$mapDatasetsOutput <- renderUI({
    mapDatasets <- c("ERA5", "GLDAS", "GRIDMET", "NOAA_NCDC", "NicheMapR", "microclim", "microclimUS")
    
    index <- which(!is.na(varsDf[input$mapVar, ]))
    choices <- mapDatasets[mapDatasets %in% datasets[index]]
    
    pickerInput("mapDatasets", "Dataset to compare", choices = choices, selected = "ERA5", # all the dataset except for SCAN
                options = list(style = "btn-success", `actions-box` = TRUE))
  })

  # Stats for spatial comparison
  statsTable <- reactive({
    
    validate(
      need(input$mapDatasets, "")
    )
    
    stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
    
    CRN <- grabMapData("USCRN", varsDf[input$mapVar, "USCRN"], input$month)
    
    if (input$mapDatasets %in% c("GRIDMET", "NOAA_NCDC")) {
      CRN$Date <- as.Date(CRN$Date)
      CRN <- aggregate(list(CRN[, c(-1, -2)]), by = list(CRN$Date), mean) %>%
        set_colnames(c("Date", stations$Name))
    }
    
    inputVar <- varsDf[input$mapVar, input$mapDatasets]
    
    mapDf <- grabMapData(input$mapDatasets, inputVar, input$month)

    stats <- cbind(stations, 
                   "Bias" = NA,
                   "RMSE" = NA,
                   "PCC" = NA)
    
    for (station in stations$Name) {
      merged <- merge(CRN[, c("Date", station)], mapDf[, c("Date", station)], by = "Date", all = T) %>%
        set_colnames(c("Date", "Data1", "Data2")) %>%
        na.omit()
      if (nrow(merged) > 7) {
        bias <- abs((sum(merged$Data1) - sum(merged$Data2)) / nrow(merged))
        stats[stats$Name == station, "Bias"] <- bias
        
        RMSE <- sum((merged$Data1 - merged$Data2)^2) / nrow(merged) # Root mean square error
        stats[stats$Name == station, "RMSE"] <- RMSE
        
        PCC <- cor.test(x = merged$Data1, y = merged$Data2, method = "pearson") # Pearson correlation coefficient
        stats[stats$Name == station, "PCC"] <- unname(PCC$estimate)
      } else {
        stats[stats$Name == station, c("Bias", "RMSE", "PCC")] <- NA
      }
    }

    stats <- stats %>% na.omit()
    stats
  })
  
  
  output$mymap <- renderLeaflet({
    
    validate(
      need(statsTable(), "")
    )
  
    stats <- statsTable()
    
    maxRawBias <- max(stats$Bias)
    
    maxRawRMSE <- max(stats$RMSE)
    
    maxRawPCC <- max(stats$PCC)

    roundUp <- function (percentile, category = "B") {
      if (category == "B") {
        return (ceiling(maxRawBias * percentile * 10) / 10)
      } else if (category == "R") {
        return (ceiling(maxRawRMSE * percentile * 10) / 10)
      } else if (category == "P") {
        return (ceiling(maxRawPCC * percentile * 10) / 10)
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
    
    
    stats$PCCCat <- cut(stats$PCC,
                         c(0, roundUp(0.25, "P"), roundUp(0.5, "P"), roundUp(0.75, "P"), roundUp(1, "P")), include.lowest = T,
                         labels = c(paste0("0 - ", roundUp(0.25, "P")), paste0(roundUp(0.25, "P"), " - ", roundUp(0.5, "P")), paste0(roundUp(0.5, "P"), " - ", roundUp(0.75, "P")), paste0(roundUp(0.75, "P"), " - ", roundUp(1, "P"))))
    
    pccCol <- colorFactor(palette = c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c'), stats$PCCCat)
    
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(data = stats, lng = ~Lon, lat = ~Lat,
                       color = ~biasCol(stats$BiasCat),
                       stroke = TRUE,
                       radius = ~Bias / maxRawBias * 15, 
                       opacity = 1, 
                       fillOpacity = 0, 
                       group = "Bias",
                       popup = paste0(str_replace_all(string = stats$Name, pattern =  "_", replacement = " "), ": ", round(stats$Bias, digits = 2))) %>%
      addCircleMarkers(data = stats, lng = ~Lon, lat = ~Lat,
                       color = ~rmseCol(stats$RMSECat),
                       stroke = TRUE,
                       radius = ~RMSE / maxRawRMSE * 15, 
                       opacity = 1, 
                       fillOpacity = 0,                        
                       group = "RMSE",
                       popup = paste0(str_replace_all(string = stats$Name, pattern =  "_", replacement = " "), ": ", round(stats$RMSE, digits = 2))) %>%
      addCircleMarkers(data = stats, lng = ~Lon, lat = ~Lat,
                       color = ~pccCol(stats$PCCCat),
                       stroke = TRUE,
                       radius = ~PCC / maxRawPCC * 7, 
                       opacity = 1, 
                       fillOpacity = 0,                        
                       group = "PCC",
                       popup = paste0(str_replace_all(string = stats$Name, pattern =  "_", replacement = " "), ": ", round(stats$PCC, digits = 2))) %>%
      addLayersControl(baseGroups = c("Bias", "RMSE", "PCC")) %>%
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
    
    maxRawPCC <- max(stats$PCC)
    
    roundUp <- function (percentile, category = "B") {
      if (category == "B") {
        return (ceiling(maxRawBias * percentile * 10) / 10)
      } else if (category == "R") {
        return (ceiling(maxRawRMSE * percentile * 10) / 10)
      } else if (category == "P") {
        return (ceiling(maxRawPCC * percentile * 10) / 10)
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
    
    
    stats$PCCCat <- cut(stats$PCC,
                        c(0, roundUp(0.25, "P"), roundUp(0.5, "P"), roundUp(0.75, "P"), roundUp(1, "P")), include.lowest = T,
                        labels = c(paste0("0 - ", roundUp(0.25, "P")), paste0(roundUp(0.25, "P"), " - ", roundUp(0.5, "P")), paste0(roundUp(0.5, "P"), " - ", roundUp(0.75, "P")), paste0(roundUp(0.75, "P"), " - ", roundUp(1, "P"))))
    
    pccCol <- colorFactor(palette = c('#ffffb2','#fecc5c','#fd8d3c','#e31a1c'), stats$PCCCat)
    
    if (input$mymap_groups == "Bias") {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = biasCol, 
                  opacity = 1,
                  values = stats$BiasCat,
                  group = "Bias legend",
                  position = "bottomright",
                  title = "Bias") %>%
        addProviderTiles("Esri.WorldImagery")
    } else if (input$mymap_groups == "RMSE") {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = rmseCol,
                  opacity = 1,
                  values = stats$RMSECat,
                  group = "RMSE legend",
                  position = "bottomright",
                  title = "Root mean squared error") %>%
        addProviderTiles("Esri.WorldImagery")
    } else { # PCC
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = pccCol,
                  opacity = 1,
                  values = stats$PCCCat,
                  group = "PCC legend",
                  position = "bottomright",
                  title = "Pearson Correlation Coefficient") %>%
        addProviderTiles("Esri.WorldImagery")
    }
  })

  
  # ---------------------- operative temperature -------------------------------
  # ---------------------- operative temperature -------------------------------
  # ---------------------- operative temperature -------------------------------
  # ---------------------- operative temperature -------------------------------
  
  
  # Data set selector
  output$datasetsOutput3 <- renderUI({
    pickerInput("datasets3", "Datasets", choices = datasets[-1][-8], selected = datasets[c(8)], multiple = T,
                options = list(style = "btn-success", `actions-box` = TRUE))
  })
  
  # Operative temperature function selector
  output$op3 <- renderUI({
    pickerInput("op3", "Operative Temperature Function", 
                choices = c("Small Ectotherm (Gates)" = "gates", "Sceloporus Lizard" = "lizard", "Small Ectotherm (Campbell-Norman)" = "campbell"), 
                selected = "lizard", multiple = F,
                options = list(style = "btn-success", `actions-box` = TRUE))
  })
  
  
  # Rendering selected location/season data
  output$info3 <- renderText({
    if (input$loc3 == "OR") {
      station3 <- "OR John Day 35 WNW (-119.65°, 44.55°)"
      loc3 <- "John Day, OR, 2267ft"
    } else if (input$loc3 == "CO") {
      station3 <- "CO Nunn 7 NNE (-104.73°, 40.87°)"
      loc3 <- "Weld county, CO 5900ft"
    } else if (input$loc3 == "HI") {
      station3 <- "HI Hilo 5 S (-155.07°, 19.7°)"
      loc3 <- "Hilo, Hawaii 62ft"
    }
    
    month3 <- ifelse(input$season3 == 1, "January", "July")
    
    text3 <- ""
    for (method in input$datasets3) {
      text3 <- paste0(text3, "<br><b>", method, ":</b> ")
      var <- "Air temperature"
      text3 <- paste0(text3, nameDf[var, method], ", ")
      var <- "Surface temperature"
      text3 <- paste0(text3, nameDf[var, method], ", ")
      var <- "Radiation"
      text3 <- paste0(text3, nameDf[var, method], ", ")
      var <- "Wind speed"
      text3 <- paste0(text3, nameDf[var, method])
    }
    
    HTML("<b><u>Input data for operative temperature estimation</u></b>",
         text3,
         "<br><br><b>Station name:</b> ", station3, 
         "<br><b>Location:</b> ", loc3,
         "<br><b>Time:</b> ", month3, "1st - 31st, 2017")
  })
  
  # Small location map in sidebar
  output$minimap3 <- renderLeaflet({
    x = c(-119.65, -104.7552, -155.07)
    y = c(44.55, 40.8066, 19.7)
    text = c("John Day, OR", "Nunn, CO", "Hilo, HI")
    names(x) = names(y) = names(text) = c("OR", "CO", "HI")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(lng = x[input$loc3], lat = y[input$loc3], popup = HTML(text[input$loc3])) %>%
      setView(lng = -97.5, lat = 39, zoom = 2.5)
  })
  
  
  # Rendering plot
  output$plot3 <- renderPlotly({
    
    validate(need(input$datasets3, "Select datasets"))
    
    colors_special <- list('#b35806', '#542788', '#8073ac', '#e08214', '#b2abd2', '#fdb863', '#fee0b6', '#d8daeb')
    fig <- plot_ly() %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = paste("Operative temperature (degC)"))
             )
    
    # For each selected method
    for (method in input$datasets3) {
      
      # Get variable name/location
      aTemp <- varsDf["Air temperature", method]
      sTemp <- varsDf["Surface temperature", method]
      radiation<- varsDf["Radiation", method]
      wind <- varsDf["Wind speed", method]
      
      # If it is not a continental-US-only dataset
      if (input$loc3 != c("HI") || !method %in% c("GRIDMET", "microclimUS")) { 
        
        # Get air temperature data
        if (is.na(aTemp)) aTemp = sTemp$Data = array(T_a, dim=c(length(aTemp$Data)))
        else {
          aTemp <- grabAnyData(method, aTemp, input$loc3, input$season3)
          aTemp$Data = aTemp$Data + 273.15 # C to K
        }
        
        # Get surface temperature data
        if (is.na(sTemp)) sTemp$Data = array(T_g, dim=c(length(aTemp$Data)))
        else {
          sTemp <- grabAnyData(method, sTemp, input$loc3, input$season3)
          sTemp$Data = sTemp$Data + 273.15 # C to K
        }
        
        # Get radiation data
        if (is.na(radiation)) radiation$Data = array(Qabs, dim=c(length(aTemp$Data)))
        else radiation <- grabAnyData(method, radiation, input$loc3, input$season3)
        
        # Get wind speed data
        if (is.na(wind)) wind$Data = array(0, dim=c(length(aTemp$Data)))
        else wind <- grabAnyData(method, wind, input$loc3, input$season3)
      
        # method data stored in aTemp, sTemp, radiation, wind
        
        op_temp = array(0, dim=c(length(aTemp$Data)))
        
        if (input$op3=="gates") {
          op_temp = mapply(Tb_Gates, A, D, psa_dir, psa_ref, psa_air, psa_g, sTemp$Data, 
                           aTemp$Data, radiation$Data, epsilon, H_L, ef, K)
          fig <- fig %>% layout(title="Small Ectoterm Operative Temperature (Gates Model)")
        } else if (input$op3=="lizard") {
          doytemp = sapply(aTemp$Date,day_of_year)
          op_temp = mapply(Tb_lizard, aTemp$Data, sTemp$Data, wind$Data, svl=60, m=10, psi=34, rho_S=0.7, elev=500,
                                  doy=doytemp, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, 
                                  epsilon, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)
          fig <- fig %>% layout(title="Sceloporus Lizard Operative Temperature")
        } else if (input$op3=="campbell") {
          op_temp = mapply(Tb_CampbellNorman, aTemp$Data, sTemp$Data, radiation$Data, 
                                          alpha_L=0.965, epsilon, c_p=29.3, D, wind$Data)
          fig <- fig %>% layout(title="Small Ectoterm Operative Temperature (Campbell-Norman Model)")
        }
        
        op_temp[op_temp < 0] = NA
        op_temp = op_temp - 273.15
        
        
        
        fig <- fig %>% add_lines(x = as.POSIXct(aTemp$Date), y = op_temp, name = method)
      }
    } 
    
    fig
    
  })

  
}
