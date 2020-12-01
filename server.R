source("SCAN.R", local = TRUE)
source("ERA5.R", local = TRUE)
source("GLDAS.R", local = TRUE)
source("GRIDMET.R", local = TRUE)
source("NOAA NCDC.R", local = TRUE)
source("microclimUS.R", local = TRUE)
source("microclim.R", local = TRUE)


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
                     "microclim" = c("TA1cm_soil_0", "TA120cm", "D100cm_soil_0", "SOLR", "V1cm", NA, NA))

nameDf <- data.frame(row.names = variables, 
                     "SCAN" = c("Daily average soil temperature 2 in below ground", "Daily Tmax and Tmin", "Daily average soil temperature 1 m below ground", "Daily average solar radiation", "Daily average wind speed", NA),
                     "ERA5" = c("Hourly skin temperature", "Hourly air temperature 2 m above ground", "Hourly soil temperature 28-100 cm", "Hourly surface net solar radiation", "Hourly wind speed 10 m above ground", NA),
                     "GLDAS" = c("3-hourly average surface skin temperature", "3-hourly average air temperature", "3-hourly soil temperature 40-100 cm", "3-hourly net longwave radiation flux", "3-hourly average wind speed", NA),
                     "GRIDMET" = c(NA, "Daily Tmax and Tmin", NA, "Daily mean shortwave radiation at surface", "Daily mean wind speed", NA),
                     "NOAA_NCDC" = c(NA, "Daily Tmax and Tmin", NA, NA, NA, "SNOW"),
                     "microclima" = c(NA, NA, NA, NA, NA, NA),
                     "SNODAS" = c(NA, NA, NA, NA, NA, NA),
                     "microclimUS" = c("Hourly surface temperature (0% shade)", "Hourly air temperature 2 m above ground", "Hourly soil temperature 1 m (0 % shade)", "Hourly solar radiation (horizontal ground)", NA, NA),
                     "microclim" = c("Air temperature 1 cm above ground (soil surface 0 % shade)", "Air temperature 1.2 m above ground", "Soil temperature 1 m", "Solar radiation", "Wind speed 1 cm above ground", NA))


methods <- c("SCAN", "ERA5", "GLDAS", "GRIDMET", "NOAA_NCDC", "microclima", "SNODAS", "microclimUS", "microclim")



shinyServer <- function(input, output, session) {
  
  output$methodsOutput <- renderUI({
    index <- which(is.na(varsDf[input$var, ]))
    pickerInput("methods", "Methods", choices = methods[c(-1, -index)], selected = methods[c(-1, -index)][1],
                options = list(style = "btn-success"))

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
    
    HTML("<b><u>Data showing</u></b>
         <br><b>SCAN:</b> ", nameDf[input$var, "SCAN"],
         "<br><b>", input$methods, ":</b> ", nameDf[input$var, input$methods],
         "<br><br><b>Station name:</b> ", station, 
         "<br><b>Location:</b> ", loc,
         "<br><b>Month:</b> ", month, "1st - 31st")
  })
  
  
  output$plot <- renderPlotly({
    validate(
      need(input$methods, "")
    )
    
    varSCAN <- varsDf[input$var, "SCAN"]
    dfSCAN <- grabSCAN(varSCAN, input$loc, input$season)
    
    inputVar <- varsDf[input$var, input$methods]
    df <- grabAnyData(input$methods, inputVar, input$loc, input$season)
    
    if (input$var == "Wind speed") {
      unit <- "(m/s)"
    } else if (input$var == "Radiation") {
      unit <- HTML("(W/m<sup>2</sup>)")
    } else if (input$var == "Snow") {
      unit <- "(m)"
    } else {
      unit <- "(°C)"
    }
    p <- plot_ly() %>%
      add_lines(x = dfSCAN$Date, y = dfSCAN$Data, name = "SCAN") %>%
      add_lines(x = df$Date, y = df$Data, name = input$methods) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = paste(input$var, unit)))
    
    
    # Adding Tmin when Air temperature is selected
    if (input$var == "Air temperature") {
      varSCAN <- varsDf["Tmin", "SCAN"]
      df <- grabAnyData("SCAN", varSCAN, input$loc, input$season)
      p <- p %>%
        add_lines(x = df$Date, y = df$Data, name = "SCAN Tmin")
      
      inputVar <- varsDf["Tmin", input$methods]
      
      if (!is.na(inputVar)) {
        df <- grabAnyData(input$methods, inputVar, input$loc, input$season)
        p <- p %>%
          add_lines(x = df$Date, y = df$Data, name = paste(input$methods, "Tmin"))
      }
    }

    
    # if (input$methods %in% "ERA5") {
    #   inputVar <- varsDf[varsDf$Variables == input$var, "ERA5"]
    #   df <- grabERA(inputVar, input$loc, input$season)
    #   p <- p %>%
    #     add_lines(x = df$Date, y = df$Data, name = input$methods)
    # } 
    # if (input$methods %in% "GLDAS") {
    #   inputVar <- varsDf[varsDf$Variables == input$var, "GLDAS"]
    #   df <- grabGLDAS(inputVar, input$loc, input$season)
    #   p <- p %>%
    #     add_lines(x = df$Date, y = df$Data, name = input$methods)
    # }  
    # if (input$methods %in% "GRIDMET") {
    #   inputVar <- varsDf[varsDf$Variables == input$var, "GRIDMET"]
    #   df <- grabGRID(inputVar, input$loc, input$season)
    #   p <- p %>%
    #     add_lines(x = df$Date, y = df$Data, name = input$methods)
    # }  
    # if (input$methods %in% "NOAA_NCDC") {
    #   inputVar <- varsDf[varsDf$Variables == input$var, "NOAA_NCDC"]
    #   df <- grabNOAA(inputVar, input$loc, input$season)
    #   p <- p %>%
    #     add_lines(x = df$Date, y = df$Data, name = input$methods)
    # }
    
    p
    
  })
}
