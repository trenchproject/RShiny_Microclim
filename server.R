source("SCAN.R", local = TRUE)
source("ERA5.R", local = TRUE)
source("GLDAS.R", local = TRUE)
source("GRIDMET.R", local = TRUE)
source("NOAA NCDC.R", local = TRUE)
source("microclim paper.R", local = TRUE)


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
  }
  return (data)
}

variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Snow")

varsDf <- data.frame(row.names = c(variables, "Tmin"), 
                     "SCAN" = c(18, 3, 22, 10, 8, NA, 4),
                     "ERA5" = c(4, 3, 5, 6, 1, NA, NA),
                     "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "Lwnet_tavg", "Wind_f_inst", NA, NA),
                     "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", NA, "tmin"),
                     "NOAA_NCDC" = c(NA, "TMAX", NA, NA, NA, "SNOW", "TMIN"),
                     "microclima" = c(NA, NA, NA, NA, NA, NA, NA),
                     "SNODAS" = c(NA, NA, NA, NA, NA, NA, NA),
                     "microclim" = c(NA, NA, NA, NA, NA, NA, NA))


methods <- c("SCAN", "ERA5", "GLDAS", "GRIDMET", "NOAA_NCDC", "microclima", "SNODAS", "microclim")



shinyServer <- function(input, output, session) {
  
  output$methodsOutput <- renderUI({
    index <- which(is.na(varsDf[input$var, ]))
    pickerInput("methods", "Methods", choices = methods[c(-1, -index)], selected = methods[c(-1, -index)][1],
                options = list(style = "btn-success"))

  })
  
  output$info <- renderText({
    HTML("<b>Data showing</b>
         <br>SCAN:", 
         "<br>", input$methods, ": ")
  })
  
  
  output$plot <- renderPlotly({
    validate(
      need(input$methods, "")
    )
    
    varSCAN <- varsDf[input$var, "SCAN"]
    dfSCAN <- grabSCAN(varSCAN, input$loc, input$season)
    
    inputVar <- varsDf[input$var, input$methods]
    df <- grabAnyData(input$methods, inputVar, input$loc, input$season)
    
    p <- plot_ly() %>%
      add_lines(x = dfSCAN$Date, y = dfSCAN$Data, name = "SCAN") %>%
      add_lines(x = df$Date, y = df$Data, name = input$methods) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = input$var))
    
    
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
          add_lines(x = df$Date, y = df$Data, name = paste(input$method, "Tmin"))
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
