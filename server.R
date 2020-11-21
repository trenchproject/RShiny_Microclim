source("SCAN.R", local = TRUE)
source("ERA-5.R", local = TRUE)
source("GLDAS.R", local = TRUE)
source("GRIDMET.R", local = TRUE)
source("NOAA NCDC.R", local = TRUE)
source("microclim paper.R", local = TRUE)

variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Snow")

varsDf <- data.frame("Variables" = variables, 
                     "SCAN" = c(18, 3, 22, 10, 8, NA),
                     "ERA5" = c(4, 3, 5, 1, 6, NA),
                     "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "Lwnet_tavg", "Wind_f_inst", NA),
                     "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", NA),
                     "NOAA_NCDC" = c(NA, "TMAX", NA, NA, NA, "SNOW"))

shinyServer <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    inputVar <- varsDf[varsDf$Variables == input$var, input$methods]
    if (input$methods == "ERA5") {
      df <- ERAdf(inputVar)
    } else if (input$methods == "GLDAS") {
      df <- fullGLDAS(inputVar)
    } else if (input$methods == "GRIDMET") {
      df <- fullGRID(inputVar)
    } else if (input$methods == "NOAA_NCDC") {
      df <- fullNOAA(inputVar)
    }

    filtered <- df %>% filter(Month == input$season)

    varSCAN <- varsDf[varsDf$Variables == input$var, "SCAN"]
    dfSCAN <- fullSCAN(varSCAN) %>% filter(Month == input$season)

    plot_ly() %>%
      add_lines(x = ~dfSCAN[, "Date"], y = ~dfSCAN[, input$loc], name = "SCAN") %>%
      add_lines(x = ~filtered$FullDate, y = ~filtered[, input$loc], name = input$methods) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = input$var))
      
    
    # if (input$var == "Surface temp") {
    #   if (input$methods == "GRIDMET") {
    #     plot_ly() %>%
    #       add_lines(x = ~as.Date(tmaxGRID[tmaxGRID$Month == input$season, "Date"]), y = ~tmaxGRID[tmaxGRID$Month == input$season, "WA"]) %>%
    #       layout(title = "",
    #              xaxis = list(title = "Date"),
    #              yaxis = list(title = "Temperature (°C)"))
    #   }
    # } else if (input$var == "Air temp") {
    #   if (input$methods == "NOAA NCDC") {
    #     plot_ly() %>%
    #       add_lines(x = ~as.Date(tmaxNOAA[tmaxNOAA$Month == input$season, "Date"]), y = ~tmaxNOAA[tmaxNOAA$Month == input$season, "WA"]) %>%
    #       layout(title = "",
    #              xaxis = list(title = "Date"),
    #              yaxis = list(title = "Temperature (°C)"))
    #   } else if (input$methods == "GRIDMET") {
    # 
    #   } else if (input$methods == "ERA5") {
    #     plot_ly() %>%
    #       add_lines(x = ~tmaxERA[tmaxERA$Month == input$season, "FullDate"], y = tmaxERA[tmaxERA$Month == input$season, "WA"]) %>%
    #       layout(title = "",
    #              xaxis = list(title = "Date"),
    #              yaxis = list(title = "Temperature (°C)"))
    #   } else if (input$methods == "microclim") {
    #     plot_ly() %>%
    #       add_lines(x = ~tempclimUS[tempclimUS$Month == input$season, "FullDate"], y = ~tempclimUS[tempclimUS$Month == input$season, "WA"]) %>%
    #       layout(title = "",
    #              xaxis = list(title = "Date"),
    #              yaxis = list(title = "Temperature (°C)"))
    #   }
    # }
  })
}