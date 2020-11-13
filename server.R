
shinyServer <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    if (input$var == "Surface temp") {
      if (input$methods == "GRIDMET") {
        plot_ly() %>%
          add_lines(x = ~as.Date(tmaxGRID[tmaxGRID$Month == input$season, "Date"]), y = ~tmaxGRID[tmaxGRID$Month == input$season, "WA"]) %>%
          layout(title = "",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Temperature (°C)"))
      }
    } else if (input$var == "Air temp") {
      if (input$methods == "NOAA NCDC") {
        plot_ly() %>%
          add_lines(x = ~as.Date(tmaxNOAA[tmaxNOAA$Month == input$season, "Date"]), y = ~tmaxNOAA[tmaxNOAA$Month == input$season, "WA"]) %>%
          layout(title = "",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Temperature (°C)"))
      } else if (input$methods == "GRIDMET") {
  
      } else if (input$methods == "ERA5") {
        plot_ly() %>%
          add_lines(x = ~tmaxERA[tmaxERA$Month == input$season, "FullDate"], y = tmaxERA[tmaxERA$Month == input$season, "WA"]) %>%
          layout(title = "",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Temperature (°C)"))
      } else if (input$methods == "microclim") {
        plot_ly() %>%
          add_lines(x = ~tempclimUS[tempclimUS$Month == input$season, "FullDate"], y = ~tempclimUS[tempclimUS$Month == input$season, "WA"]) %>%
          layout(title = "",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Temperature (°C)"))
      }
    }
  })
}