
shinyServer <- function(input, output, session) {
  
  output$plot <- renderPlot({
    if (input$methods == "NOAA NCDC") {
      ggplot() + geom_line(aes(x = as.Date(tmax[tmax$month == input$season, ]$date), y = tmax[tmax$month == input$season, ]$WA))
        
    }
  })
}