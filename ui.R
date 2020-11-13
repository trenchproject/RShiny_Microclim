library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(plotly)

variables <- c("Surface temp", "Air temp", "Soil temperature", "Snow", "Radiation", "Wind speed")
methods <- c("ERA5", "GRIDMET", "microclima", "NOAA NCDC", "SNODAS", "microclim")

shinyUI <- fluidPage(
  theme = shinytheme("united"),
  setBackgroundColor(color = "#F5F5F5"), 
  
  title = "Microclim",
  titlePanel("Microclim"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("var", "Variable", choices = variables,
                  options = list(style = "btn-success")),
      
      pickerInput("methods", "Methods", choices = methods,
                  options = list(style = "btn-success")),
      
      pickerInput("season", "Season", choices = c("Summer" = 7, "Winter" = 1),
                  options = list(style = "btn-success"))
    ),
    
    mainPanel(
      plotlyOutput("plot")  
    )
  )

)