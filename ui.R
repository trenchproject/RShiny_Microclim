library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(data.table)

variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Snow")
methods <- c("ERA5", "GLDAS", "GRIDMET", "microclima", "NOAA_NCDC", "SNODAS", "microclim")

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
                  options = list(style = "btn-success")),
      
      pickerInput("loc", "Location", choices = c("Washington" = "WA", "Colorado" = "CO", "Puerto Rico" = "PR"),
                  options = list(style = "btn-success"))
    ),
    
    mainPanel(
      plotlyOutput("plot")  
    )
  )

)