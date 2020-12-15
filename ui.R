library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(data.table)
library(shinycssloaders)
library(leaflet)
library(viridis)

variables <- c("Air temperature", "Surface temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Snow")

shinyUI <- fluidPage(
  theme = shinytheme("united"),
  setBackgroundColor(color = "#F5F5F5"), 
  
  title = "Microclim",
  titlePanel("Microclim"),
  hr(),
  
  includeHTML("intro.html"),
  
  hr(),
  h3("Temporal comparison"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("var", "Variable", choices = variables,
                  options = list(style = "btn-success")),
      
      uiOutput("methodsOutput"),
      
      radioGroupButtons("season", "Season", choices = c("Summer" = 7, "Winter" = 1), selected = 7, status = "danger", size = "sm"),
      
      radioGroupButtons("loc", "Location", choices = c("Washington" = "WA", "Colorado" = "CO", "Puerto Rico" = "PR"), selected = "WA", status = "danger", size = "sm"),
      br(),
      htmlOutput("info")
    ),
    
    mainPanel(
      plotlyOutput("plot") %>% withSpinner(type = 7)
    )
  ),
  
  hr(),
  h3("Spatial comparison"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("mapVar", "Variable", choices = variables[-6],
                  options = list(style = "btn-success")),
      
      fluidRow(
        column(6, radioGroupButtons("month", "Month", choices = c("January" = 1, "June" = 7), selected = 7, status = "danger", size = "sm")),
        column(6, radioGroupButtons("date", "Date", choices = 1:7, selected = 1, status = "danger", size = "sm"))
      ),
      
      radioGroupButtons("hour", "Hour", choices = 0:23, selected = 0, status = "danger", size = "sm"),
      
      hr(),
      
      uiOutput("mapMethodsOutput1"),
      uiOutput("mapMethodsOutput2"),
    ),
    
    mainPanel(
      h4("Method 1"),
      leafletOutput("map1") %>% withSpinner(type = 7),
      br(),
      h4("Method 2"),
      leafletOutput("map2") %>% withSpinner(type = 7)
     
    )
  )

)