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
  
  tabsetPanel(
    tabPanel("Temporal comparison",
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
             )
      ),
      tabPanel("Spatial comparison",
               sidebarLayout(
                 sidebarPanel(
                   pickerInput("mapVar", "Variable", choices = c("Maximum air temperature" = "Air temperature", "Minimum air temperature" = "Tmin", variables[c(-1, -6)]),
                               options = list(style = "btn-success")),
                   
                   fluidRow(
                     column(6, radioGroupButtons("month", "Month", choices = c("January" = 1, "July" = 7), selected = 7, status = "danger", size = "sm")),
                     column(6, radioGroupButtons("date", "Date", choices = 1:7, selected = 1, status = "danger", size = "sm"))
                   ),
                   
                   # radioGroupButtons("hour", "Hour", choices = 0:23, selected = 0, status = "danger", size = "sm"),
                   
                   hr(),
                   
                   uiOutput("mapMethodsOutput1"),
                   uiOutput("mapMethodsOutput2")
                 ),
                 
                 mainPanel(
                   leafletOutput("mymap") %>% withSpinner(type = 7)
                   
                 )
               )
      )
  )
)