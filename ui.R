library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(data.table)
library(shinycssloaders)

variables <- c("Air temperature", "Surface temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Snow")

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

)