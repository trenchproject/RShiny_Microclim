library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(data.table)
library(shinycssloaders)
library(leaflet)
library(viridis)
library(shinyBS)
library(shinyjs)
library(cicerone)

variables <- c("Air temperature", "Surface temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Precipitation", "Relative humidity", "Soil moisture", "Snow Depth")


variables2 <- c("Maximum air temperature" = "Air temperature", "Minimum air temperature" = "Tmin", "Average surface temperature" = "Surface temperature", "Daily average radiation" = "Radiation") 
                #"Average soil temperature (1 m deep)" = "Soil temperature (1 m deep)", "Daily average radiation" = "Radiation", "Average wind speed" = "Wind speed")
shinyUI <- fluidPage(id = "page",
  use_cicerone(),
  useShinyjs(),
  
  theme = shinytheme("united"),
  setBackgroundColor(color = "#F5F5F5"), 
  
  title = "Microclim",
  titlePanel("Visual comparison of microclimate datasets"),
  hr(),
  
  includeHTML("intro.html"),
  
  hr(),
  
  div(
    id = "viz-wrapper",
    
    tabsetPanel(
      tabPanel("Temporal comparison",
               sidebarLayout(
                 sidebarPanel(
                   h4("Temporal comparison"),
                   p("Select a variable you are interested in and some datasets that contain that variable. 
                     The plot will show how much the data can differ temporally for a given location and a given month, depending on the dataset."),
                   actionBttn(
                     inputId = "reset1",
                     label = "Reset", 
                     style = "material-flat",
                     color = "danger",
                     size = "xs"
                   ),
                   bsTooltip("reset1", "If you have already changed the variables, reset them to default here before starting the tour."),
                   
                   actionBttn(
                     inputId = "tour1",
                     label = "Take a tour!", 
                     style = "material-flat",
                     color = "success",
                     size = "xs"
                   ),
                   hr(),
                   div(
                     id = "var-wrapper",
                     pickerInput("var", "Variable", choices = variables,
                                 options = list(style = "btn-success")),
                   ),
                   
                   uiOutput("methodsOutput"),
                   
                   div(
                     id = "sealoc-wrapper",
                   
                     radioGroupButtons("season", "Season", choices = c("Summer" = 7, "Winter" = 1), selected = 7, status = "danger", size = "sm"),
                   
                     radioGroupButtons("loc", "Location", choices = c("Washington" = "WA", "Colorado" = "CO", "Puerto Rico" = "PR"), selected = "WA", status = "danger", size = "sm"),
                   ),
                   
                   br(),
                   htmlOutput("info"),
                   
                   leafletOutput("minimap")
                 ),
                 
                 mainPanel(
                   div(
                     id = "plot-wrapper",
                   
                     plotlyOutput("plot") %>% withSpinner(type = 7)
                   ),
                   
                   br(),
                   hr(),
                   
                   div(
                     id = "stats-wrapper",
                   
                     h4("Statistics"),
  
                     uiOutput("datasetComparison"),
                     
                     htmlOutput("stats"),
                   ),
                   
                   hr(),
                     
                   includeHTML("stats.html")
                   
                   
                 )
               )
        ),
        tabPanel("Spatial comparison",
                 sidebarLayout(
                   sidebarPanel(
                     h4("Spatial comparison"),
                     p("Select a variable you are interested in and some datasets that contain that variable. 
                       The map of Colorado will show how much the data can differ spatially depending on the dataset for a given time.
                       Loading data can take around 45 seconds."),
                     
                     actionBttn(
                       inputId = "reset2",
                       label = "Reset", 
                       style = "material-flat",
                       color = "danger",
                       size = "xs"
                     ),
                     bsTooltip("reset2", "If you have already changed the variables, reset them to default here before starting the tour."),
                     
                     actionBttn(
                       inputId = "tour2",
                       label = "Take a tour!", 
                       style = "material-flat",
                       color = "success",
                       size = "xs"
                     ),
                     hr(),
                     
                     div(
                       id = "var2-wrapper",
                       pickerInput("mapVar", "Variable", choices = variables2,
                                   options = list(style = "btn-success")),
                     ),
                     fluidRow(id = "mondate-wrapper",
                       column(6, radioGroupButtons("month", "Month", choices = c("January" = 1, "July" = 7), selected = 7, status = "danger", size = "sm")),
                       column(6, radioGroupButtons("date", "Date", choices = 1:7, selected = 1, status = "danger", size = "sm"))
                     ),
                     
                     # radioGroupButtons("hour", "Hour", choices = 0:23, selected = 0, status = "danger", size = "sm"),
                     
                     hr(),
                     
                     div(
                       id = "methods-wrapper",
                       uiOutput("mapMethodsOutput1"),
                       uiOutput("mapMethodsOutput2")
                     )
                   ),
                   
                   mainPanel(
                     div(
                       id = "map-wrapper",
                       leafletOutput("mymap") %>% withSpinner(type = 7)
                     )
                   )
                 )
        )
    )
  )
)