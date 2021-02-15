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
library(DataExplorer)

variables <- c("Air temperature", "Surface temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Precipitation", "Relative humidity", "Soil moisture", "Snow Depth")

variablesTable <- c("Air temperature" = "AirTemp", 
                    "Surface temperature" = "SurfTemp", 
                    "Soil temperature (1 m deep)" = "SoilTemp", 
                    "Radiation", 
                    "Wind speed" = "Wind", 
                    "Precipitation", 
                    "Relative humidity" = "Humidity", 
                    "Soil moisture" = "SoilMoist", 
                    "Snow Depth" = "Snow")

variables2 <- c("Air temperature", "Surface temperature", "Radiation") 
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
      
      tabPanel("Data selection",
        br(),
        fluidRow(
          column(2, awesomeCheckboxGroup("spaCov", "Spatial coverage", choices = c("US", "Global"), selected = "US")),
          column(2, numericInput("tempCov_start", "Beginning of temporal coverage", min = 1979, max = 2021, value = 2017)),
          column(2, numericInput("tempCov_end", "End of temporal coverage", min = 1979, max = 2021, value = 2017)),
          
          # column(6, sliderInput("tempCov", "Temporal coverage", min = 1979, max = 2021, value = c(2017, 2017))),
          column(2, awesomeCheckboxGroup("tempRes", "Temporal resolution", choices = c("Daily", "3-hourly", "Hourly", "Sub-hourly"), selected = "")),
          column(3, pickerInput("varTable", "Variables", choices = variablesTable, multiple = T, selected = NA, options = list(title = "Select variables")))
        ),
        
        p(strong("Suitable datasets")),
        htmlOutput("datasetOutput")
        
      ),
      
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
                                 options = list(style = "btn-success"), multiple = F),
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
                     
                     uiOutput("statsTable")
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
                       column(6, radioGroupButtons("month", "Month", choices = c("January" = 1, "July" = 7), selected = 7, status = "danger", size = "sm"))
                       # column(6, radioGroupButtons("date", "Date", choices = 1:7, selected = 1, status = "danger", size = "sm"))
                     ),
                     
                     # radioGroupButtons("hour", "Hour", choices = 0:23, selected = 0, status = "danger", size = "sm"),
                     
                     hr(),
                     
                     div(
                       id = "methods-wrapper",
                       uiOutput("mapMethodsOutput")
                       # uiOutput("mapMethodsOutput2")
                     )
                   ),
                   
                   mainPanel(
                     div(
                       id = "map-wrapper",
                       leafletOutput("mymap") %>% withSpinner(type = 7)
                     )
                   )
                 )
         ),
      tabPanel("Operative temperature comparison",
               sidebarLayout(
                 sidebarPanel(
                   h4("Operative temperature comparison"),
                   p("These plots predict operative temperature for an ectotherm in the 
                     environmental conditions given by the selected datasets. The Tb_Gates()
                     function in the TrenchR package is used to make these predictions."),
                   
                   uiOutput("methodsOutput3"),
                   
                   div(
                     id = "sealoc-wrapper3",
                     
                     radioGroupButtons("season3", "Season", choices = c("Summer" = 7, "Winter" = 1), selected = 7, status = "danger", size = "sm"),
                     
                     radioGroupButtons("loc3", "Location", choices = c("Washington" = "WA", "Colorado" = "CO", "Puerto Rico" = "PR"), selected = "WA", status = "danger", size = "sm"),
                   ),
                   br(),
                   htmlOutput("info3"),
                   
                   leafletOutput("minimap3")
                   ),
                 mainPanel(
                   div(
                     id = "plot-wrapper3",
                     
                     plotlyOutput("plot3") %>% withSpinner(type = 7)
                   )
                 ))
      )
    )
  )
)