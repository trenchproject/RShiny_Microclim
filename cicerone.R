guide1 <- Cicerone$
  new()$
  step(
    el = "viz-wrapper",
    title = "Temporal comparison tour",
    description = HTML("Welcome to the guided tour for temporal comparison. We will be going over how to compare monthly time series between environmental datasets.
                       <br>Hit next to start the tour.")
  )$
  step(
    el = "plot-wrapper",
    title = "Plot comparing datasets",
    description = HTML("The current plot is a monthly time series of USCN observations for July 2017 at a location in Oregon.
                       Select another dataset for a temporal comparison.
                       Let's see how different the data are. Hit next."),
    position = "left"
  )$
  step(
    el = "stats-wrapper",
    title = "Statistics",
    description = HTML("Here, you can select two datasets and see how closely the data agree. 
                       Click on both <b>USCRN</b> and another selected dataset to get a checkmark on each.
                       <br>The Pearson correlation coefficient, statistical bias, and RMSE (Root mean squared error) are displayed. 
                      <br>Now let's see how to plot something else. Hit next."),
    position = "left"
  )$
  step(
    el = "var-wrapper",
    title = "Variable to plot",
    description = HTML("Here, you can select the variables for which you are looking to get the data. Select <b>Radiation</b> and hit next."),
    position = "right"
  )$
  step(
    el = "datasetsOutput",
    title = "Datasets to show on the plot",
    description = HTML("Here, you will see a list of avialable datasets including the variable you selected above.
                       The USCRN data are automatically selected as default. Select <b>ERA5</b> and <b>GRIDMET</b> and hit next."),
    position = "right"
  )$
  step(
    el = "sealoc-wrapper",
    title = "Season and location",
    description = HTML("There are options to plot for a summer month (July) or a winter month (January) in 2017, and for three locations in the US.
                       <br>These options can be used to see if the datasets perform consistently across locatations and seasons.")
  )$
  step(
    el = "info",
    title = "Display of data info",
    description = HTML("Here ypu can see the details of the plotted data. 
                       Each dataset collects data differently, so be sure to check this section when plotting data.
                       <br>Information on data collection site can be viewed here as well.")
  )$
  step(
    el = "minimap",
    title = "Map",
    description = HTML("This map shows the location of the selected station. ")
  )$
  step(
    el = "plot-wrapper",
    title = "New plot",
    description = HTML("Here's the new plot! At quick glance, there seems to be a huge difference between the two datasets but remember that GRIDMET records daily values, which are plotted at midnight for the following day.")
  )$
  step(
    el = "viz-wrapper",
    title = "Revisiting stats",
    description = HTML("Select both datasets to see the stats.  
                       When the two datasets have different recording frequency, the one with the greater frequency is averaged to match the frequency of the other.
                       <br>In this case, the hourly USCRN data are averaged over each day to match the daily values from GRIDMET."),
    position = "left"
  )$
  step(
    el = "viz-wrapper",
    title = "End of tour",
    description = HTML("That's it for the temporal comparison tour.")
  )




guide2 <- Cicerone$
  new()$
  step(
    el = "viz-wrapper",
    title = "Spatial comparison tour",
    description = HTML("Welcome to the guided tour for spatial comparison.")
  )$
  step(
    el = "map-wrapper",
    title = "Map comparing datasets",
    description = HTML("The map currently shows the bias between the air temperature data from all USCRN weather stations and ERA-5. The color of the circle corresponds to the value of the bias. Darker colors indicate more bias.
                       Clicking on the circle will show the name of the location and the precise value.  
                       <br>On the top right corner, you can select to view RMSE (roor mean squared error) or PCC (Pearson correlation coefficient) as well."),
    position = "left"
  )$
  step(
    el = "var2-wrapper",
    title = "Variable to plot",
    description = HTML("Here, you can select a climatic variable."),
    position = "right"
  )$
  step(
    el = "mondate-wrapper",
    title = "Month",
    description = HTML("There are options to plot for January and July in 2017. Either case, the statistics are calculated using the data for the whole month.")
  )$
  step(
    el = "datasets-wrapper",
    title = "A dataset to compare",
    description = HTML("Here, you can select a dataset that collect the variable you selected above to compare to the USCRN weather station data.
                       <br>")
  )