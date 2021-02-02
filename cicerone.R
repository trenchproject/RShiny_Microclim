guide1 <- Cicerone$
  new()$
  step(
    el = "viz-wrapper",
    title = "Temporal comparison tour",
    description = HTML("Welcome to the guided tour for temporal comparison. We will be going over how to visualize data collected by different datasets over 31 days.
                       <br>Hit next to start the tour.")
  )$
  step(
    el = "plot-wrapper",
    title = "Plot comparing datasets",
    description = HTML("This plot visualizes how different datasets measure the values of certain variables in a month.
                       <br>Right now, the plot shows the air temperature records from SCAN and ERA5 in July 2017 at a location in WA.
                       As you can see, the general trend of the temperature fluctuation matches between the two datasets but SCAN tends to record lower temperatures at night.
                       Let's see how different the data are. Hit next."),
    position = "left"
  )$
  step(
    el = "stats-wrapper",
    title = "Statistics",
    description = HTML("Here, you can select two datasets and see how close the data are. 
                       Click on both <b>SCAN</b> and <b>ERA5</b> to get a checkmark on each.
                       <br>The Pearson correlation coefficient, statistical bias, and RMSE (Root mean squared error) are displayed. 
                       The coefficient of 0.91 shows the strong relatedness. Additionally, the bias of 1.54 is pretty small, and so is the RMSE of 10.39, suggesting that the two datasets generally have similar values with little outliers.
                       <br>Now let's see how to plot something else. Hit next."),
    position = "left"
  )$
  step(
    el = "var-wrapper",
    title = "Variable to plot",
    description = HTML("Here, you can select the variables for which you are looking to get the data. Select <b>Wind speed</b> and hit next."),
    position = "right"
  )$
  step(
    el = "methodsOutput",
    title = "Datasets to show on the plot",
    description = HTML("Here, you will see a list of avialable datasets that collect the variable you selected above.
                       The first two on the list are automatically selected as default. Deselect <b>ERA5</b>, select <b>SCAN</b> and <b>GRIDMET</b> and hit next."),
    position = "right"
  )$
  step(
    el = "sealoc-wrapper",
    title = "Season and location",
    description = HTML("There are options to plot for a summer month (July) or a winter month (January) in 2017, and for three locations in the US.
                       <br>These options can be used to see if the datasets are consistent across different conditions, and add credibility to them.")
  )$
  step(
    el = "info",
    title = "Display of data info",
    description = HTML("Here shows the details of the plotted data. You can see that SCAN has hourly values but GRIDMET records daily. 
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
    description = HTML("Click on both again to see the stats. The bias and RMSE are actually very very small. 
                       When the two datasets have different recording frequency, one with the more frequent dataset is averaged to match the frequency of the other's.
                       <br>In this case, the hourly SCAN data are averaged over each day to match the daily values from GRIDMET, which turned out to have very similar values."),
    position = "left"
  )$
  step(
    el = "viz-wrapper",
    title = "End of tour",
    description = HTML("That's it for the temporal comparison tour. Add other datasets to see which one has the best quality of data.")
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
    description = HTML("This map shows the state of Colorado and the maximum air temperature using the data from ERA5.
                       <br>On the top right corner, you can select to view the data from GLDAS to compare the resolution and the data accuracy.
                       <br>Selecting \"Difference\" will adjust the two maps to the coarser resolution and visualizes the temperature difference between the two datasets."),
    position = "left"
  )$
  step(
    el = "var2-wrapper",
    title = "Variable to plot",
    description = HTML("Here, you can select the variables for which you are looking to get the data."),
    position = "right"
  )$
  step(
    el = "mondate-wrapper",
    title = "Month and date",
    description = HTML("There are options to plot for any day in the first week of January and July in 2017.")
  )$
  step(
    el = "methods-wrapper",
    title = "Two datasets to map",
    description = HTML("Here, you can select two different datasets that collect the variable you selected above.
                       <br>")
  )