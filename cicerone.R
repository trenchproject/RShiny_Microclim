guide1 <- Cicerone$
  new()$
  step(
    el = "viz-wrapper",
    title = "Temporal comparison tour",
    description = HTML("Welcome to the guided tour for temporal comparison. We will be going over how to visualize data collected by different methods over 31 days.
                       <br>Hit next to start the tour.")
  )$
  step(
    el = "plot-wrapper",
    title = "Plot comparing datasets",
    description = HTML("This plot visualizes how different datasets measure the values of certain variables in a month.
                       <br>Right now, the plot shows the air temperature records from SCAN and ERA5 in July 2017 at a location in WA.
                       Note that the ERA5 dataset contains hourly air temperatures while the SCAN dataset only collects daily Tmax and Tmin.                       As you can see, the general trend of the temperature fluctuation matches between the two datasets, yet there are some notable discrepancies in the values.
                       <br>Let's see a plot with different variables. Hit next."),
    position = "left"
  )$
  step(
    el = "var-wrapper",
    title = "Variable to plot",
    description = HTML("Here, you can select the variables for which you are looking to get the data. Select <b>Surface temperature</b> and hit next."),
    position = "right"
  )$
  step(
    el = "methodsOutput",
    title = "Datasets to show on the plot",
    description = HTML("Here, you will see a list of avialable datasets that collect the variable you selected above.
                       The first two on the list are automatically selected as default. Deselect <b>SCAN</b>, select <b>ERA5</b> and <b>GLDAS</b> and hit next."),
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
    description = HTML("Here shows the details of the plotted data. You can see that ERA5 has hourly temperatures but GLDAS has 3-hourly temperatures.
                       <br>Information on data collection site can be viewed here as well.")
  )$
  step(
    el = "plot-wrapper",
    title = "New plot",
    description = HTML("Here's the new plot! It seems that the daily surface temperature fluctuation has a very similar frequency between the two datasets but ERA5 tends to recored a higher maximum surface temperature.")
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
    description = HTML("Welcome to the guided tour for spatial comparison. ")
  )$
  step(
    el = "map-wrapper",
    title = "Map comparing datasets",
    description = HTML("This map shows the state of Colorado and the maximum air temperature using the data from ERA5.
                       <br>On the top right corner, you can select to view the data from GLDAS to compare the resolution and the data accuracy.
                       <br>Selecting \"Difference\" will adjust the two maps to the coarser resolution and visualizes the temperature difference between the two datasets.")
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