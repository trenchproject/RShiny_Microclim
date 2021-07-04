# Creating figures for manuscript
source("server.R", local = TRUE)
source("cicerone.R", local= TRUE)
source("functions.R", local = TRUE)
options(shiny.sanitize.errors = FALSE)

library(viridis)
library(plotly)
library(formattable)
library(knitr)
library(TrenchR)


variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Precipitation", "Relative humidity", "Soil moisture", "Snow Depth")

varsDf <- data.frame(row.names = c(variables, "Tmin"),
 "ERA5" = c(4, 3, 6, 7, 1, 8, NA, NA, 5, 9),
 "ERA51cm" = c(4, 3, 6, 7, 1, 8, NA, NA, 5, 9),
 "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "SWdown_f_tavg", "Wind_f_inst", "Rainf_f_tavg", "Qair_f_inst", "SoilMoi40_100cm_inst", "SnowDepth_inst", "Tmin"),
 "GLDAS1cm" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "SWdown_f_tavg", "Wind_f_inst", "Rainf_f_tavg", "Qair_f_inst", "SoilMoi40_100cm_inst", "SnowDepth_inst", "Tmin"),
 "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", "prcp", NA, NA, NA, "tmin"),
 "microclimUS" = c("soil0cm_0pctShade", "TA1cm_0pctShade", "soil100cm_0pctShade", "SOLR", "V1cm", NA, "RH1cm_0pctShade", "moist100cm_0pctShade", "SNOWDEP_0pctShade", "Tmin"),
 "microclim" = c("D0cm_soil_0", "TA1cm_soil_0", "D100cm_soil_0", "SOLR", "V1cm", NA, "RH1cm_soil_0", NA, NA, "Tmin"),
 "USCRN" = c("SUR_TEMP", "T_MAX", "SOIL_TEMP_100", "SOLARAD", NA, NA, "RH_HR_AVG", "SOIL_MOISTURE_100", NA, NA),
 "USCRN1cm" = c("SUR_TEMP", "T_MAX", "SOIL_TEMP_100", "SOLARAD", NA, NA, "RH_HR_AVG", "SOIL_MOISTURE_100", NA, NA),
 "NCEP" = c("skt","air","tmp","csdsf","uwnd","prate",NA,"soilw",NA,NA),
 "NCEP1cm" = c("skt","air","tmp","csdsf","uwnd","prate",NA,"soilw",NA,NA),
 "micro_ncep" = c("D0cm", "TALOC", "D100cm", "SOLR", "VLOC", NA, "RHLOC", NA, "SNOWDEP", NA),
 "micro_usa" = c("D0cm", "TALOC", "D100cm", "SOLR", "VLOC", NA, "RHLOC", NA, "SNOWDEP", NA),
 "micro_global" = c("D0cm", "TALOC", "D100cm", "SOLR", "VLOC", NA, "RHLOC", NA, "SNOWDEP", NA),
 "micro_era5" = c("D0cm", "TALOC", "D100cm", "SOLR", "VLOC", NA, "RHLOC", NA, "SNOWDEP", NA),
 "NEW01" = c(NA, "TMAXX", NA, NA, "WNMAXX", "RAINFALL", "RHMAXX", NA, NA, "TMINN"))

colorsDf <- data.frame(row.names = c("color"),
 "ERA5" = viridis_pal(option = "D")(5)[[2]],
 "ERA51cm" = viridis_pal(option = "D")(5)[[2]],
 "GRIDMET" = viridis_pal(option = "D")(5)[[3]],
 "NCEP" = viridis_pal(option = "D")(5)[[4]],
 "GLDAS" = viridis_pal(option = "D")(5)[[5]],
 "GLDAS1cm" = viridis_pal(option = "D")(5)[[5]],
 "USCRN" = viridis_pal(option = "D")(5)[[1]],
 "NEW01" = "#000000",
 "micro_usa" = viridis_pal(option = "D")(5)[[3]],
 "micro_ncep" = viridis_pal(option = "D")(5)[[4]],
 "USCRN1cm" = viridis_pal(option = "D")(5)[[1]],
 "micro_global" = "#000000",
 "microclim" = "#000000",
 "NCEP1cm" = viridis_pal(option = "D")(5)[[4]],
 "microclimUS" = viridis_pal(option = "D")(5)[[3]]
 )

nameDf <- data.frame(row.names = variables,
 "ERA5" = c("Hourly skin temperature", "Hourly air temperature 2 m aboveground", "Hourly soil temperature 28-100 cm below ground", "Hourly surface net solar radiation", "Hourly wind speed 10 m above ground", "Total precipitation", NA, NA, "Hourly snow depth"),
 "ERA51cm" = c("Hourly skin temperature", "Hourly air temperature 2 m aboveground", "Hourly soil temperature 28-100 cm below ground", "Hourly surface net solar radiation", "Hourly wind speed 10 m above ground", "Total precipitation", NA, NA, "Hourly snow depth"),
 "GLDAS" = c("3-hourly average surface skin temperature", "3-hourly average air temperature", "3-hourly average soil temperature 40-100 cm below ground", "3-hourly net longwave radiation flux", "3-hourly average wind speed", "Total precipitation", "3-hourly relative humidity", "3-hourly average soil moisture 40-100 cm below ground", "3-hourly snow depth"),
 "GLDAS1cm" = c("3-hourly average surface skin temperature", "3-hourly average air temperature", "3-hourly average soil temperature 40-100 cm below ground", "3-hourly net longwave radiation flux", "3-hourly average wind speed", "Total precipitation", "3-hourly relative humidity", "3-hourly average soil moisture 40-100 cm below ground", "3-hourly snow depth"),
 "GRIDMET" = c(NA, "Daily Tmax and Tmin", NA, "Daily mean shortwave radiation at surface", "Daily mean wind speed", "Daily precipitation amount", NA, NA, NA),
 "microclimUS" = c("Hourly surface temperature (0% shade)", "Hourly air temperature 1cm above ground", "Hourly soil temperature 1m belowground (0 % shade)", "Hourly solar radiation (horizontal ground)", "Wind speed 1cm aboveground", NA, "Relative humidity 1cm aboveground", "Hourly soil moisture 1m belowground (0 % shade)", NA),
 "microclim" = c("Surface temperature (0% shade)", "Air temperature 1cm aboveground", "Soil temperature 1m belowground", "Solar radiation", "Wind speed 1cm aboveground", NA, "Relative humidity 1cm aboveground", NA, NA),
 "USCRN" = c("Hourly infrared surface temperature", "Hourly air temperature", "Hourly soil temperature 1m belowground", "Average global solar radiation received", NA, NA, "Hourly relative humidity", "Hourly soil moisture 1m belowground", NA),
 "USCRN1cm" = c("Hourly infrared surface temperature", "Hourly air temperature", "Hourly soil temperature 1m belowground", "Average global solar radiation received", NA, NA, "Hourly relative humidity", "Hourly soil moisture 1m belowground", NA),
 "NCEP" = c("Land Skin Temperature","Air temperature at 2m","Temperature between 10-200cm below ground level","Clear Sky Downward Solar Flux at surface","Wind speed at 10m","Daily Precipitation Rate at surface","Specific Humidity at 2m","Volumetric Soil Moisture between 10-200cm Below Ground Level",NA),
 "NCEP1cm" = c("Land Skin Temperature","Air temperature at 2m","Temperature between 10-200cm below ground level","Clear Sky Downward Solar Flux at surface","Wind speed at 10m","Daily Precipitation Rate at surface","Specific Humidity at 2m","Volumetric Soil Moisture between 10-200cm Below Ground Level",NA),
 "micro_ncep" = c("Hourly soil temperature at 0cm", "Hourly air temperature 1cm above ground", "Hourly soil temperature 1m below ground", "Hourly solar radiation, unshaded", "Hourly wind speed 1cm above ground", NA, "Hourly relative humidity 1cm above ground", NA, "Hourly predicted snow depth"),
 "micro_usa" = c("Hourly soil temperature at 0cm", "Hourly air temperature 1cm above ground", "Hourly soil temperature 1m below ground", "Hourly solar radiation, unshaded", "Hourly wind speed 1cm above ground", NA, "Hourly relative humidity 1cm above ground", NA, "Hourly predicted snow depth"),
 "micro_global" = c("Hourly soil temperature at 0cm", "Hourly air temperature 1cm above ground", "Hourly soil temperature 1m below ground", "Hourly solar radiation, unshaded", "Hourly wind speed 1cm above ground", NA, "Hourly relative humidity 1cm above ground", NA, "Hourly predicted snow depth"),
 "micro_era5" = c("Hourly soil temperature at 0cm", "Hourly air temperature 1cm above ground", "Hourly soil temperature 1m below ground", "Hourly solar radiation, unshaded", "Hourly wind speed 1cm above ground", NA, "Hourly relative humidity 1cm above ground", NA, "Hourly predicted snow depth"),
 "NEW01" = c(NA, "Maximum monthly air temperature (C)", NA, NA, "Maximum 10m monthly wind speed (m/s)", "Total rainfall during that month (mm/month)", "% Relative humidity", NA, NA))


# ------------------------------------------------------------------
# ------------------------------------------------------------------
# -------------------------- FIGURE 1 ------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------

makeTimeSeries <- function(param, loc, month, methods) {
  p <- plot_ly()

  for (method in methods) {
    # get variable name for dataset
    inputVar <- varsDf[param, method] 

    # if this dataset has this variable
    if (!is.na(inputVar)) {
      # get variable from dataset
      df <- grabAnyData(method, inputVar, loc, month) 

      # add data to the plot 
      if(method=="NEW01") p <- p %>% add_trace(x = as.POSIXct(df$Date), y = df$Data, name = method, marker = list(color = colorsDf["color", method]), mode = 'markers')
      else p <- p %>% add_lines(x = as.POSIXct(df$Date), y = df$Data, name = method, line = list(color = colorsDf["color", method]))
    }
  }

  # Adding  GRIDMET and NEW01 Tmin when Air temperature is selected
  if (param == "Air temperature") {
    for (method in methods) {
      inputVar <- varsDf["Tmin", method]
      if (method %in% c("GRIDMET", "NEW01")) { 
        # get TMIN from dataset
        df <- grabAnyData(method, inputVar, loc, month)

        # add data to the plot 
        if(method=="NEW01") p <- p %>% add_trace(x = as.POSIXct(df$Date), y = df$Data, name = paste(method, "Tmin"), marker = list(color = colorsDf["color", method]), mode = 'markers')
        else p <- p %>% add_lines(x = as.POSIXct(df$Date), y = df$Data, name = paste(method, "Tmin"), line = list(color = colorsDf["color", method]))
      }
    }
  }

  p   # return the plot

}

get_figure_1 <- function() {
  methods <- c("GLDAS","NCEP","ERA5","GRIDMET","USCRN","NEW01")

  # getting individual plots
  ORair1 <- makeTimeSeries("Air temperature", "OR", 1, methods)
  COair1 <- makeTimeSeries("Air temperature", "CO", 1, methods)

  ORsurf1 <- makeTimeSeries("Surface temperature", "OR", 1, methods)
  COsurf1 <- makeTimeSeries("Surface temperature", "CO", 1, methods)

  ORrad1 <- makeTimeSeries("Radiation", "OR", 1, methods)
  COrad1 <- makeTimeSeries("Radiation", "CO", 1, methods)

  ORair7 <- makeTimeSeries("Air temperature", "OR", 7, methods) %>%
    layout(yaxis = list(title = "John Day, Oregon"))
  COair7 <- makeTimeSeries("Air temperature", "CO", 7, methods) %>%
    layout(yaxis = list(title = "Weld county, Colorado"))

  ORsurf7 <- makeTimeSeries("Surface temperature", "OR", 7, methods)
  COsurf7 <- makeTimeSeries("Surface temperature", "CO", 7, methods)

  ORrad7 <- makeTimeSeries("Radiation", "OR", 7, methods)
  COrad7 <- makeTimeSeries("Radiation", "CO", 7, methods)


  # creating figure
  fig1 <- subplot(ORair1, ORair7, nrows = 2, titleX = TRUE, titleY= TRUE, shareX = FALSE) %>%
    layout(showlegend = FALSE)

  fig2 <- subplot(ORsurf1, ORsurf7, nrows = 2, titleX = TRUE, shareY = TRUE, shareX = FALSE) %>%
    layout(showlegend = FALSE)

  fig3 <- subplot(ORrad1, ORrad7, nrows = 2, titleX = TRUE, shareY = TRUE, shareX = FALSE) %>%
    layout(showlegend = FALSE)

  figOR <- subplot(list(fig1,fig2,fig3), titleX = TRUE, shareX = FALSE, titleY = TRUE, shareY = FALSE) %>%
    layout(showlegend = FALSE)



  fig11 <- subplot(COair1, COair7, nrows = 2, titleX = TRUE, titleY= TRUE, shareX = FALSE) %>%
    layout(showlegend = FALSE)

  fig21 <- subplot(COsurf1, COsurf7, nrows = 2, titleX = TRUE, shareY = TRUE, shareX = FALSE) %>%
    layout(showlegend = FALSE)

  fig31 <- subplot(COrad1, COrad7, nrows = 2, titleX = TRUE, shareY = TRUE, shareX = FALSE) %>%
    layout(showlegend = FALSE)

  figCO <- subplot(list(fig11,fig21,fig31), titleX = TRUE, shareX = FALSE, titleY = TRUE, shareY = FALSE) %>%
    layout(showlegend = FALSE)


  fig <- subplot(figOR, figCO, nrows = 2, titleX = TRUE, shareX = FALSE, titleY = TRUE, shareY = FALSE) %>%
    layout(showlegend=FALSE, title="Air temperature (˚C)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Surface temperature (˚C)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Solar radiation (W/m2)")

  fig  # return figure
}

# RUN below to get figure 1
#get_figure_1()


# ------------------------------------------------------------------
# ------------------------------------------------------------------
# ---------------------------- TABLE 1 -----------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------ 

stat <- function(method1, param, loc, month, statistic){

  # grab data to compare
  df1 <- grabAnyData(method1, varsDf[param, method1], loc, month)
  df2 <- grabAnyData("USCRN", varsDf[param, "USCRN"], loc, month)

  if (method1 == "GRIDMET" && param == "Air temperature") {
    # get mean daily air temperature of USCRN for gridmet air temps
    # and get mean of min/max for gridmet
    df1_tmin <- grabAnyData(method1, "tmin", loc, month)
    df1$Date <- as.Date(df1$Date)
    df1_tmin$Date <- as.Date(df1_tmin$Date)
    df1 <- aggregate(df1$Data, by = list(df1$Date), mean) %>% set_colnames(c("Date", "Data"))
    df1_tmin <- aggregate(df1_tmin$Data, by = list(df1_tmin$Date), mean) %>% set_colnames(c("Date", "Data"))
    df1$Data <- (df1$Data + df1_tmin$Data)/2
    df2$Date <- as.Date(df2$Date)
    df2 <- aggregate(df2$Data, by = list(df2$Date), mean) %>% set_colnames(c("Date", "Data"))
  } else if(method1 == "GRIDMET"){
    # get mean daily radiation of USCRN for gridmet radiation
    df1$Date <- as.Date(df1$Date)
    df1 <- aggregate(df1$Data, by = list(df1$Date), mean) %>% set_colnames(c("Date", "Data"))
    df2$Date <- as.Date(df2$Date)
    df2 <- aggregate(df2$Data, by = list(df2$Date), mean) %>% set_colnames(c("Date", "Data"))
  }

  colnames(df1)[colnames(df1) == "Data"] <- "Data1"
  colnames(df2)[colnames(df2) == "Data"] <- "Data2"
  setDT(df1)
  setDT(df2)

  merge <- df1[df2, on = "Date"] %>%
    na.omit() %>%
    as.data.frame()

  data1 <- merge$Data1
  data2 <- merge$Data2

  if(statistic == "PCC") return(signif(unname(cor.test(x = data1, y = data2, method = "pearson")$estimate), digits = 2))
  if(statistic == "bias") return(round(abs((sum(data1) - sum(data2)) / length(data1)), digits = 2))
  if(statistic == "RMSE") return(round(sqrt(sum((data1 - data2)^2) / length(data1)), digits = 2))
}

custom_color_tile = function (...) {
  formatter("span",
            style = function(x) style(display = "block",
                                      padding = "0 4px",
                                      `color` = "black",
                                      `border-radius` = "4px",
                                      `width` = "50px",
                                      `background-color` = csscolor(gradient(as.numeric(x),
                                                                             ...))))
}

get_table_1 <- function() {
  methods <- c("ERA5","GLDAS","GRIDMET","NCEP")
  columns <- c("Methods", " ", "Air", "  ",
               "   ", "Surface", "    ",
               "     ", "Solar", "      ")
  statistics <- c("PCC","bias","RMSE")
  variables <- c("Air temperature","Surface temperature","Radiation")
  
  valuesOR1 <- c()
  valuesOR7 <- c()
  valuesCO1 <- c()
  valuesCO7 <- c()
  
  for (method in methods){
    for (var in variables){
      for (statistic in statistics){
        if(is.na(varsDf[var, method])){
          valuesOR1 <- append(valuesOR1, NA)
          valuesOR7 <- append(valuesOR7, NA)
          valuesCO1 <- append(valuesCO1, NA)
          valuesCO7 <- append(valuesCO7, NA)
        } else {
          valuesOR1 <- append(valuesOR1, stat(method, var, "OR", 1, statistic))
          valuesOR7 <- append(valuesOR7, stat(method, var, "OR", 7, statistic))
          valuesCO1 <- append(valuesCO1, stat(method, var, "CO", 1, statistic))
          valuesCO7 <- append(valuesCO7, stat(method, var, "CO", 7, statistic))
        }
      }
    }
  }
  
  tab <- matrix(valuesCO7, ncol=9, byrow=TRUE)
  tab <- cbind(methods, tab)
  columns_temp = columns <- c("Methods", " ", "Air", "  ",
                              "   ", "Surface", "    ",
                              "     ", "Solar", "      ")
  colnames(tab) <- columns_temp
  tab <- data.table(tab)
  formattable(tab, align =c("l","c","c","c","c","c","c","c","c","c"),
              list(`Methods` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                   ` `= custom_color_tile('#ffedd6','#ff8c00'),
                   `Air`= custom_color_tile('#00bd0d','#c4ffc8'),
                   `  `= custom_color_tile('#d07af5','#f1d7fc'),
                   `   `= custom_color_tile('#ffedd6','#ff8c00'),
                   `Surface`= custom_color_tile('#00bd0d','#c4ffc8'),
                   `    `= custom_color_tile('#d07af5','#f1d7fc'),
                   `     `= custom_color_tile('#ffedd6','#ff8c00'),
                   `Solar`= custom_color_tile('#00bd0d','#c4ffc8'),
                   `      `= custom_color_tile('#d07af5','#f1d7fc')
  ))
}

# get_table_1 is wrapped in a function, but I usually run through it step by 
# step and check "tab <- matrix(valuesCO7, ncol=9, byrow=TRUE)" values___ to 
# get what I need


 
# ------------------------------------------------------------------
# ------------------------------------------------------------------
# -------------------------- FIGURE 3 ------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------
plotOpTemp <- function(loc, month, op, methods) {
  fig <- plot_ly()
  dates <- vector()

  # For each selected method
  for (method in methods) {

    # Get variable name
    aTemp <- varsDf["Air temperature", method]
    sTemp <- varsDf["Surface temperature", method]
    radiation<- varsDf["Radiation", method]

    # Get air temperature data
    aTemp <- grabAnyData(method, aTemp, loc, month)
    if(method == "GRIDMET"){
      aTempTmin <- grabAnyData(method, varsDf["Tmin", method], loc, month)
      aTemp$Data <- rowMeans(cbind(aTemp$Data,aTempTmin$Data))
    }
    aTemp$Data = aTemp$Data + 273.15 # C to K

    # Get surface temperature data
    if (is.na(sTemp)) {
      if (loc == c("CO") && month==1) sTemp$Data = array(T_g_CO1, dim=c(length(aTemp$Data)))
      if (loc == c("CO") && month==7) sTemp$Data = array(T_g_CO7, dim=c(length(aTemp$Data)))
      if (loc == c("OR") && month==1) sTemp$Data = array(T_g_OR1, dim=c(length(aTemp$Data)))
      if (loc == c("OR") && month==7) sTemp$Data = array(T_g_OR7, dim=c(length(aTemp$Data)))
    }
    else {
      sTemp <- grabAnyData(method, sTemp, loc, month)
      sTemp$Data = sTemp$Data + 273.15 # C to K
    }

    # Get radiation data
    if (is.na(radiation)) radiation$Data = array(Qabs_default, dim=c(length(aTemp$Data)))
    else radiation <- grabAnyData(method, radiation, loc, month)

    # Initialize operative temperature vector
    op_temp = array(0, dim=c(length(aTemp$Data)))

    # CALCULATE OPERATIVE TEMPERATURE
    # Areas
    GMASS=8.9
    ATOTAL=(10.4713*GMASS*0.688)/10000
    AV=(0.425*GMASS*0.85)/10000 
    ASILN=(3.798*GMASS*.683)/10000 # MAX. SILHOUETTE AREA (NORMAL TO THE SUN)
    ASILP=(0.694*GMASS*.743)/10000 # MIN. SILHOUETTE AREA (POINTING TOWARD THE SUN)
    As=(ASILN + ASILP)/2           # MEAN SILHOUTTE AREA
    
    #A=sa_from_mass(8.9, "lizard")
    
    # characteristic dimension -- cube root of volume
    D=(volume_from_length(l=0.063,"lizard"))^(1/3)
    
    # radiation absorbed
    # diffuse is received by half the total area and diffuse below (from reflected, based on substrate reflectivity=0.3) is also received by half the total
    # solar absorptivity 0.9 from Gates 1980
    # assumes albedo of 0.3
    S=radiation$Data # W/m2 measured solar radiation
    df= partition_solar_radiation(method="Liu_Jordan", kt=0.6) # diffuse fraction of solar radiation, assumes kt=0.6. 
    Qabs=0.9*(As*S*(1-df)+A/2*S*(df)+A/2*S*(1-df)*0.3) # direct, diffuse, reflected

    H_L=heat_transfer_coefficient_approximation(V=0.1, D=(volume_from_length(l=0.063,"lizard"))^(1/3), K=25.7 * 10^(-3), nu=15.3 * 10^(-6), taxa = "lizard")
    #Documentation: https://trenchproject.github.io/TrenchR/reference/heat_transfer_coefficient_approximation.html
    
    op_temp = mapply(Tb_Gates, A=sa_from_mass(8.9, "lizard"), D=(volume_from_length(l=0.063,"lizard"))^(1/3), psa_dir=0.6, psa_ref=0.4, psa_air=0.95, psa_g=0.05, 
                     T_g=sTemp$Data, T_a=aTemp$Data, Qabs=Qabs, epsilon=0.95, H_L=H_L, K=0.15)
    op_temp = op_temp - 273.15 # K to C

    # add to figure
    fig <- fig %>% add_lines(x = as.POSIXct(aTemp$Date), y = op_temp, name = method, opacity = 0.75, line = list(color = colorsDf["color", method]))

    dates <- aTemp$Date
  }

  # Add activity range
  fig <- layout(fig,shapes = list(list(type = "rect", fillcolor = "green", line = list(color = "green"), opacity = 0.3,
                       x0 = dates[1], x1 = dates[length(dates)], xref = "x", y0 = 32, y1 = 37, yref = "y")))
  
  fig <- fig %>% add_lines(x = as.POSIXct(aTemp$Date), y = 43, opacity = 0.75, line = list(color = "#FF0000", dash = 'dash'))
  
  fig # Return figure
}

get_figure_3 <- function(loc, column) {
  # Defaults
  T_g_OR1 = .27 - 5 + 273.15 # surface temperature average for oregon january
  T_g_OR7 = 21 + 5 + 273.15  # surface temperature average for oregon july
  T_g_CO1 = -3 - 5 + 273.15  # surface temperature average for colorado january
  T_g_CO7 = 22 + 5 + 273.15  # surface temperature average for colorado july
  Qabs_default = 800         # solar radiation 
  
  if(column==1)      methods <- c("GLDAS","NCEP","ERA5","GRIDMET","USCRN")
  else if(column==2) methods <- c("micro_global","micro_ncep","micro_era5","micro_usa","USCRN1cm")
  else if(column==3) methods <- c("GLDAS1cm","microclim","NCEP1cm","ERA51cm","microclimUS", "USCRN1cm")
  
  data1 <- vector()
  data7 <- vector()
  if(loc == "OR"){
    data1 <- plotOpTemp("OR", 1, "gates", methods) # operative temperature oregon january
    data2 <- plotOpTemp("OR", 7, "gates", methods) # operative temperature oregon july
    subplot(data1, data2, nrows = 2, titleX = FALSE, titleY= TRUE, shareX = FALSE) %>%
      layout(showlegend = FALSE)
  } else if (loc == "CO"){
    data1 <- plotOpTemp("CO", 1, "gates", methods) # operative temperature colorado january
    data2 <- plotOpTemp("CO", 7, "gates", methods) # operative temperature colorado july
    subplot(data1, data2, nrows = 2, titleX = TRUE, titleY= TRUE, shareX = FALSE) %>%
      layout(showlegend = FALSE)
  }
}

# RUN below -- uncomment before pushing
#get_figure_3("OR",1)


# ------------------------------------------------------------------
# ------------------------------------------------------------------
# -------------------------- FIGURE 4 ------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------
methodsOP <- c("ERA5","GLDAS","GRIDMET","microclim","microclimUS","NicheMapR","NOAA_NCDC")

plotBox <- function(var) {
  fig <- plot_ly()

  # For each selected method
  for(l in c("OR","CO")) {
    for(m in c(1,7)){
      vec <- vector()
      methods_plot <- vector()
      uscrn <- 0
      aTemp <- varsDf["Air temperature", 'USCRN']
      sTemp <- varsDf["Surface temperature", 'USCRN']
      radiation<- varsDf["Radiation", 'USCRN']
      if (is.na(aTemp)) aTemp = sTemp$Data = array(T_a, dim=c(length(aTemp$Data)))
      else {
        aTemp <- grabAnyData('USCRN', aTemp, l, m)
        aTemp$Data = aTemp$Data + 273.15 # C to K
      }
      if (is.na(sTemp)) {
        if (l == c("CO") && m==1) sTemp$Data = array(T_g_CO1, dim=c(length(aTemp$Data)))
        if (l == c("CO") && m==7) sTemp$Data = array(T_g_CO7, dim=c(length(aTemp$Data)))
        if (l == c("OR") && m==1) sTemp$Data = array(T_g_OR1, dim=c(length(aTemp$Data)))
        if (l == c("OR") && m==7) sTemp$Data = array(T_g_OR7, dim=c(length(aTemp$Data)))
      }
      else {
        sTemp <- grabAnyData("USCRN", sTemp, l, m)
        sTemp$Data = sTemp$Data + 273.15 # C to K
      }
      if (is.na(radiation)) radiation$Data = array(Qabs_default, dim=c(length(aTemp$Data)))
      else radiation <- grabAnyData("USCRN", radiation, l, m)

      op_temp = array(0, dim=c(length(aTemp$Data)))
      op_temp = mapply(Tb_Gates, A=sa_from_mass(8.9, "lizard"), D=0.06, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.2,
                       T_g=sTemp$Data, T_a=aTemp$Data, Qabs=radiation$Data * sa_from_mass(8.9, "lizard") * 0.6, epsilon=0.95, H_L=10, K=0.15)
      op_temp = op_temp - 273.15
      op_tempK = op_temp + 273.15

      df <- data.frame(Column1=aTemp$Date, Column2=op_temp)
      write.csv(df, paste0("uscrn",m,l,".csv"))
      if(var == "avgTe"){
        avgTe = mean(op_temp, na.rm=TRUE)
        if(is.na(avgTe)) uscrn = 0
         else {
          uscrn = avgTe
        }
      } else if (var == "CTmax_hours"){
        ct <- op_temp[op_temp > 43]
        ct <- ct[!is.na(ct)]
        CTmax_hours = length(ct)
        if(is.na(CTmax_hours)) CTmax_hours = 0
        uscrn = CTmax_hours

      } else if (var == "activity_hours"){
        activeLower = op_temp[op_temp >= 32]
        active = activeLower[activeLower <= 37]
        active <- active[!is.na(active)]
        activity_hours = length(active)
        if(is.na(activity_hours)) activity_hours = 0
        uscrn = activity_hours
      } else if (var == "avgQmet"){
        avgQmet=0
         
        Qmet = mapply(Qmetabolism_from_mass_temp, m=8.9, T_b=na.omit(op_tempK), taxa="reptile")
        avgQmet = mean(Qmet, na.rm=TRUE)
        if(is.na(avgQmet)) avgQmet = 0
        uscrn = avgQmet
      }

      for (met in methodsOP) {

        # Get variable name/location
        aTemp <- varsDf["Air temperature", met]
        sTemp <- varsDf["Surface temperature", met]
        radiation<- varsDf["Radiation", met]

        # Get air temperature data
        if (is.na(aTemp)) aTemp = sTemp$Data = array(T_a, dim=c(length(aTemp$Data)))
        else {
          aTemp <- grabAnyData(met, aTemp, l, m)
          if(met == "GRIDMET"){
            aTempTmin <- grabAnyData(met, "tmin", l, m)
            aTemp$Data <- rowMeans(cbind(aTemp$Data,aTempTmin$Data))
          } else if(met == "NOAA_NCDC"){
            aTempTmin <- grabAnyData(met, "TMIN", l, m)
            aTemp$Data <- rowMeans(cbind(aTemp$Data,aTempTmin$Data))
          }
          aTemp$Data = aTemp$Data + 273.15 # C to K
        }

        # Get surface temperature data
        if (is.na(sTemp)) {
          if (l == c("CO") && m==1) sTemp$Data = array(T_g_CO1, dim=c(length(aTemp$Data)))
          if (l == c("CO") && m==7) sTemp$Data = array(T_g_CO7, dim=c(length(aTemp$Data)))
          if (l == c("OR") && m==1) sTemp$Data = array(T_g_OR1, dim=c(length(aTemp$Data)))
          if (l == c("OR") && m==7) sTemp$Data = array(T_g_OR7, dim=c(length(aTemp$Data)))
        }
        else {
          sTemp <- grabAnyData(met, sTemp, l, m)
          sTemp$Data = sTemp$Data + 273.15 # C to K
        }

        # Get radiation data
        if (is.na(radiation)) radiation$Data = array(Qabs_default, dim=c(length(aTemp$Data)))
        else radiation <- grabAnyData(met, radiation, l, m)

        # method data stored in aTemp, sTemp, radiation, wind

        op_temp = array(0, dim=c(length(aTemp$Data)))

        # use selected method to calculate operative temperature
        op_temp = mapply(Tb_Gates, A=sa_from_mass(8.9, "lizard"), D=0.06, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.2,
                         T_g=sTemp$Data, T_a=aTemp$Data, Qabs=radiation$Data * sa_from_mass(8.9, "lizard") * 0.6, epsilon=0.95, H_L=10, K=0.15)

        op_temp = op_temp - 273.15
        op_tempK = op_temp + 273.15

        df <- data.frame(Column1=aTemp$Date, Column2=op_temp)
        write.csv(df, paste0(met,m,l,".csv"))

        # calculate biostatistics
        if(var == "avgTe"){
          avgTe = mean(op_temp, na.rm=TRUE)
          if(is.na(avgTe)) vec = append(vec, NA)
          else {
            vec = append(vec, avgTe-uscrn)
          }
        } else if (var == "CTmax_hours"){
          ct <- op_temp[op_temp > 43]
          ct <- ct[!is.na(ct)]
          CTmax_hours = length(ct)
          if (met == "GLDAS"){ # 3 hourly
            CTmax_hours = CTmax_hours * 3
          } else if(met == "microclim"){
            CTmax_hours = CTmax_hours * 31
          }
          if(met %in% c("NOAA_NCDC","GRIDMET")){
            vec = append(vec, 25)
          } else {
            vec = append(vec, CTmax_hours-uscrn)
          }
        } else if (var == "activity_hours"){
          activeLower = op_temp[op_temp >= 32]
          active = activeLower[activeLower <= 37]
          active <- active[!is.na(active)]
          activity_hours = length(active)
          if (met == "GLDAS"){ # 3 hourly
            activity_hours = activity_hours * 3
          } else if(met == "microclim"){
            activity_hours = activity_hours * 31
          }

          if(met == "NOAA_NCDC"){
            vec = append(vec, 30)
          } else if(met == "GRIDMET"){
            vec = append(vec, 30)
          } else {
            vec = append(vec, activity_hours-uscrn)
          }
        } else if (var == "avgQmet"){
          avgQmet=0
          Qmet = mapply(Qmetabolism_from_mass_temp, m=8.9, T_b=na.omit(op_tempK), taxa="reptile")
          avgQmet = mean(Qmet, na.rm=TRUE)
          df = data.frame(col1=uscrn, col2=avgQmet, col3=Qmet)
          colnames(df) <- c("uscrn","avgQmet","Qmet")
          write(df, paste0("qmet",met,l,m,".csv"))

          vec = append(vec, avgQmet-uscrn)
        }
      }

      name <-""
      color<-""
      if(l=="OR") {
        name = "Oregon"
        color = "purple3"
      }
      if(l=="CO") {
        name = "Colorado"
        color = "yellowgreen"
      }

      if(m==1) fig <- fig %>% add_trace(x = methodsOP, type = 'scatter', mode = 'markers',  y = vec, marker = list(symbol = 'circle',size=30), name = paste(name,"January"), color=I(color))
      if(m==7) fig <- fig %>% add_trace(x = methodsOP, type = 'scatter', mode = 'markers',  y = vec, marker = list(symbol = 'triangle-up',size=30), name = paste(name,"July"), color=I(color))
    }
  }
  fig %>%
    layout(yaxis = list(title = "Hours above CTmax"), legend = list(orientation = 'h'))
}

f <- plotBox("CTmax_hours")
f

