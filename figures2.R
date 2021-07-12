# Creating figures for manuscript

#remotes::install_github("mikejohnson51/AOI")
#remotes::install_github("mikejohnson51/climateR")
#load grabNEW01 from R folder

source("server.R", local = TRUE)
source("cicerone.R", local= TRUE)
source("functions.R", local = TRUE)
options(shiny.sanitize.errors = FALSE)

library(viridis)
library(plotly)
library(formattable)
library(knitr)
library(TrenchR)
library(ggplot2)
library(reshape)
library(reshape2)
library(tidyr)
library(patchwork)
library(viridis)

#-----
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
# -------------------------- FIGURES 3 and 4 ------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------

getOpTemp <- function(loc, mo, method) {
  
  # Defaults
  T_g_OR1 = .27 - 5 + 273.15 # surface temperature average for oregon january
  T_g_OR7 = 21 + 5 + 273.15  # surface temperature average for oregon july
  T_g_CO1 = -3 - 5 + 273.15  # surface temperature average for colorado january
  T_g_CO7 = 22 + 5 + 273.15  # surface temperature average for colorado july
  Qabs_default = 800         # solar radiation 
  
  #----
  #Set up To calculations
  
  # Areas
  GMASS=8.9
  ATOTAL=(10.4713*GMASS*0.688)/10000
  AV=(0.425*GMASS*0.85)/10000 
  ASILN=(3.798*GMASS*.683)/10000 # MAX. SILHOUETTE AREA (NORMAL TO THE SUN)
  ASILP=(0.694*GMASS*.743)/10000 # MIN. SILHOUETTE AREA (POINTING TOWARD THE SUN)
  As=(ASILN + ASILP)/2           # MEAN SILHOUTTE AREA
  
  A=sa_from_mass(8.9, "lizard")
  
  # characteristic dimension -- cube root of volume
  D=(volume_from_length(l=0.063,"lizard"))^(1/3)
  
  df= partition_solar_radiation(method="Liu_Jordan", kt=0.6) # diffuse fraction of solar radiation, assumes kt=0.6. 
  
  H_L=heat_transfer_coefficient_approximation(V=0.1, D=(volume_from_length(l=0.063,"lizard"))^(1/3), K=25.7 * 10^(-3), nu=15.3 * 10^(-6), taxa = "lizard")
  #Documentation: https://trenchproject.github.io/TrenchR/reference/heat_transfer_coefficient_approximation.html
  #---
  
    # Get variable name
    aTemp <- varsDf["Air temperature", method]
    sTemp <- varsDf["Surface temperature", method]
    radiation<- varsDf["Radiation", method]
    
    # Get air temperature data
    aTemp <- grabAnyData(method, aTemp, loc, mo)
    if(method == "GRIDMET"){
      aTempTmin <- grabAnyData(method, varsDf["Tmin", method], loc, mo)
      aTemp$Data <- rowMeans(cbind(aTemp$Data,aTempTmin$Data))
    }
    aTemp$Data = aTemp$Data + 273.15 # C to K
    
    # Get surface temperature data
    if (is.na(sTemp)) {
      if (loc == c("CO") && mo==1) sTemp$Data = array(T_g_CO1, dim=c(length(aTemp$Data)))
      if (loc == c("CO") && mo==7) sTemp$Data = array(T_g_CO7, dim=c(length(aTemp$Data)))
      if (loc == c("OR") && mo==1) sTemp$Data = array(T_g_OR1, dim=c(length(aTemp$Data)))
      if (loc == c("OR") && mo==7) sTemp$Data = array(T_g_OR7, dim=c(length(aTemp$Data)))
    }else {
      sTemp <- grabAnyData(method, sTemp, loc, mo)
      sTemp$Data = sTemp$Data + 273.15 # C to K
    }
    
    # Get radiation data
    if (is.na(radiation)) {radiation$Data = array(Qabs_default, dim=c(length(aTemp$Data)))
    }else radiation <- grabAnyData(method, radiation, loc, mo)
    
    # Initialize operative temperature vector
    op_temp = array(0, dim=c(length(aTemp$Data)))
    
    # CALCULATE OPERATIVE TEMPERATURE
    # radiation absorbed
    # diffuse is received by half the total area and diffuse below (from reflected, based on substrate reflectivity=0.3) is also received by half the total
    # solar absorptivity 0.9 from Gates 1980
    # assumes albedo of 0.3
    S=radiation$Data # W/m2 measured solar radiation
    Qabs=0.9*(As*S*(1-df)+A/2*S*(df)+A/2*S*(1-df)*0.3) # direct, diffuse, reflected
    
    op_temp = mapply(Tb_Gates, A=sa_from_mass(8.9, "lizard"), D=(volume_from_length(l=0.063,"lizard"))^(1/3), psa_dir=0.6, psa_ref=0.4, psa_air=0.95, psa_g=0.05, 
                     T_g=sTemp$Data, T_a=aTemp$Data, Qabs=Qabs, epsilon=0.95, H_L=H_L, K=0.15)
    op_temp = op_temp - 273.15 # K to C
    

    dates <- aTemp$Date
    
    return(cbind(dates, op_temp) )
    
}

#make array to store operative temperature data
methods1 <- c("GLDAS","NCEP","ERA5","GRIDMET","USCRN")
methods2 <- c("micro_global","micro_ncep","micro_era5","micro_usa","USCRN1cm")
methods3 <- c("GLDAS1cm","microclim","NCEP1cm","ERA51cm","microclimUS", "USCRN1cm")
methods= c(methods1, methods2, methods3)

To= array(NA, dim=c(2,2,length(methods),744,2), 
          dimnames = list(site=c("CO","OR"),month=c("January","July"), dataset=c(methods), t=NULL, dat=c("To","Date")) )

for(metk in 1:length(methods)){
  out=getOpTemp("CO", 1, methods[metk])
  To[1,1,metk,1:nrow(out),]=out

  out=getOpTemp("CO", 7, methods[metk])
  To[1,2,metk,1:nrow(out),]=out
  
  out=getOpTemp("OR", 1, methods[metk])
  To[2,1,metk,1:nrow(out),]=out
  
  out=getOpTemp("OR", 7, methods[metk])
  To[2,2,metk,1:nrow(out),]=out
}

#=================================================
#Plot figure 3

PlotTo= function(loc,mo, ind){ 

titles=c("Weld County, Colorado, January 2017", "Weld County, Colorado, July 2017","John Day, Oregon, January 2017", "John Day, Oregon, July 2017")
  
#Gather data in long format
To.long <- reshape2::melt(To[loc,mo,,,1], value.name = "date")
To.long2 <- reshape2::melt(To[loc,mo,,,2], value.name = "value")
To.long$To= as.numeric(To.long2$value)
To.long$date= as.POSIXct(To.long$date, format="%Y-%m-%d %H:%M")

#make columns
To.long$column= NA
To.long$column[To.long$dataset %in% c("GLDAS","NCEP","ERA5","GRIDMET","USCRN")]= "Environmental Forcing Data"
To.long$column[To.long$dataset %in% c("micro_global","micro_ncep","micro_era5","micro_usa","USCRN1cm")]= "Microclimate Model Output"
To.long$column[To.long$dataset %in% c("GLDAS1cm","microclim","NCEP1cm","ERA51cm","microclimUS","USCRN1cm")]= "Microclimate Datasets"

#code source data
To.long$ForcingData=NA
To.long$ForcingData[To.long$dataset %in% c("GLDAS","GLDAS1cm")]="GLDAS"
To.long$ForcingData[To.long$dataset %in% c("micro_global","microclim")]="NEW01"
To.long$ForcingData[To.long$dataset %in% c("NCEP","micro_ncep","NCEP1cm")]="NCEP"
To.long$ForcingData[To.long$dataset %in% c("ERA5","micro_era5","ERA51cm")]="ERA5"
To.long$ForcingData[To.long$dataset %in% c("GRIDMET","micro_usa","microclimUS")]="GRIDMET"
To.long$ForcingData[To.long$dataset %in% c("USCRN","USCRN1cm")]="USCRN"

#Make factor
To.long$ForcingData= factor(To.long$ForcingData, levels=c("USCRN","GLDAS","NCEP","ERA5","GRIDMET","NEW01"), ordered=TRUE)
To.long$column= factor(To.long$column, levels=c("Environmental Forcing Data","Microclimate Model Output","Microclimate Datasets"), ordered=TRUE)

#Specify those vertically scaled to 1cm
To.long$Scaled=0
To.long$Scaled[To.long$dataset %in% c("USCRN1cm","GLDAS1cm","NCEP1cm","ERA51cm")] =1

To.fig= ggplot(data=To.long, aes(x=date, y=To, color=ForcingData, lty=factor(Scaled)))+ 
  facet_grid(.~column, scales="free", switch="y")+geom_line(aes(alpha=0.5))+
  theme_bw()+ylab("Operative Temperature (°C)")+xlab("Date")+ggtitle(titles[ind])+
  guides(lty=FALSE, alpha=FALSE)+scale_color_viridis_d(name="Forcing Data") 
#+theme(legend.position = "bottom")

To.fig= To.fig + geom_hline(yintercept=43, color="red", lty="dashed")+
  annotate("rect", xmin = To.long$date[1], xmax = max(To.long$date, na.rm=TRUE), ymin = 32, ymax = 37,
           alpha = .3,fill = "darkgreen")

return(To.fig)
}

#plot together
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/Projects/Microclimate/figures/")

pdf("Fig3_To.pdf",height = 12, width = 12)

PlotTo(1,1, 1) +PlotTo(1,2, 2) +PlotTo(2,1, 3) +PlotTo(2,2, 4) +plot_layout(ncol = 1)+ plot_layout(guides = "collect")

dev.off()

#=================================================
#Plot figure 4

CalcMetric <- function(loc, mo, var) {
  
  op_temp= To[loc,mo,,,2]
  #make numeric
  class(op_temp)<- "numeric"
  op_temp= as.data.frame(t(op_temp))
  
  lengths= apply(op_temp, 2, FUN= function(x) length(na.omit(x)))
  
  #pick metric
  if(var == "avgTe") est= colMeans(op_temp, na.rm=TRUE)
  
  if (var == "avgQmet") est= apply(op_temp+273.15, 2, FUN=function(x) mean(Qmetabolism_from_mass_temp(na.omit(x), m=8.9, taxa="reptile")) )
   
  if (var == "CTmax_hours") est= apply(op_temp, 2, FUN= function(x) length(which(x > 43)) )
 
  if (var == "activity_hours") est= apply(op_temp, 2, FUN= function(x) length(which(x >=32 & x<=37)) )
    
  #Scale to number observations
  if(var %in% c("CTmax_hours","activity_hours")) est= est * lengths[names(lengths)=="USCRN"]/lengths
  
  #calculate delta
  deltas= est-est[which(names(est)=="USCRN1cm")[1]]
  
  inds= which(names(est) %in% c("GLDAS","NCEP","ERA5","GRIDMET","USCRN") )
  deltas[inds]= est[inds]-est[names(est)=="USCRN"]
  
  return(deltas) 
}

#store data for each column
for(loc in 1:2){
  for(mo in 1:2){
    
    m1= as.data.frame(rbind(CalcMetric(loc, mo,"avgTe"),CalcMetric(loc, mo,"CTmax_hours"),CalcMetric(loc, mo,"activity_hours"),CalcMetric(loc, mo,"avgQmet") ))
    m1$site= c("CO","OR")[loc]
    m1$month= c("January","July")[mo]
    m1$metric=c("Δ Operative Temperature (°C)","Δ Hours above CTmax","Δ Potential Hours of Activity","Δ Metabolism (W)")
    
    if(loc==1&mo==1)m.all=m1
    if(!(loc==1&mo==1))m.all=rbind(m.all,m1)
  }
}

#drop observation columns
m.all= m.all[-which(names(m.all)%in%c("USCRN","USCRN1cm") )]

#to long format
m.long <- melt(m.all, id.vars=c("site", "month","metric"))
m.long$group= paste(m.long$site, m.long$month,sep="")

#determine columns
names(m.long)[4]="dataset"
m.long$column= NA
m.long$column[m.long$dataset %in% c("GLDAS","NCEP","ERA5","GRIDMET")]= "Environmental Forcing Data"
m.long$column[m.long$dataset %in% c("micro_global","micro_ncep","micro_era5","micro_usa")]= "Microclimate Model Output"
m.long$column[m.long$dataset %in% c("GLDAS1cm","microclim","NCEP1cm","ERA51cm","microclimUS")]= "Microclimate Datasets"

#make ordered factors
m.long$column= factor(m.long$column, levels=c("Environmental Forcing Data","Microclimate Model Output","Microclimate Datasets"), ordered=TRUE)
m.long$metric= factor(m.long$metric, levels=c("Δ Operative Temperature (°C)","Δ Metabolism (W)","Δ Potential Hours of Activity","Δ Hours above CTmax"), ordered=TRUE)

#plot
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/Projects/Microclimate/figures/")

pdf("Fig4_Metrics.pdf",height = 10, width = 14)

ggplot(data=m.long, aes(x=dataset, y=value, color=site,lty=month, group=group))+ 
  facet_grid(metric~column, scales="free", switch="y")+geom_point()+geom_line()+
  geom_hline(yintercept=0)+theme_bw()+ylab("")+xlab("Environmental Data Source")

dev.off()






