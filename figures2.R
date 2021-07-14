# Creating figures for manuscript

#remotes::install_github("mikejohnson51/AOI")
#remotes::install_github("mikejohnson51/climateR")
#load grabNEW01 from R folder

wd= getwd()

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
#library(mapview)
library(ggmap)
library(patchwork)
#library(htmltools)
#library(webshot)
library(tidyr)

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

#=================================
#GGPLOT VERSION

getEnvDat <- function(loc, mo, method) {
  
  # Get variable name
  aTemp.v <- varsDf["Air temperature", method]
  sTemp.v <- varsDf["Surface temperature", method]
  radiation.v<- varsDf["Radiation", method]
  
  # Get air temperature data
  aTemp <- grabAnyData(method, aTemp.v, loc, mo)
  
  #make default vectors
  sTemp=aTemp
  sTemp$Data=NA
  radiation=aTemp
  radiation$Data=NA
  
  #get min as well for Gridmet
  tmin=aTemp
  tmin$Data=NA
  
  if(method == "GRIDMET"){
    tmin <- grabAnyData(method, "tmin", loc, mo)
  }
  if(method == "NEW01"){
    tmin <- grabAnyData(method, "TMINN", loc, mo)
  }
  
  #get surface temperature data
   if(!is.na(sTemp.v)) sTemp <- grabAnyData(method, sTemp.v, loc, mo)
   
  # Get radiation data
  if(!is.na(radiation.v)) radiation <- grabAnyData(method, radiation.v, loc, mo)
  
  dates <- as.character(aTemp$Date)
  
  edat= cbind(dates, aTemp$Data, sTemp$Data, radiation$Data, tmin$Data)
  
  return(edat)
}

setwd(wd)

#make array to store operative temperature data
methods <- c("GLDAS","NCEP","ERA5","GRIDMET","USCRN","NEW01")

Ts= array(NA, dim=c(2,2,length(methods),744,5), 
          dimnames = list(site=c("CO","OR"),month=c("January","July"), dataset=c(methods), t=NULL, dat=c("Date","Ta","Ts","rad","tmin")) )

for(metk in 1:length(methods)){
  out=getEnvDat("CO", 1, methods[metk])
  Ts[1,1,metk,1:nrow(out),]=out
  
  out=getEnvDat("CO", 7, methods[metk])
  Ts[1,2,metk,1:nrow(out),]=out
  
  out=getEnvDat("OR", 1, methods[metk])
  Ts[2,1,metk,1:nrow(out),]=out
  
  out=getEnvDat("OR", 7, methods[metk])
  Ts[2,2,metk,1:nrow(out),]=out
}

#PLOT
  titles=c("Weld County, Colorado, January 2017", "Weld County, Colorado, July 2017","John Day, Oregon, January 2017", "John Day, Oregon, July 2017")
  
  #Gather data in long format
  Ts1 <- reshape2::melt(Ts[1,1,,,], value.name = "value")
  Ts1$LocMo= "Colorado, January 2017"
  Ts2 <- reshape2::melt(Ts[1,2,,,], value.name = "value")
  Ts2$LocMo= "Colorado, July 2017"
  Ts3 <- reshape2::melt(Ts[2,1,,,], value.name = "value")
  Ts3$LocMo= "Oregon, January 2017"
  Ts4 <- reshape2::melt(Ts[2,2,,,], value.name = "value")
  Ts4$LocMo= "Oregon, July 2017"
  Ts.long= rbind(Ts1,Ts2,Ts3,Ts4)
  
  #spread metrics
  Ts.wide <- spread(Ts.long, dat, value)
  Ts.wide$Ta= as.numeric(Ts.wide$Ta)
  Ts.wide$Ts= as.numeric(Ts.wide$Ts)
  Ts.wide$rad= as.numeric(Ts.wide$rad)
  Ts.wide$tmin= as.numeric(Ts.wide$tmin)
  dates= as.POSIXct(Ts.wide$Date, format="%Y-%m-%d %H:%M")
  dates[Ts.wide$dataset=="GRIDMET"]= as.POSIXct(Ts.wide$Date[Ts.wide$dataset=="GRIDMET"], format="%Y-%m-%d")
  Ts.wide$Date= dates
  
  #make environmental data long
  Ts.wide= gather(Ts.wide, metric, value, Ta:tmin)
  #change metric names
  Ts.wide$metric[Ts.wide$metric=="Ta"]="Air temperature"
  Ts.wide$metric[Ts.wide$metric=="Ts"]="Surface temperature"
  Ts.wide$metric[Ts.wide$metric=="rad"]="Radiation"
  
  #Make factor
  Ts.wide$dataset= factor(Ts.wide$dataset, levels=c("USCRN","GLDAS","NCEP","ERA5","GRIDMET","NEW01"), ordered=TRUE)
  
  #Get day of month
  Ts.wide$day= as.numeric(format(Ts.wide$Date, format = "%d"))
  Ts.wide$hour= as.numeric(format(Ts.wide$Date, format = "%H"))
  Ts.wide$dh= Ts.wide$day + Ts.wide$hour/24
  
  Ts.fig1= ggplot(data=Ts.wide[Ts.wide$metric %in%c("Air temperature","Surface temperature"),], aes(x=dh, y=value, color=dataset))+ 
    facet_grid(LocMo~metric, scales="free_y", switch="y")+geom_line(aes(alpha=0.5))+
    theme_bw()+ylab("Temperature (°C)")+xlab("Date")+
    guides(alpha=FALSE)+scale_color_viridis_d(name="Dataset")
  
  #add GRIDMET
  Ts.gm= Ts.wide[Ts.wide$dataset=="GRIDMET",]
  Ts.gm= na.omit(Ts.gm[Ts.gm$metric %in%c("tmin"), ])
  Ts.gm$metric="Air temperature"
  
  Ts.fig1= Ts.fig1 + geom_line(data=Ts.gm, aes(x=dh, y=value, color=dataset))+
    geom_point(data=Ts.new2, size=1.5, aes(x=dh, y=value, color=dataset))+
    guides(size=FALSE)
  
  #add NEW01
  Ts.new= Ts.wide[Ts.wide$dataset=="NEW01",]
  Ts.new1= na.omit(Ts.new[Ts.new$metric %in%c("Air temperature","Surface temperature"), ])
  #get min
  Ts.new2= na.omit(Ts.new[Ts.new$metric %in%c("tmin"), ])
  Ts.new2$metric="Air temperature"
  
  Ts.fig1= Ts.fig1 + geom_point(data=Ts.new1, size=1.5, aes(x=dh, y=value, color=dataset))+
    geom_point(data=Ts.new2, size=1.5, aes(x=dh, y=value, color=dataset))+
    guides(size=FALSE)+
    theme(legend.position = "bottom")+ 
    guides(fill = guide_legend(override.aes = list(shape = NA)))
  
  #radiation plot
  Ts.fig2= ggplot(data=Ts.wide[Ts.wide$metric %in%c("Radiation"),], aes(x=dh, y=value, color=dataset))+ 
    facet_grid(LocMo~metric, scales="free_y")+geom_line(aes(alpha=0.5))+
    theme_bw()+ylab("Radiation (W/m2)")+xlab("Date")+
    guides(alpha=FALSE)+scale_color_viridis_d(name="Dataset") +theme(strip.text.y = element_blank())+
    theme(legend.position = "none")
  
#plot together
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/Projects/Microclimate/figures/")

pdf("Fig1_EnvDat.pdf",height = 12, width = 12)

Ts.fig1+Ts.fig2 + plot_layout(widths = c(2, 1))

dev.off()

# ------------------------------------------------------------------
# ------------------------------------------------------------------
# ---------------------------- TABLE 1 -----------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------ 

setwd(wd)

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
  
  setwd(wd)
  
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
  
  #output data
  tab1 <- matrix(valuesOR1, ncol=9, byrow=TRUE)
  tab1 <- cbind(methods, tab1)
  tab1= as.data.frame(tab1)
  tab1$loc.mo="OR January"
  tab1$Location="OR"
  tab1$Month="January"
  
  tab2 <- matrix(valuesOR7, ncol=9, byrow=TRUE)
  tab2 <- cbind(methods, tab2)
  tab2= as.data.frame(tab2)
  tab2$loc.mo="OR July"
  tab2$Location="OR"
  tab2$Month="July"
  
  tab3 <- matrix(valuesCO1, ncol=9, byrow=TRUE)
  tab3 <- cbind(methods, tab3)
  tab3= as.data.frame(tab3)
  tab3$loc.mo="CO January"
  tab3$Location="CO"
  tab3$Month="January"
  
  tab4 <- matrix(valuesCO7, ncol=9, byrow=TRUE)
  tab4 <- cbind(methods, tab4)
  tab4= as.data.frame(tab4)
  tab4$loc.mo="CO July"
  tab4$Location="CO"
  tab4$Month="July"
  
  tab.all= rbind(tab1, tab2, tab3, tab4)
  colnames(tab.all)[2:10]=c("Air.PCC","Air.Bias","Air.RMSE","Surface.PCC","Surface.Bias","Surface.RMSE","Radiation.PCC","Radiation.Bias","Radiation.RMSE" )
  
  #to long format
  tab.long= gather(tab.all, metric, value, Air.PCC:Radiation.RMSE)
  tab.long$var="Air Temperature"
  tab.long$var[grep("Surface", tab.long$metric)]="Surface Temperature"
  tab.long$var[grep("Radiation", tab.long$metric)]="Radiation"
  
  #remove labels
  tab.long$metric= sub("Air.", "", tab.long$metric)
  tab.long$metric= sub("Surface.", "", tab.long$metric)
  tab.long$metric= sub("Radiation.", "", tab.long$metric)
  
  #value to numeric
  tab.long$value= as.numeric(tab.long$value)
  
  #spread metrics
  tab.wide <- spread(tab.long, metric, value)
  
  #-------
  # tab <- matrix(valuesCO7, ncol=9, byrow=TRUE)
  # tab <- cbind(methods, tab)
  # 
  # columns_temp = columns <- c("Methods", " ", "Air", "  ",
  #                             "   ", "Surface", "    ",
  #                             "     ", "Solar", "      ")
  # colnames(tab) <- columns_temp
  # tab <- data.table(tab)
  # formattable(tab, align =c("l","c","c","c","c","c","c","c","c","c"),
  #             list(`Methods` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  #                  ` `= custom_color_tile('#ffedd6','#ff8c00'),
  #                  `Air`= custom_color_tile('#00bd0d','#c4ffc8'),
  #                  `  `= custom_color_tile('#d07af5','#f1d7fc'),
  #                  `   `= custom_color_tile('#ffedd6','#ff8c00'),
  #                  `Surface`= custom_color_tile('#00bd0d','#c4ffc8'),
  #                  `    `= custom_color_tile('#d07af5','#f1d7fc'),
  #                  `     `= custom_color_tile('#ffedd6','#ff8c00'),
  #                  `Solar`= custom_color_tile('#00bd0d','#c4ffc8'),
  #                  `      `= custom_color_tile('#d07af5','#f1d7fc')
  # ))

  return(tab.wide)
  }

# get_table_1 is wrapped in a function, but I usually run through it step by 
# step and check "tab <- matrix(valuesCO7, ncol=9, byrow=TRUE)" values___ to 
# get what I need

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

#export_formattable(tab1, "tab1.png")

#----
#plot

tab.wide= get_table_1()

#order facets
tab.wide$var= factor(tab.wide$var, levels=c("Air Temperature", "Surface Temperature", "Radiation"), ordered=TRUE)

met.fig=ggplot(data=tab.wide, aes(x=PCC, y = RMSE, size=Bias, color=methods, shape=loc.mo))+ 
  geom_point()+facet_grid(var~., scales="free")+
  theme_bw()+ylab("Root mean squared error")+xlab("Pearson's correlation coefficient")+
  scale_shape_manual(values=c(0,15,1,16))+ labs(color="Dataset", shape = "Location & Month")
  
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/Projects/Microclimate/figures/")
pdf("Fig_Metrics.pdf",height = 10, width =5)
met.fig
dev.off()

# ------------------------------------------------------------------
# ------------------------------------------------------------------
# -------------------------- FIGURE 2 ------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------

getMap= function(m.stat="Bias", mo=7){  
  
  # compute the bounding box
  bbox <- c(-130, 25, -65, 50)
  #get map
  us.map <- get_map(location = bbox, source = "stamen", maptype = "terrain")
  
  # Collects and returns stats table produced in map_helper.R
  vars= c("Air temperature","Surface temperature","Radiation")
  
  for(var.k in 1:3){
    
    mapDatasets <- c("ERA5", "GLDAS", "NCEP", "GRIDMET")
    if(var.k==2) mapDatasets <- c("ERA5", "GLDAS", "NCEP")
    
    for(dat.k in 1:length(mapDatasets)){
  
  var.ind= match(vars[var.k], row.names(varsDf))
  ds.ind= match(mapDatasets[dat.k], colnames(varsDf))
  m.var= vars[var.k]
  
  inputVar <- varsDf[var.ind, ds.ind]
  
  setwd(wd)
  load(paste0("Data/Maps/",mapDatasets[dat.k],"_0",mo,"_",inputVar,".Rda"))
  stats$RMSE <- sqrt(stats$RMSE)
  
  #set up map parameters
  maxRawBias <- max(stats$Bias)
  maxRawRMSE <- max(stats$RMSE)
  maxRawPCC <- max(stats$PCC)
  
  roundUp <- function (percentile, category = "B") {
    if (category == "B") return (ceiling(maxRawBias * percentile * 10) / 10)
    else if (category == "R") return (ceiling(maxRawRMSE * percentile * 10) / 10)
    else if (category == "P") return (ceiling(maxRawPCC * percentile * 10) / 10)
  }
  
  if(m.var == "Air temperature"){
    # Air temperature bias quantiles: min; 0.84; 2.5; 4.9; max
    stats$BiasCat <- cut(stats$Bias, c(0, .84, 2.5, 4.9, roundUp(1)), include.lowest = T,
                         labels = c("<.84","<2.5", "<4.9", ">4.9"))  
    # Air temperature RMSE quantiles: min; 2.5; 4.2; 5.8; max
    stats$RMSECat <- cut(stats$RMSE, c(0, 2.5, 4.2, 5.8, roundUp(1)), include.lowest = T,
                         labels = c("<2.5","<4.2", "<5.8", ">5.8"))  
    # Air temperature PCC quantiles: min; .77; .86; .92; max
    stats$PCCCat <- cut(stats$PCC, c(-1, .77, .86, .92, 1), include.lowest = T,
                        labels = c("<0.77","<0.86", "<0.92", ">0.92"))
  } else if (m.var == "Surface temperature"){
    # Surface temperature bias quantiles: min; 1.2; 2.8; 6.1; max
    stats$BiasCat <- cut(stats$Bias, c(0, 1.2, 2.8, 6.1, roundUp(1)), include.lowest = T,
                         labels = c("<1.2","<2.8", "<6.1", ">6.1"))  
    # Surface temperature RMSE quantiles: min; 4.0; 6.7; 10.3; max
    stats$RMSECat <- cut(stats$RMSE, c(0, 4, 6.7, 10.3, roundUp(1)), include.lowest = T,
                         labels = c("<4","<6.7", "<10.3", ">10.3"))  
    # Surface temperature PCC quantiles: min; .79; .88; .93; max
    stats$PCCCat <- cut(stats$PCC, c(-1, .79, .88, .93, 1), include.lowest = T,
                        labels = c("<0.79","<0.88", "<0.93", ">0.93"))
  } else if (m.var == "Radiation"){
    # Solar radiation bias quantiles: min; 12.7; 29.8; 55.7; max
    stats$BiasCat <- cut(stats$Bias, c(0, 12.7, 29.8, 55.7, roundUp(1)), include.lowest = T,
                         labels = c("<12.7","<29.8", "<55.7", ">55.7"))  
    # Solar radiation RMSE quantiles: min; 66.4; 105.3; 146.9; max
    stats$RMSECat <- cut(stats$RMSE, c(0, 66.4, 105.3, 146.9, roundUp(1)), include.lowest = T,
                         labels = c("<66.4","<105.3", "<146.9", ">146.9"))  
    # Solar radiation PCC quantiles: min; .62; .86; .93; max
    stats$PCCCat <- cut(stats$PCC, c(-1, .62, .86, .93, 1), include.lowest = T,
                        labels = c("<0.62","<0.86", "<0.93", ">0.93"))
  }
  
  #combine stats across datasets
  stats$dataset=mapDatasets[dat.k]
  if(dat.k==1) stats.all=stats
  if(dat.k>1) stats.all=rbind(stats.all,stats)
  
    } #end loop datasets
    
  #combine stats across variables
  stats.all$var= vars[var.k]
  
  if(var.k==1) map.dat=stats.all
  if(var.k>1) map.dat=rbind(map.dat, stats.all)
  
  } #end loop variables
  
  #order variables
  map.dat$var== factor(map.dat$var, levels=c("Air temperature","Surface temperature","Radiation"), ordered=TRUE) 
  map.dat$dataset== factor(map.dat$dataset, levels=c("ERA5", "GLDAS",  "GRIDMET", "NCEP"), ordered=TRUE) 
  
  #add blank facet for GRIDMET surface temperature
  map.dat.na= map.dat[1,]
  map.dat.na[]=NA
  map.dat.na$var="Surface temperature"
  map.dat.na$dataset="GRIDMET"
  map.dat=rbind(map.dat,map.dat.na)
  
  if(m.stat=="Bias"){
    #by variable
    map1= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=BiasCat), data = map.dat[map.dat$var=="Air temperature",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="")+
      theme(strip.text.x = element_text(size = 16))
    
    map2= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=BiasCat), data = map.dat[map.dat$var=="Surface temperature",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="")+
      theme(strip.text.x = element_text(size = 16))
   
    map3= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=BiasCat), data = map.dat[map.dat$var=="Radiation",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="")+
      theme(strip.text.x = element_text(size = 16))
  }
  
  if(m.stat=="RMSE"){
    #by variable
    map1= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=RMSECat), data = map.dat[map.dat$var=="Air temperature",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="Root mean squared error")+
      theme(strip.text.x = element_text(size = 16))
    
    map2= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=RMSECat), data = map.dat[map.dat$var=="Surface temperature",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="Root mean squared error")+
      theme(strip.text.x = element_text(size = 16))
    
    map3= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=RMSECat), data = map.dat[map.dat$var=="Radiation",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="Root mean squared error")+
      theme(strip.text.x = element_text(size = 16))
  }
  
  if(m.stat=="PCC"){
    #by variable
    map1= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=PCCCat), data = map.dat[map.dat$var=="Air temperature",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="Pearson Correlation Coefficient")+
      theme(strip.text.x = element_text(size = 16))
    
    map2= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=PCCCat), data = map.dat[map.dat$var=="Surface temperature",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="Pearson Correlation Coefficient")+
      theme(strip.text.x = element_text(size = 16))
    
    map3= ggmap(us.map)+
      geom_point(aes(x = Lon, y = Lat, color=PCCCat), data = map.dat[map.dat$var=="Radiation",],
                 alpha = .5, size = 3)+facet_grid(dataset~var, switch="y")+theme_void()+
      theme(legend.position = "bottom")+ scale_colour_manual(values=rev(heat.colors(4)), name="Pearson Correlation Coefficient")+
      theme(strip.text.x = element_text(size = 16))
  }
  
  if(mo==1) map.all= map1+map2+map3+plot_annotation(
    title = 'January 2017')
  if(mo==7) map.all= map1+map2+map3+plot_annotation(
    title = 'July 2017')
  
  return(map.all)
}

m1= getMap(m.stat="Bias", mo=1)
m7= getMap(m.stat="Bias", mo=7)

#plot
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/Projects/Microclimate/figures/")
pdf("Fig2_Maps.pdf",height = 10, width = 10)
m1 / m7 + plot_annotation(tag_levels = 'A')
dev.off()

# ------------------------------------------------------------------
# ------------------------------------------------------------------
# -------------------------- FIGURES 3 and 4 ------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------

getOpTemp <- function(loc, mo, method) {
  setwd(wd)
  
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
  D=(volume_from_length(l=0.063,"lizard"))^(1/3) #SVL from Levy 2017
  
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
    
    #Compare with another version of Gates
    #op_temp = mapply(Tb_Gates2, A=sa_from_mass(8.9, "lizard"), D=(volume_from_length(l=0.063,"lizard"))^(1/3), 
    #             T_g=sTemp$Data, T_a=aTemp$Data, Qabs=Qabs, epsilon=0.95, V=0.1)
    
    op_temp = op_temp - 273.15 # K to C
    

    dates <- as.character(aTemp$Date)
    
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
dates=as.POSIXct(To.long$date, format="%Y-%m-%d %H:%M")
dates[To.long$dataset=="GRIDMET"]= as.POSIXct(To.long$date[To.long$dataset=="GRIDMET"], format="%Y-%m-%d")
To.long$date= dates

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
#To.long$Scaled=0
#To.long$Scaled[To.long$dataset %in% c("USCRN1cm","GLDAS1cm","NCEP1cm","ERA51cm")] =1

To.fig= ggplot(data=To.long, aes(x=date, y=To, color=ForcingData))+ 
  facet_grid(.~column, scales="free", switch="y")+geom_line(aes(alpha=0.5))+
  theme_bw()+ylab("Operative Temperature (°C)")+xlab("Date")+ggtitle(titles[ind])+
  guides(lty=FALSE, alpha=FALSE)+scale_color_viridis_d(name="Forcing Data") +
  theme(legend.position = "bottom")

To.fig= To.fig + geom_hline(yintercept=43, color="red", lty="dashed")+
  annotate("rect", xmin = To.long$date[1], xmax = max(To.long$date, na.rm=TRUE), ymin = 32, ymax = 37,
           alpha = .3,fill = "darkgreen")

#add USCRN 1cm on top and to middle column
To.obs1= To.long[To.long$dataset=="USCRN1cm",]
To.obs2= To.obs1
To.obs2$column= "Microclimate Model Output"
To.obs= rbind(To.obs1, To.obs2)

To.fig= To.fig+geom_line(data=To.obs,aes(alpha=0.5))

#remove legend except for last combination
if(ind<4) To.fig= To.fig + theme(legend.position = "none")

return(To.fig)
}

#plot together
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/Projects/Microclimate/figures/")

pdf("Fig3_To.pdf",height = 12, width = 12)

PlotTo(1,1, 1) +PlotTo(1,2, 2) +PlotTo(2,1, 3) +PlotTo(2,2, 4) +plot_layout(ncol = 1) + plot_layout(heights = c(1,1,1,1.2))

# + plot_layout(guides = "collect")

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
    m1$metric=c("Delta Operative Temperature (°C)","Delta Hours above CTmax","Delta Potential Hours of Activity","Delta Metabolism (W)")
    
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
m.long$metric= factor(m.long$metric, levels=c("Delta Operative Temperature (°C)","Delta Metabolism (W)","Delta Potential Hours of Activity","Delta Hours above CTmax"), ordered=TRUE)

#plot
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/Projects/Microclimate/figures/")

pdf("Fig4_Metrics.pdf",height = 10, width = 12)

ggplot(data=m.long, aes(x=dataset, y=value, color=site,lty=month, group=group))+ 
  facet_grid(metric~column, scales="free", switch="y")+geom_point(size=2)+geom_line(lwd=0.8)+
  geom_hline(yintercept=0)+theme_bw()+ylab("")+xlab("Environmental Data Source")+
  scale_color_manual(values=c("darkorange", "blue"))+theme(legend.position="bottom")+
  scale_linetype_manual(values=c("dotted", "solid"))
dev.off()






