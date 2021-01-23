# SCAN

# Variables:
# [1] "Date"                                                        
# [2] "Station.Id"                                                  
# [3] "Air.Temperature.Maximum..degF."                              
# [4] "Air.Temperature.Minimum..degF."                              
# [5] "Precipitation.Increment..in."                                
# [6] "Relative.Humidity..pct..Mean.of.Hourly.Values"               
# [7] "Wind.Speed.Maximum..mph..Max.of.Hourly.Values"               
# [8] "Wind.Speed.Average..mph..Mean.of.Hourly.Values"              
# [9] "Solar.Radiation.Average..watt.m2..Mean.of.Hourly.Values"     
# [10] "Solar.Radiation.langley.Total..langley."                     
# [11] "Vapor.Pressure...Partial..inch_Hg..Mean.of.Hourly.Values"    
# [12] "Vapor.Pressure...Saturated..inch_Hg..Mean.of.Hourly.Values"  
# [13] "Soil.Moisture.Percent..2in..pct..Mean.of.Hourly.Values"      
# [14] "Soil.Moisture.Percent..4in..pct..Mean.of.Hourly.Values"      
# [15] "Soil.Moisture.Percent..8in..pct..Mean.of.Hourly.Values"      
# [16] "Soil.Moisture.Percent..20in..pct..Mean.of.Hourly.Values"     
# [17] "Soil.Moisture.Percent..40in..pct..Mean.of.Hourly.Values"     
# [18] "Soil.Temperature.Observed..2in..degF..Mean.of.Hourly.Values" 
# [19] "Soil.Temperature.Observed..4in..degF..Mean.of.Hourly.Values" 
# [20] "Soil.Temperature.Observed..8in..degF..Mean.of.Hourly.Values" 
# [21] "Soil.Temperature.Observed..20in..degF..Mean.of.Hourly.Values"
# [22] "Soil.Temperature.Observed..40in..degF..Mean.of.Hourly.Values"

# Element Name                   Value Type  Function Type  Function Duration  Base Data  Measurement Units    Sensor Depth  Element Code  Description                                              
# Air Temperature Maximum        Value       None           Day                N/A        Degrees fahrenheit   N/A           TMAX          Maximum air temperature - sub-hourly sampling frequency  
# Air Temperature Minimum        Value       None           Day                N/A        Degrees fahrenheit   N/A           TMIN          Minimum air temperature - sub-hourly sampling frequency  
# Precipitation Increment        Value       None           Day                N/A        Inches               N/A           PRCP          Total precipitation                                      
# Relative Humidity              Value       Mean           Day                Hourly     Percent              N/A           RHUM          Relative humidity                                        
# Wind Speed Maximum             Value       Max            Day                Hourly     Miles/hour           N/A           WSPDX         Maximum wind speed                                       
# Wind Speed Average             Value       Mean           Day                Hourly     Miles/hour           N/A           WSPDV         Average wind speed                                       
# Solar Radiation Average        Value       Mean           Day                Hourly     Watts/meter squared  N/A           SRADV         Average solar radiation                                  
# Solar Radiation/langley Total  Value       None           Day                N/A        Langleys             N/A           LRADT         Total solar radiation                                    
# Vapor Pressure - Partial       Value       Mean           Day                Hourly     Inches hg            N/A           PVPV          Partial vapor pressure                                   
# Vapor Pressure - Saturated     Value       Mean           Day                Hourly     Inches hg            N/A           SVPV          Saturated vapor pressure                                 
# Soil Moisture Percent          Value       Mean           Day                Hourly     Percent              -2            SMS           Percent volumetric soil moisture                         
# Soil Moisture Percent          Value       Mean           Day                Hourly     Percent              -4            SMS           Percent volumetric soil moisture                         
# Soil Moisture Percent          Value       Mean           Day                Hourly     Percent              -8            SMS           Percent volumetric soil moisture                         
# Soil Moisture Percent          Value       Mean           Day                Hourly     Percent              -20           SMS           Percent volumetric soil moisture                         
# Soil Moisture Percent          Value       Mean           Day                Hourly     Percent              -40           SMS           Percent volumetric soil moisture                         
# Soil Temperature Observed      Value       Mean           Day                Hourly     Degrees fahrenheit   -2            STO           Observed soil temperature                                
# Soil Temperature Observed      Value       Mean           Day                Hourly     Degrees fahrenheit   -4            STO           Observed soil temperature                                
# Soil Temperature Observed      Value       Mean           Day                Hourly     Degrees fahrenheit   -8            STO           Observed soil temperature                                
# Soil Temperature Observed      Value       Mean           Day                Hourly     Degrees fahrenheit   -20           STO           Observed soil temperature                                
# Soil Temperature Observed      Value       Mean           Day                Hourly     Degrees fahrenheit   -40           STO           Observed soil temperature 

library(magrittr)
library(utils)

grabSCAN <- function(varIndex, loc, month) {
  scan <- read.delim(paste0("Data/SCAN/SCAN_", loc, "_", month, ".txt"), sep = ",")
  
  # scan$Date <- as.Date(scan$Date)
  
  # data <- scan[scan$Date >= as.Date(paste0("2017-0", month, "-01")) & scan$Date <= as.Date(paste0("2017-0", month, "-31")),]
  
  vals <- scan[, varIndex]
  
  if (varIndex %in% c(3, 4, 18:22)) {
    vals <- (vals - 32) / 1.8 # degF to degC
  } else if (varIndex %in% c(7, 8)) {
    vals <- vals * 0.44704  # mi/hr to m/s
  } else if (varIndex == 5) { # in to mm
    vals <- vals * 25.4
  }
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  df <- data.frame("Date" = rep(days, each = 24), 
                   "Hour" = 0:23)
  df <- cbind(df, "Data" = vals)
  
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}
