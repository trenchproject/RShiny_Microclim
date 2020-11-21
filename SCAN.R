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

# Function: fullSCAN(varIndex)

getSCAN <- function(varIndex, loc) {
  
  scan <- read.delim(paste0(loc, "_scan.txt"), sep = ",")
  
  scan$Date <- as.Date(scan$Date)
  
  Jan <- scan[scan$Date >= as.Date("2017-01-01") & scan$Date <= as.Date("2017-01-31"),]
  Jul <- scan[scan$Date >= as.Date("2017-07-01") & scan$Date <= as.Date("2017-07-31"),]
  
  df <- rbind(Jan, Jul)

  vals <- df[, varIndex]
  
  if (varIndex %in% c(3, 4, 18:22)) {
    vals <- (vals - 32) / 1.8
  }
  
  return (vals)
}


fullSCAN <- function(varIndex) {
  Jan <- c()
  Jul <- c()
  for (i in 1:31) {
    Jan <- c(Jan, paste0("2017-01-", i))
    Jul <- c(Jul, paste0("2017-07-", i))
  }
  dates <- as.Date(c(Jan, Jul))

  df <- data.frame("Date" = dates, 
                   "Month" = rep(c(1, 7), each = 31),
                   "WA" = getSCAN(varIndex, "WA"), 
                   "PR" = getSCAN(varIndex, "PR"), 
                   "CO" = getSCAN(varIndex, "CO"))
  
  return (df)
}



grabSCAN <- function(varIndex, loc, month) {
  scan <- read.delim(paste0(loc, "_scan.txt"), sep = ",")
  
  scan$Date <- as.Date(scan$Date)
  
  data <- scan[scan$Date >= as.Date(paste0("2017-0", month, "-01")) & scan$Date <= as.Date(paste0("2017-0", month, "-31")),]
  
  vals <- data[, varIndex]
  
  if (varIndex %in% c(3, 4, 18:22)) {
    vals <- (vals - 32) / 1.8
  }
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  df <- data.frame("Date" = days, 
                   "Data" = vals)
  return (df)
}

grabSCAN(3, "WA", 1)
