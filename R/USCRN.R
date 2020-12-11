# USCRN

# Variables
# "AIR_TEMPERATURE"    
# "PRECIPITATION"
# "SOLAR_RADIATION": Average global solar radiation received, in watts/meter^2
# "SURFACE_TEMPERATURE"       
# "RELATIVE_HUMIDITY"
# "SOIL_MOISTURE_5"    
# "SOIL_TEMPERATURE_5": Average soil temperature at 5 cm below the surface, in degrees C.
# "WETNESS"        
# "WIND_1_5": Average wind speed, in meters per second, at a height of 1.5 meters.

# CO: Nunn (-104.76, 40.81)
# WA: Spokane (-117.53, 47.42)

grabUSCRN <- function(var, loc, month) {
  
  fulldf <- read.delim(paste0("Data/CRN/", loc, "_CRN.txt"), sep = "", header = F)
  
  headers <- read.delim("Data/CRN/HEADERS.txt", sep = "", header = T, skip = 1)
  
  colnames(fulldf) <- colnames(headers)
  
  time <- paste0(floor(fulldf$LST_TIME / 100), ":", fulldf$LST_TIME %% 100)
  
  df <- data.frame("Date" = as.POSIXct(paste(fulldf$LST_DATE, time), format = "%Y%m%d %H:%M"),
                   "Data" = fulldf[, var]) %>% na.omit()
  
  df <- df[df$Date >= as.Date(paste0("2017-0", month, "-01")) & 
             df$Date <= as.Date(paste0("2017-0", month, "-31")), ]
 
  return (df) 
}
