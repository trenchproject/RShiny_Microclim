# USCRN 1cm

library(magrittr)

# Variables
# 1    WBANNO                         XXXXX
# 2    UTC_DATE                       YYYYMMDD
# 3    UTC_TIME                       HHmm
# 4    LST_DATE                       YYYYMMDD
# 5    LST_TIME                       HHmm
# 6    CRX_VN                         XXXXXX
# 7    LONGITUDE                      Decimal_degrees
# 8    LATITUDE                       Decimal_degrees
# 9    T_CALC                         Celsius
# 10   T_HR_AVG                       Celsius
# 11   T_MAX                          Celsius   Maximum air temperature, in degrees C, during the hour.
# 12   T_MIN                          Celsius
# 13   P_CALC                         mm
# 14   SOLARAD                        W/m^2    Average global solar radiation
# 15   SOLARAD_FLAG                   X
# 16   SOLARAD_MAX                    W/m^2
# 17   SOLARAD_MAX_FLAG               X
# 18   SOLARAD_MIN                    W/m^2
# 19   SOLARAD_MIN_FLAG               X
# 20   SUR_TEMP_TYPE                  X
# 21   SUR_TEMP                       Celsius   Average infrared surface temperature
# 22   SUR_TEMP_FLAG                  X
# 23   SUR_TEMP_MAX                   Celsius
# 24   SUR_TEMP_MAX_FLAG              X
# 25   SUR_TEMP_MIN                   Celsius
# 26   SUR_TEMP_MIN_FLAG              X
# 27   RH_HR_AVG                      %
# 28   RH_HR_AVG_FLAG                 X
# 29   SOIL_MOISTURE_5                m^3/m^3
# 30   SOIL_MOISTURE_10               m^3/m^3
# 31   SOIL_MOISTURE_20               m^3/m^3
# 32   SOIL_MOISTURE_50               m^3/m^3
# 33   SOIL_MOISTURE_100              m^3/m^3
# 34   SOIL_TEMP_5                    Celsius
# 35   SOIL_TEMP_10                   Celsius
# 36   SOIL_TEMP_20                   Celsius
# 37   SOIL_TEMP_50                   Celsius
# 38   SOIL_TEMP_100                  Celsius

# CO: Nunn (-104.76, 40.81)
# WA: Spokane (-117.53, 47.42)

grabUSCRN1cm <- function(var, loc, month) {
  
  fulldf <- read.delim(paste0("Data/CRN/", loc, "_CRN.txt"), sep = "", header = F)
  
  headers <- read.delim("Data/CRN/HEADERS_hourly.txt", sep = "", header = T, skip = 1)
  
  colnames(fulldf) <- colnames(headers)
  
  time <- paste0(floor(fulldf$LST_TIME / 100), ":", fulldf$LST_TIME %% 100)
  df <- data.frame("Date" = format(as.POSIXct(paste(fulldf$LST_DATE, time), format = "%Y%m%d %H:%M"), format = "%Y-%m-%d %H:%M"),
                   "Data" = fulldf[, var]) %>% na.omit()
  
  df <- df[df$Date >= as.Date(paste0("2017-0", month, "-01")) & 
             df$Date <= as.Date(paste0("2017-0", month, "-31")), ]
  
  if(var=="T_MAX"){
    dfsurf <- data.frame("Date" = format(as.POSIXct(paste(fulldf$LST_DATE, time), format = "%Y%m%d %H:%M"), format = "%Y-%m-%d %H:%M"),
                     "Data" = fulldf[, "SUR_TEMP"]) %>% na.omit()
    dfsurf <- dfsurf[dfsurf$Date >= as.Date(paste0("2017-0", month, "-01")) & 
                  dfsurf$Date <= as.Date(paste0("2017-0", month, "-31")), ]
    
    df$Data = mapply(air_temp_profile_neutral, df$Data, zr=2, z0=0.05, z=0.01, dfsurf$Data)
  }
  
  return (df) 
}