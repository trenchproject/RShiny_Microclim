# USCRN

library(magrittr)
# Variables
# "AIR_TEMPERATURE" C
# "PRECIPITATION" mm
# "SOLAR_RADIATION": Average global solar radiation received, in watts/meter^2
# "SURFACE_TEMPERATURE" C    
# "RELATIVE_HUMIDITY" %
# "SOIL_MOISTURE_5"    
# "SOIL_TEMPERATURE_5": Average soil temperature at 5 cm below the surface, in degrees C.
# "WETNESS"        
# "WIND_1_5": Average wind speed, in meters per second, at a height of 1.5 meters.

# CO: Nunn (-104.76, 40.81)
# WA: Spokane (-117.53, 47.42)

# TX: Panther Junction (-103.21, 29.35)

grabUSCRN <- function(var, loc, month) {

  fulldf <- read.delim(paste0("Data/CRN/", loc, "_CRN.txt"), sep = "", header = F)
  
  headers <- read.delim("Data/CRN/HEADERS.txt", sep = "", header = T, skip = 1)
  
  colnames(fulldf) <- colnames(headers)
  
  time <- paste0(floor(fulldf$LST_TIME / 100), ":", fulldf$LST_TIME %% 100)
  df <- data.frame("Date" = format(as.POSIXct(paste(fulldf$LST_DATE, time), format = "%Y%m%d %H:%M"), format = "%Y-%m-%d %H:%M"),
                   "Data" = fulldf[, var]) %>% na.omit()
  
  df <- df[df$Date >= as.Date(paste0("2017-0", month, "-01")) & 
             df$Date <= as.Date(paste0("2017-0", month, "-31")), ]

  return (df) 
}

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


# crn::downloadCRN(url = CRN.HOURLY.URL, directory = HOURLY_DIR, years = 2017)
# crn::collateHourly(HOURLY_DIR)

# df <- read.table("CRN_Hourly_2021-02-17.dat")
# colnames(df) <- headers
# df[df < -9000] <- NA
# # df[df == -99] <- NA
# time <- paste0(floor(df$LST_TIME / 100), ":", df$LST_TIME %% 100)
# df$Date <- format(as.POSIXct(paste(df$LST_DATE, time), format = "%Y%m%d %H:%M"), format = "%Y-%m-%d %H:%M")
# 
# df <- df[, c("WBANNO", "Date", "LONGITUDE", "LATITUDE", "T_MAX", "T_MIN", "SOLARAD", "SUR_TEMP")]
# original <- df
# Jan <- df[df$Date >= as.Date("2017-01-01") & df$Date <= as.Date("2017-01-31"), ]
# Jul <- df[df$Date >= as.Date("2017-07-01") & df$Date <= as.Date("2017-07-31"), ]
# 
# fwrite(Jan, "USCRNmap_1")
# fwrite(Jul, "USCRNmap_7")


# Variables
# SUR_TEMP
# T_MAX
# SOLARAD

var = "SUR_TEMP"
month = 7
mapUSCRN <- function(var, month) {
  var <- ifelse(var == "SURFACE_TEMPERATURE", "SUR_TEMP", ifelse(var == "AIR_TEMPERATURE", "T_MAX", "SOLARAD"))
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  df <- fread(paste0("Data/CRN/USCRNmap_", month, ".csv")) %>% as.data.frame()

  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  fullDf <- data.frame(Date = rep(days, each = 24),
                       Hour = 0:23)
  
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  for (i in 1 : nrow(stations)) {
    station <- stations$Name[i]
    stationData <- df[df$WBANNO == stations$WBANNO[i], c("Date", var)] 
    stationData[stationData < - 30] <- NA  # Montrose, CO has surface temperature values that are completely off
    stationData <- stationData %>% set_colnames(c("Date", station)) %>% na.omit()
    
    fullDf <- merge(fullDf, stationData, by = "Date", all = T)
  }
  
  return (fullDf)
  
}
