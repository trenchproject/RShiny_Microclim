# ERA-5

# Variables

# 1. 10m_u_component_of_wind
# 2. 10m_v_component_of_wind
# 3. 2m_temperature
# 4. skin_temperature (surface temperature)
# 5. soil_temperature_level_3 (28-100cm)
# 6. surface_net_solar_radiation


# Function: ERAdf("varIndex")

library(raster)
library(MALDIquant)

vars <- c('10m_u_component_of_wind', '10m_v_component_of_wind', '2m_temperature', 
          'skin_temperature', 'soil_temperature_level_3', 'surface_net_solar_radiation')

getERA <- function(loc, varIndex) {

  db <- brick(paste0(loc, "_ERA.grib"))
  # 8928 (6 variables * 2 months * 31 days * 24 hours)
  
  df <- rasterToPoints(db) %>% as.data.frame()

  lon <- sort(df$x)[match.closest(locs[loc, "lon"], sort(df$x))]
  lat <- sort(df$y)[match.closest(locs[loc, "lat"], sort(df$y))]
  
  one_loc <- df[df$x == lon & df$y == lat, ]
  
  vals <- c()
  for (i in 0:1487) {
    vals <- c(vals, one_loc[, 2 + varIndex + i * 6] - 273.15)
  }
  return (vals)
}


ERAdf <- function(varIndex) {
  Jan <- c()
  Jul <- c()
  for (i in 1:31) {
    Jan <- c(Jan, paste0("2017-01-", i))
    Jul <- c(Jul, paste0("2017-07-", i))
  }
  dates <- as.Date(c(Jan, Jul))
  repdate <- rep(dates, each = 24)
  
  df <- data.frame("Date" = repdate, 
                   "Hour" = 0:23,
                   "WA" = getERA("WA", 3), 
                   "PR" = getERA("PR", 3), 
                   "CO" = getERA("CO", 3),
                   "Month" = rep(c(1, 7), each = 24 * 31))
  
  df$FullDate <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}

