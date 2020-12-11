# ERA-5 hourly

# db <- brick(paste0("G:/Shared drives/TrEnCh/Projects/Microclimate/R/", loc, "_ERA.grib"))
# # 8928 (6 variables * 2 months * 31 days * 24 hours)
# 
# df <- rasterToPoints(db) %>% as.data.frame()
# lon <- sort(df$x)[match.closest(locs[loc, "lon"], sort(df$x))]
# lat <- sort(df$y)[match.closest(locs[loc, "lat"], sort(df$y))]
# 
# one_loc <- df[df$x == lon & df$y == lat, ]
# write.csv(one_loc, paste0(loc, "_ERA.csv"), row.names = F)

# Variables

# 1. 10m_u_component_of_wind (m/s)
# 2. 10m_v_component_of_wind
# 3. 2m_temperature (K)
# 4. skin_temperature (surface temperature)
# 5. soil_temperature_level_3 (28-100cm)
# 6. surface_net_solar_radiation (J/m^2)


# Test:
# varIndex = 6; loc = "WA"; month = 7
library(raster)
library(magrittr)
library(MALDIquant)


grabERA <- function(varIndex, loc, month) {
  locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                     "lon" = c(-118.5657, -66.98880, -104.7552), 
                     "lat" = c(47.0022, 18.15110, 40.8066), 
                     "offset" = c(-8, -4, -7))
  
  df <- fread(paste0("Data/ERA/", loc, "_ERA.csv")) %>% as.data.frame()
  
  vals <- c()
  for (i in 0:1487) {
    vals <- c(vals, df[, 2 + varIndex + i * 6])  # adding 2 because the first two columns are x and y. After that, the selected variable shows up every 6 columns.
  }
  
  offset <- -locs[loc, "offset"] # Data are stored as UCT. So we need adjustment to be aligned to the local time.
  # The data for July comes after the data for January in the data frame. Each month has 24 hours and 31 days of data.
  if (month == 1) {
    vals <- vals[(1 + offset) : 744]
  } else if (month == 7) {
    vals <- vals[(745 + offset) : 1488]
  }
  
  if (varIndex %in% c(3, 4, 5)) {
    vals <- vals - 273.15
  }
  
  if (varIndex == 6) {
    vals <- vals / 3600
  }
  
  if (varIndex == 1) {  # For wind speed, we want to consider the combined wind speed.
    vals2 <- c()
    for (i in 0:1487) {
      vals2 <- c(vals2, df[, 2 + 2 + i * 6])
    }
    
    offset <- -locs[loc, "offset"] # Data are stored as UCT. So we need adjustment to be aligned to the local time.

    # The data for July comes after the data for January in the data frame. Each month has 24 hours and 31 days of data.
    if (month == 1) {
      vals2 <- vals2[(1 + offset) : 744]
    } else if (month == 7) {
      vals2 <- vals2[(745 + offset) : 1488]
    }
    
    vals <- sqrt(vals^2 + vals2^2)
  }
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  df <- data.frame("Date" = rep(days, each = 24), 
                   "Hour" = 0:23)
  df <- cbind(df[1: (31 * 24 - offset), ], "Data" = vals)
  
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}
