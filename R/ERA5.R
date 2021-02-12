# ERA-5 hourly

# db <- brick(paste0("G:/Shared drives/TrEnCh/Projects/Microclimate/R/", loc, "_ERA.grib"))
# # 10416 (7 variables * 2 months * 31 days * 24 hours)
# 
# df <- rasterToPoints(db) %>% as.data.frame()
# lon <- sort(df$x)[match.closest(locs[loc, "lon"], sort(df$x))]
# lat <- sort(df$y)[match.closest(locs[loc, "lat"], sort(df$y))]
# 
# one_loc <- df[df$x == lon & df$y == lat, ]
# write.csv(one_loc, paste0(loc, "_ERA.csv"), row.names = F)


# db <- brick("ERA_US.grib")
# 
# west <- raster::crop(db, extent(-124.05, -114, 33.4, 47.15))
# WA <- raster::crop(db, extent(-119, -118, 46, 48))
# CA <- raster::crop(db, extent(-121, -115, 33.5, 41.2))
# central <- raster::crop(db, extent(-105, -104, 40, 42))
# east <- raster::crop(db, extent(-68, -66.85, 18.05, 20.15))
# 
# CADf <- rasterToPoints(CA) %>% as.data.frame()
# WADf <- rasterToPoints(WA) %>% as.data.frame()
# westDf <- rasterToPoints(west) %>% as.data.frame()
# centralDf <- rasterToPoints(central) %>% as.data.frame()
# eastDf <- rasterToPoints(east) %>% as.data.frame()
# 
# df <- rasterToPoints(db) %>% as.data.frame()
# 
# process <- function (df, lon, lat, locName) {
#   lon <- sort(df$x)[match.closest(lon, sort(df$x))]
#   lat <- sort(df$y)[match.closest(lat, sort(df$y))]
#   one_loc <- df[df$x == lon & df$y == lat, ]
#   print(one_loc[1, c(1:10)])
#   
#   write.csv(one_loc, paste0(locName, "_ERA.csv"), row.names = F)
# }


# Variables

# 1. 10m_u_component_of_wind (m/s)
# 2. 10m_v_component_of_wind
# 3. 2m_temperature (K)
# 4. skin_temperature (surface temperature)
# 5. Snow depth: Instantaneous grib-box average of the snow thickness on the ground (excluding snow on canopy).
# 6. soil_temperature_level_3 (28-100cm)
# 7. surface_solar_radiation_downwards (J/m^2)
# 8. total precipitation (m)
# 9. tmin (for the map)


# Test:
# varIndex = 6; loc = "WA"; month = 7
library(raster)
library(magrittr)
library(MALDIquant)
library(data.table)


locs <- data.frame(row.names = c("WA", "CO", "PR"), 
                   "lon" = c(-118.5657, -104.7552, -66.98880), 
                   "lat" = c(47.0022, 40.8066, 18.15110), 
                   "offset" = c(-8, -7, -4))

grabERA <- function(varIndex, loc, month) {

  df <- fread(paste0("Data/ERA/", loc, "_ERA.csv")) %>% as.data.frame()
  
  vals <- c()
  for (i in 0:1487) {  # 1488 = 2 months * 31 days * 24 hours
    vals <- c(vals, df[, 2 + varIndex + i * 8])  # adding 2 because the first two columns are x and y. After that, the selected variable shows up every 8 columns.
  }
  
  offset <- -locs[loc, "offset"] # Data are stored as UCT. So we need adjustment to be aligned to the local time.
  # The data for July comes after the data for January in the data frame. Each month has 24 hours and 31 days of data.
  if (month == 1) {
    vals <- vals[(1 + offset) : 744]
  } else if (month == 7) {
    vals <- vals[(745 + offset) : 1488]
  }
  
  if (varIndex %in% c(3, 4, 6)) { # K to C
    vals <- vals - 273.15
  } else if (varIndex == 7) { # J/m^2 to W/m^2
    vals <- vals / 3600
  } else if (varIndex %in% c(8, 5)) { # m to mm
    vals <- vals * 1000
    vals[vals < 0] <- 0
  } else if (varIndex == 1) {  # For wind speed, we want to consider the combined wind speed.
    vals2 <- c()
    for (i in 0:1487) {
      vals2 <- c(vals2, df[, 2 + 2 + i * 8])
    }
    
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

# Daily average

# month = 1
# date = 1
# varIndex = 3


# mapERA <- function(varIndex, month, date) {
#   brickERA <- brick("COmap_ERA.grib")
#   # 1/1 - 1/7 + 7/1 - 7/7
#   # 2016 layers (2 months * 7 days * 24 hours * 6 variables)
#   
#   monthIndex <- ifelse(month == 1, 0, 7)
#   offset = 7
# 
#   max <- -1000
#   min <- 1000
#   
#   if (varIndex %in% c(3, 7)) { # When air temperature, maps the daily max or min
#     for (hour in 0:23) {
#       raster <- brickERA[[varIndex + 6 * ((monthIndex + date - 1) * 24 + hour + offset)]]
#       max <- max(raster, max)
#       min <- min(raster, min)
#     }
#     
#     if (varIndex == 3) {
#       raster <- max
#     } else {
#       raster <- min
#     }
#     
#   } else { # otherwise maps the daily mean
#     ave <- 0
#     for (hour in 0:23) {
#       raster <- brickERA[[varIndex + 6 * ((monthIndex + date - 1) * 24 + hour + offset)]]
#       ave <- ave + raster
#     }
#     raster <- ave / 24
#   }
#   
#   if (varIndex %in% c(3, 4, 5)) {
#     raster <- raster - 273.15
#   }
#   
#   if (varIndex == 6) {
#     raster <- raster / 3600
#   }
#   
#   return (raster)
# }



# Extent 41.2, 33.5, -120.9, -115

# 1. 2m_temperature (K)
# 2. skin_temperature (surface temperature)
# 3. surface_net_solar_radiation (J/m^2)

# mapERA(3, 1)
mapERA <- function(varIndex, month) {
  db <- brick("ERA_CAmap.grib") 
  # 4464 layers (3 variables * 2 months * 31 days * 24 hours)
  
  varIndex <- ifelse(varIndex == 3, 1, ifelse(varIndex == 4, 2, 3))
  
  df <- rasterToPoints(db) %>% as.data.frame()
  
  stations <- readxl::read_xlsx("SCAN_stations.xlsx") %>% as.data.frame()
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  offset = 8

  fullDf <- data.frame(Date = rep(days, each = 24),
                       Hour = 0:23)
  fullDf <- fullDf[1: (31 * 24 - offset), ]
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  for (i in 1:nrow(stations)) {
    
    station <- stations$Station[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    
    lon <- sort(df$x)[match.closest(lon, sort(df$x))]
    lat <- sort(df$y)[match.closest(lat, sort(df$y))]
    one_loc <- df[df$x == lon & df$y == lat, ]
    
    vals <- c()
    for (i in 0:1487) {  # 1488 = 2 months * 31 days * 24 hours
      vals <- c(vals, one_loc[, 2 + varIndex + i * 3])  # adding 2 because the first two columns are x and y. After that, the selected variable shows up every 3 columns.
    }

    # The data for July comes after the data for January in the data frame. Each month has 24 hours and 31 days of data.
    if (month == 1) {
      vals <- vals[(1 + offset) : 744]
    } else if (month == 7) {
      vals <- vals[(745 + offset) : 1488]
    }
    
    vals <- as.data.frame(vals) %>% set_colnames(station)
    
    if (varIndex %in% c(1, 2)) { # K to C
      vals <- vals - 273.15
    }
    
    fullDf <- cbind(fullDf, vals)
  }
  
  return (fullDf)
}
