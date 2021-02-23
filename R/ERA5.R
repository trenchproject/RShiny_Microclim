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

  df <- fread(paste0("Data/ERA/ERA_", loc, ".csv")) %>% as.data.frame()
  
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
  } else if (varIndex == 7) { # J/m^2 to W/m^2 for 9-hourly accumulation
    vals <- vals / 32400
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



# Extent 41.2, 33.5, -120.9, -115

# 1. 2m_temperature (K)
# 2. skin_temperature (surface temperature)
# 3. surface_net_solar_radiation (J/m^2)


# db <- brick(paste0("ERA_conus.grib"))
#  
# stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
# stations$Name[128]
# stations$Name[129]
# for (i in 129 : nrow(stations)) {
#   # r <- raster::setExtent(db, c(stations$Lon[i] - 0.15, stations$Lon[i] + 0.15, stations$Lat[i] - 0.15, stations$Lat[i] + 0.15), keepres = T, snap = T)
#   c <- crop(db, c(stations$Lon[i] - 0.1, stations$Lon[i] + 0.1, stations$Lat[i] - 0.1, stations$Lat[i] + 0.1))
#   p <- rasterToPoints(c) %>% as.data.frame()
# 
#   fwrite(p, paste0("ERA_", stations$Name[i], ".csv"))
#   print(paste0("Done with ", stations$Name[i], ". (No.", i, ")"))
# }


# 10416 (7 variables * 2 months * 31 days * 24 hours)


mapERA <- function(varIndex, month) {

  varIndex <- ifelse(varIndex == 3, 1, ifelse(varIndex == 4, 2, 3))
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }

  fullDf <- data.frame(Date = rep(days, each = 24),
                       Hour = 0:23)
  # fullDf <- fullDf[1: (31 * 24 - offset), ]
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  fullDates <- fullDf$Date
  
  for (i in 1:nrow(stations)) {
    station <- stations$Name[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    offset <- -stations$Offset[i]
    df <- fread(paste0("Data/ERA/ERA_", station, ".csv"))
    
    lon <- sort(df$x)[match.closest(lon, sort(df$x))]
    lat <- sort(df$y)[match.closest(lat, sort(df$y))]
    one_loc <- df[df$x == lon & df$y == lat, ] %>% as.data.frame()
    
    vals <- c()
    for (j in 0:1487) {  # 1488 = 2 months * 31 days * 24 hours
      vals <- c(vals, one_loc[, 2 + varIndex + j * 3])  # adding 2 because the first two columns are x and y. After that, the selected variable shows up every 3 columns.
    }
    
    # The data for July comes after the data for January in the data frame. Each month has 24 hours and 31 days of data.
    if (month == 1) {
      vals <- vals[(1 + offset) : 744]
    } else if (month == 7) {
      vals <- vals[(745 + offset) : 1488]
    }
    
    if (varIndex %in% c(1, 2)) { # K to C
      vals <- vals - 273.15
    }
    
    df <- cbind(fullDates[1: (31 * 24 - offset)], vals) %>% as.data.frame() %>% 
      set_colnames(c("Date", station))
    
    fullDf <- merge(fullDf, df, by = "Date")
  }
  
  return (fullDf)
}
