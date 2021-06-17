# ERA-5 hourly

# Processing grib files for just one location

# When specifying geographical area, select "Sub-region extraction" and specify a very small region.
# e.g.) when getting data for (47.00, -118.56), make the region something like west: -118.6, east: -118.5, north: 47.1, south: 46.9 
# This will minimize the data size and make the processing faster.
# Then the following lines of code produces a csv file that has one row.

# db <- brick("....grib")
# 
# df <- rasterToPoints(db) %>% as.data.frame()
# lon <- sort(df$x)[match.closest("lon", sort(df$x))]
# lat <- sort(df$y)[match.closest("lat", sort(df$y))]
# 
# r <- df[df$x == lon & df$y == lat, ]
# data.table::fwrite(r, "....csv")



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


locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
                   "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
                   "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
                   "offset" = c(-8, -7, -4, -7, -10))

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

grabERA1cm <- function(varIndex, loc, month) {
  
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
  
  if(varIndex==3){
    varIndex <- 4
    df_surf <- fread(paste0("Data/ERA/ERA_", loc, ".csv")) %>% as.data.frame()
    
    vals <- c()
    for (i in 0:1487) {  # 1488 = 2 months * 31 days * 24 hours
      vals <- c(vals, df_surf[, 2 + varIndex + i * 8])  # adding 2 because the first two columns are x and y. After that, the selected variable shows up every 8 columns.
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
    
    df_surf <- data.frame("Date" = rep(days, each = 24), 
                     "Hour" = 0:23)
    df_surf <- cbind(df_surf[1: (31 * 24 - offset), ], "Data" = vals)
    df$Data = mapply(air_temp_profile_neutral, df$Data, zr=2, z0=0.05, z=0.01, df_surf$Data)
  }
  
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}
