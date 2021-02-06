# GRIDMET

# Locations: WA and CO

# Variables (param_meta$gridmet)
#       common.name   call                                     description                        units
# 1            prcp     pr                            precipitation_amount                           mm
# 2           rhmax   rmax                 daily_maximum_relative_humidity                      Percent
# 3           rhmin   rmin                 daily_minimum_relative_humidity                      Percent
# 4            shum    sph                    daily_mean_specific_humidity                        kg/kg
# 5            srad   srad       daily_mean_shortwave_radiation_at_surface                        W/m^2
# 6        wind_dir     th                       daily_mean_wind_direction Degrees Clockwise from north
# 7            tmin   tmmn                       daily_minimum_temperature                         degK
# 8            tmax   tmmx                       daily_maximum_temperature                         degK
# 9        wind_vel     vs                           daily_mean_wind_speed                          m/s
# 10     burn_index     bi                      daily_mean_burning_index_g                     Unitless
# 11     fmoist_100  fm100                        dead_fuel_moisture_100hr                      Percent
# 12    fmoist_1000 fm1000                       dead_fuel_moisture_1000hr                      Percent
# 13 energy_release    erc           daily_mean_energy_release_component-g                     Unitless
# 14         palmer   pdsi        daily_mean_palmer_drought_severity_index                     Unitless
# 15    pet_alfalfa    etr daily_mean_reference_evapotranspiration_alfalfa                           mm
# 16      pet_grass    pet   daily_mean_reference_evapotranspiration_grass                           mm
# 17            vpd    vpd               daily_mean_vapor_pressure_deficit                          kPa


library(AOI)
library(climateR)
library(MALDIquant)
library(raster)
library(magrittr)

grabGRID <- function(param, loc, month) {
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  if (loc == "WA") {
    AOI = aoi_get(state = "WA", county = "adams")
  } else if (loc == "CO") {
    AOI = aoi_get(state = "CO", county = "weld")
  }
  # } else if (loc == "TX") {
  #   AOI = aoi_get(state = "TX", county = "brewster")
  # }
  
  p = getGridMET(AOI, param = param, startDate = paste0("2017-0", month, "-01"), endDate = paste0("2017-0", month, "-31"))
  r = raster::brick(p)
  
  array <- c()
  for (i in 1:31) {
    df <- rasterToPoints(r[[i]]) %>% as.data.frame()
    x <- sort(df$x)[match.closest(locs[loc, "lon"], sort(df$x))]
    y <- sort(df$y)[match.closest(locs[loc, "lat"], sort(df$y))]
    array <- c(array, df[df$x == x & df$y == y, 3])  # columns are ["x", "y", "data"] so 3 corresponds to the data. Colname for that is the date. 
  }
  if (param %in% c("tmin", "tmax")) {
    array <- array - 273.2
  }
  
  df <- data.frame(Date = as.Date(days), 
                   Data = array)
  
  return (df)
}


# mapGRID <- function(param, month, date) {
#   
#   AOI = aoi_get(state = "CO")
#   char_date <- ifelse(date < 10, paste0("0", date), date)
# 
#   p = getGridMET(AOI, param = param, startDate = paste0("2017-0", month, "-", char_date), endDate = paste0("2017-0", month, "-", char_date))
#   r = raster::brick(p)[[1]]
#   
#   if (param %in% c("tmin", "tmax")) {
#     r <- r - 273.15
#   }
#   
#   return(r)
# }


mapGRID <- function(param, month) {
  
  AOI = aoi_get(state = "CA")

  p = getGridMET(AOI, param = param, startDate = paste0("2017-0", month, "-01"), endDate = paste0("2017-0", month, "-31"))
  
  df <- rasterToPoints(p[[1]]) %>% as.data.frame()
  
  stations <- readxl::read_xlsx("SCAN_stations.xlsx") %>% as.data.frame()

  fullDf <- data.frame()
  for (i in 1:nrow(stations)) {

    station <- stations$Station[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    
    
    data <- df[df$x == sort(df$x)[match.closest(lon, sort(df$x))] & df$y == sort(df$y)[match.closest(lat, sort(df$y))], ]
    rownames(data) <- station
    
    fullDf <- rbind(fullDf, data)
  }

  t_fullDf <- transpose(fullDf[, 3 : length(fullDf)]) # first two columns are x and y
  setnames(t_fullDf, rownames(fullDf))

  if (param %in% c("tmax")) {
    t_fullDf <- t_fullDf - 273.15
  }
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  df <- cbind(Date = as.Date(days), t_fullDf)
  
  return (df)
}
