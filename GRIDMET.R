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


# Function: fullGRID("var")


locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                   "lon" = c(-118.5657, -66.98880, -104.7552), 
                   "lat" = c(47.0022, 18.15110, 40.8066))

library(AOI)
library(climateR)
library(MALDIquant)
library(raster)

getGridMET(AOIWA, "burn_index", startDate = Sys.Date() - 3)
param_meta$gridmet

valsToArray <- function(loc, AOI, param, month) {
  array <- c()
  p = getGridMET(AOI, param = param, startDate = paste0("2017-0", month, "-01"), endDate = paste0("2017-0", month, "-31"))
  r = raster::brick(p)
  
  for (i in 1:31) {
    df <- rasterToPoints(r[[i]]) %>% as.data.frame()
    x <- sort(df$x)[match.closest(locs[loc, "lon"], sort(df$x))]
    y <- sort(df$y)[match.closest(locs[loc, "lat"], sort(df$y))]
    array <- c(array, df[df$x == x & df$y == y, 3])
  }
  return (array - 273.2)
}

fullArray <- function(loc, AOI, param) {
  full <- c(valsToArray(loc, AOI, param, 1), valsToArray(loc, AOI, param, 7))
  return (full)
}


fullGRID <- function(param) {
  AOIWA = aoi_get(state = "WA", county = "adams")
  AOICO = aoi_get(state = "CO", county = "weld")
  
  Jan <- c()
  Jul <- c()
  for (i in 1:31) {
    Jan <- c(Jan, paste0("2017-01-", i))
    Jul <- c(Jul, paste0("2017-07-", i))
  }
  dates <- as.Date(c(Jan, Jul))
  
  df <- data.frame(Date = dates, 
                   Month = rep(c(1, 7), each = 31), 
                   WA = fullArray("WA", AOIWA, param), 
                   CO = fullArray("CO", AOICO, param))
  return (df)
}

tmaxGRID <- fullGRID("tmax")
tminGRID <- fullGRID("tmin")
