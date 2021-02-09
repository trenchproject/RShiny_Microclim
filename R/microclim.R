# microclim

# Variables
# TA120cm
# V1cm: wind speed 1 cm above ground
# TA1cm_soil_0
# SOLR
# D100cm_soil_0
# D0cm_soil_0
# RH120cm
# Tmin (for map)

# For outside of US

library(ncdf4)
library(raster)
library(AOI)

grabmicro <- function(var, loc, month) {
  
  nc <- nc_open(paste0("Data/microclim/", var, "_", month, ".nc"))
  ncvar <- ncvar_get(nc)
  # dimension: 2159 * 852 * 24
  
  lonInd <- match.closest(locs[loc, "lon"], nc$dim$longitude$vals)
  lat <- sort(nc$dim$latitude$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$latitude$vals))]
  latInd <- match(lat, nc$dim$latitude$vals)
  
  array <- c()
  for (i in 1:24) {
    array <- c(array, ncvar[lonInd, latInd, i])
  }
  
  dates <- rep(paste0("2017-0", month, "-15"), 24)
  
  df <- data.frame("Date" = dates, 
                   "Hour" = rep(0:23), 
                   "Data" = array)
  
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return(df)
}


# mapmicro <- function(var, month, hour) {
#   
#   AOI = aoi_get(state = "CO")
#   
#   stack <- raster::stack(paste0("Data/microclim/", var, "_", month, ".nc"))
#   
#   raster <- crop(stack[[hour + 1]], AOI)
#   
#   return (raster)
# }


# mapmicro <- function(var, month) {
#   
#   AOI = aoi_get(state = "CO")
#   
#   varName <- ifelse(var == "Tmin", "TA120cm", var)
#   
#   stack <- raster::stack(paste0("Data/microclim/", var, "_", month, ".nc"))
#   
#   if (var %in% c("TA120cm", "Tmin")) {
#     max = -100
#     min = 100
#     
#     for (hour in 0:23) {
#       raster <- crop(stack[[hour + 1]], AOI)
#       
#       max <- max(raster, max)
#       min <- min(raster, min)
#     }
#     
#     if (var == "Tmin") {
#       raster <- min
#     } else {
#       raster <- max
#     }
#   } else {
#     ave = 0
#     for (hour in 0:23) {
#       raster <- crop(stack[[hour + 1]], AOI)
#       
#       ave = ave + raster
#     }
#     
#     raster <- ave / 24
#   }
#   
#   return (raster)
# }


mapmicro <- function(var, month) {
  
  AOI = aoi_get(state = "CA")
  
  stations <- readxl::read_xlsx("SCAN_stations.xlsx") %>% as.data.frame()
  
  fullDf <- data.frame(Date = 0:23)
  
  for (i in 1:nrow(stations)) {
    station <- stations$Station[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    
    nc <- nc_open(paste0("Data/microclim/", var, "_", month, ".nc"))
    ncvar <- ncvar_get(nc)
    # dimension: 2159 * 852 * 24
    
    lonInd <- match.closest(lon, nc$dim$longitude$vals)
    lat <- sort(nc$dim$latitude$vals)[match.closest(lat, sort(nc$dim$latitude$vals))]
    latInd <- match(lat, nc$dim$latitude$vals)
    
    array <- c()
    for (j in 1:24) {
      array <- c(array, ncvar[lonInd, latInd, j])
    }
    
    array <- array %>% as.data.frame() %>% set_colnames(station)
    
    fullDf <- cbind(fullDf, array)
  }
  
  return (fullDf)
}