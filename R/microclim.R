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
  locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
                     "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
                     "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
                     "offset" = c(-8, -7, -4, -8, -10))
  
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


mapmicro <- function(var, month) {
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  fullDf <- data.frame(Date = 0:23)
  
  for (i in 1:nrow(stations)) {
    station <- stations$Name[i]
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
