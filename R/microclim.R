# microclim

# Variables
# V1cm: wind speed 1 cm above ground
# TA1cm_soil_0: Air temperature (deg C) 1cm aboveground assuming soil substrate and 0% shade
# SOLR: Solar radiation (W/m2) on horizontal ground
# D100cm_soil_0: Substrate temperature (C) at 100cm, assuming soil substrate and 0% shade
# D0cm_soil_0: Substrate temperature (C) at 0cm, assuming soil substrate and 0% shade
# RH1cm_soil_0: Relative humidity (%) 1cm aboveground assuming soil substrate and 0% shade
# Tmin (for map)

# For outside of US

library(ncdf4)
library(raster)
library(AOI)

grabmicro <- function(var, loc, month) {
  locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
                     "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
                     "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
                     "offset" = c(-8, -7, -4, -7, -10))
  
  nc <- nc_open(paste0("Data/microclim/", var, "_", month, ".nc"))
  ncvar <- ncvar_get(nc)
  # dimension: 2159 * 852 * 24
  
  lonInd <- match.closest(locs[loc, "lon"], nc$dim$longitude$vals)
  lat <- sort(nc$dim$latitude$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$latitude$vals))]
  latInd <- match(lat, nc$dim$latitude$vals)
  
  i=1
  while(is.na(ncvar[lonInd, latInd, 1])){
    if(!is.na(ncvar[lonInd+i, latInd+i, 1])) {
      lonInd = lonInd+i;
      latInd = latInd+i;
      break
    }
    if(!is.na(ncvar[lonInd, latInd+i, 1]))  {
      latInd = latInd+i;
      break
    }
    if(!is.na(ncvar[lonInd+i, latInd, 1]))  {
      lonInd = lonInd+i;
      break
    }
    if(!is.na(ncvar[lonInd-i, latInd-i, 1]))  {
      lonInd = lonInd-i;
      latInd = latInd-i;
      break
    }
    if(!is.na(ncvar[lonInd-i, latInd, 1]))  {
      lonInd = lonInd-i;
      break
    }
    if(!is.na(ncvar[lonInd, latInd-i, 1]))  {
      latInd = latInd-i;
      break
    }
    i = i + 1
  }
  
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
