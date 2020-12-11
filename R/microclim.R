# microclim

# Variables
# TA120cm
# V1cm: wind speed 1 cm above ground
# TA1cm_soil_0
# SOLR
# D100cm_soil_0
# D0cm_soil_0

# For outside of US

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
