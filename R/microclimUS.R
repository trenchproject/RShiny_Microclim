# microclimUS

library(ncdf4)
library(MALDIquant)


# Variables
# 96 * 40 * 8760 (24 * 365)

# TA200cm
# soil0cm_0pctShade
# soil100cm_0pctShade
# TA1cm_0pctShade: Air temp 1cm height (degC * 10)
# SOLR: Solar radiation (horizontal ground) W/m^2 * 10
# RH200cm: Humidity 
# moist100cm_0pctShade: Soil moisture (% * 10)

# Tmin (for map)

grabmicroUS <- function(var, loc, month) {

  locs <- data.frame(row.names = c("WA", "CO", "PR"),
                     "lon" = c(-118.5657, -104.7552, -66.98880),
                     "lat" = c(47.0022, 40.8066, 18.15110),
                     "offset" = c(-8, -7, -4))

  nc <- nc_open(paste0("Data/microclimUS/", var, "_2017.nc"))

  ncvar <- ncvar_get(nc)

  lonInd <- match.closest(locs[loc, "lon"], nc$dim$longitude$vals)
  lat <- sort(nc$dim$latitude$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$latitude$vals))]
  latInd <- match(lat, nc$dim$latitude$vals)

  vals <- c()
  extra <- ifelse(month == 1, 0, 24 * 181)
  for (i in 1 : (24 * 31)) {
    vals <- c(vals, ncvar[lonInd, latInd, i + extra])
  }

  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }

  df <- data.frame("Date" = rep(days, each = 24),
                   "Hour" = 0:23,
                   "Data" = vals / 10)

  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")

  return (df)
}



mapmicroUS <- function(var, month) {
  
  stations <- readxl::read_xlsx("SCAN_stations.xlsx") %>% as.data.frame()
  
  nc <- nc_open(paste0("Data/microclimUS/", var, "_2017.nc"))
  
  ncvar <- ncvar_get(nc)
  
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  fullDf <- data.frame(Date = rep(days, each = 24),
                       Hour = 0:23)
    
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  for (i in 1:nrow(stations)) {
    station <- stations$Station[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
  
    
    lonInd <- match.closest(lon, nc$dim$longitude$vals)
    lat <- sort(nc$dim$latitude$vals)[match.closest(lat, sort(nc$dim$latitude$vals))]
    latInd <- match(lat, nc$dim$latitude$vals)
    
    vals <- c()
    extra <- ifelse(month == 1, 0, 24 * 181)
    for (i in 1 : (24 * 31)) {
      vals <- c(vals, ncvar[lonInd, latInd, i + extra])
    }
    
    vals <- as.data.frame(vals) %>% set_colnames(station)
    fullDf <- cbind(fullDf, vals / 10)
    
  }

  return (fullDf)
}