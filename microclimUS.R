# microclimUS

locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                   "lon" = c(-118.5657, -66.98880, -104.7552), 
                   "lat" = c(47.0022, 18.15110, 40.8066), 
                   "offset" = c(-8, -4, -7))
library(ncdf4)
library(MALDIquant)


# Variables
# 96 * 40 * 8760 (24 * 365)

# TA200cm
# soil0cm_0pctShade
# soil100cm_0pctShade
# TA1cm_0pctShade: Air temp 1cm height (degC * 10)
# SOLR: Solar radiation (horizontal ground) W/m^2 * 10

grabmicroUS <- function(var, loc, month) {
  nc <- nc_open(paste0("G:/Shared drives/TrEnCh/Projects/Microclimate/R/microclimUS/", var, "_2017.nc"))
  
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

