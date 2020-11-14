# GLDAS

# Variables: 
# array <- c()
# for (i in 1:nc$nvars) {
#   array <- c(array, nc$var[[i]]$name)
# }
# "time_bnds"            "Lwnet_tavg"           "AvgSurfT_inst"        "SnowDepth_inst"      
# "SoilTMP40_100cm_inst" "Wind_f_inst"          "Tair_f_inst" 

# Function: fullGLDAS("var")


locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                   "lon" = c(-118.5657, -66.98880, -104.7552), 
                   "lat" = c(47.0022, 18.15110, 40.8066))

library(ncdf4)
library(MALDIquant)
library(magrittr)


valGLDAS <- function(nc, var, loc) {
  ncvar <- ncvar_get(nc, varid = var)
  
  lonInd <- match.closest(locs[loc, "lon"], nc$dim$lon$vals)
  lat <- sort(nc$dim$lat$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$lat$vals))]
  latInd <- match(lat, nc$dim$lat$vals)
  
  val <- ncvar[lonInd, latInd] - 273.15
  return (val)
}

arrayGLDAS <- function(var, loc) {
  array <- c()
  for (month in c(1, 7)) {
    for (day in 1:31) {
      for (hour in seq(from = 0, to = 21, by = 3)) {
        char_day <- ifelse(day < 10, paste0("0", day), day)
        char_hour <- ifelse(hour < 10, paste0("0", hour), hour)

        nc <- nc_open(paste0("GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4"))
        
        val <- valGLDAS(nc, var, loc)
        array <- c(array, val)
      }
    }
  }
  return (array)
}


fullGLDAS <- function(var) {
  Jan <- c()
  Jul <- c()
  for (i in 1:31) {
    Jan <- c(Jan, paste0("2017-01-", i))
    Jul <- c(Jul, paste0("2017-07-", i))
  }
  dates <- as.Date(c(Jan, Jul))
  repdate <- rep(dates, each = 8)
  
  df <- data.frame(Date = repdate, 
                   Hour = seq(from = 0, to = 21, by = 3), 
                   WA = arrayGLDAS(var, "WA"), 
                   PR = arrayGLDAS(var, "PR"), 
                   CO = arrayGLDAS(var, "CO"),
                   Month = rep(c(1, 7), each = 31 * 8))
  
  df$FullDate <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}


TairGLDAS <- fullGLDAS("Tair_f_inst")

