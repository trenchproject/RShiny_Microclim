# microclim and microclimUS

# Variables:


locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                   "lon" = c(-118.5657, -66.98880, -104.7552), 
                   "lat" = c(47.0022, 18.15110, 40.8066))
library(ncdf4)
library(MALDIquant)

ncUS <- nc_open("G:/Shared drives/TrEnCh/TSMVisualization/Data/Microclim/R/TA200cm_2017.nc")
ncUSvar <- ncvar_get(ncUS)
# 96 * 40 * 8760

microData <- function(loc) {
  lonInd <- match.closest(locs[loc, "lon"], ncUS$dim$longitude$vals)
  lat <- sort(ncUS$dim$latitude$vals)[match.closest(locs[loc, "lat"], sort(ncUS$dim$latitude$vals))]
  latInd <- match(lat, ncUS$dim$latitude$vals)
  
  Janvals <- c()
  Julvals <- c()
  for (i in 1:(24*31)) {
    Janvals <- c(Janvals, ncUSvar[lonInd, latInd, i] / 10)
    Julvals <- c(Julvals, ncUSvar[lonInd, latInd, i + 24 * 181] / 10)
  }
  array <- c(Janvals, Julvals)
  
  return (array)
}


Jan <- c()
Jul <- c()
for (i in 1:31) {
  Jan <- c(Jan, paste0("2017-01-", i))
  Jul <- c(Jul, paste0("2017-07-", i))
}
dates <- as.Date(c(Jan, Jul))
repdate <- rep(dates, each = 24)

tempclimUS <- data.frame("Date" = repdate, 
                         "Month" = rep(c(1, 7), each = length(repdate) / 2), 
                         "Hour" = rep(0:23, 62), 
                         "WA" = microData("WA"), 
                         "CO" = microData("CO"))

tempclimUS$FullDate <- format(as.POSIXct(paste0(tempclimUS$Date, " ", tempclimUS$Hour, ":00")), format = "%Y-%m-%d %H:%M")



# For outside of US

microclimProcess <- function(month) {
  
  nc <- nc_open(paste0("G:/Shared drives/TrEnCh/TSMVisualization/Data/Microclim/R/air_temperature_degC_120cm/TA120cm_", month, ".nc"))
  ncvar <- ncvar_get(nc)
  # dimension: 2159 * 852 * 24
  
  lonInd <- match.closest(locs["PR", "lon"], nc$dim$longitude$vals)
  lat <- sort(nc$dim$latitude$vals)[match.closest(locs["PR", "lat"], sort(nc$dim$latitude$vals))]
  latInd <- match(lat, nc$dim$latitude$vals)
  
  array <- c()
  for (i in 1:24) {
    array <- c(array, ncvar[lonInd, latInd, i])
  }
  
  return(array)
}

dates <- c(rep(as.Date("2017-01-15"), 24), rep(as.Date("2017-07-15"), 24))

tempclimPR <- data.frame("Date" = dates, "Hour" = rep(0:23), "PR" = c(microclimProcess(1), microclimProcess(7)))
