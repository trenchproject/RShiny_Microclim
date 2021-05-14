# NCEP

# Variables
# air: air temperature at 2m (K)
# csdsf: Clear Sky Downward Solar Flux at surface (W/m^2)
# prate: Daily Precipitation Rate at surface (Kg/m^2/s)
# shum: Specific Humidity at 2m (kg/kg)
# skt: Land Skin Temperature (K)
# soilw: Volumetric Soil Moisture between 10-200 cm Below Ground Level (fraction)
# tmp: Temperature between 10-200 cm below ground level (K)
# uwnd: u-wind at 10m (m/s)
# vwnd: v-wind at 10m (m/s)

# Values are 6-hourly at 0, 6, 12, 18

library(ncdf4)
library(raster)
library(AOI)

grabNCEP <- function(var, loc, month) {
  locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
                     "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
                     "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
                     "offset" = c(-8, -7, -4, -8, -10))
  
  # Open variable file and pull out variable
  nc <- nc_open(paste0("Data/NCEP/", var, ".2017.nc"))
  ncvar <- ncvar_get(nc)
  
  # Find closest data point
  lonInd <- match.closest(mod(locs[loc, "lon"],360), nc$dim$lon$vals)
  lat <- sort(nc$dim$lat$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$lat$vals))]
  latInd <- match(lat, nc$dim$lat$vals)
  
  # Pull values from dates of interest
  vals <- c()
  extra <- ifelse(month == 1, 0, 4*day_of_year("2017-07-01"))
  for (i in 1 : (4 * 31)) {
    vals <- c(vals, ncvar[lonInd, latInd, i + extra])
  }
  
  # Perform necessary unit conversions
  if (var %in% c("air", "skt", "tmp")){ # K to C
    vals <- vals - 273.15
  } else if (var == "prate"){
    vals <- vals * 21600
  } else if (var == "uwnd"){
    # Get vwnd portion
    nc_vwnd <- nc_open(paste0("Data/NCEP/vwnd.2017.nc"))
    ncvar_vwnd <- ncvar_get(nc_vwnd)
    
    vwnd <- c()
    for (i in 1 : (4 * 31)) {
      vwnd <- c(vwnd, ncvar_vwnd[lonInd, latInd, i + extra])
    }
    
    vals <- sqrt(vals^2 + vwnd^2)
  }
  
  # Create date column 
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  # Create dataframe with Date and Data
  df <- data.frame("Date" = rep(days, each = 4),
                   "Hour" = c(0,6,12,18),
                   "Data" = vals)
  
  # Format date as POSIX
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}
