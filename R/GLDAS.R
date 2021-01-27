# GLDAS 3-hourly
# Extent: -119,18,-66,48

# https://disc.gsfc.nasa.gov/data-access
# wget for Windows methods for download

# Variables: 
# array <- c()
# for (i in 1:nc$nvars) {
#   array <- c(array, nc$var[[i]]$name)
# }
# or, https://disc.gsfc.nasa.gov/datasets/GLDAS_NOAH025_3H_2.1/summary?keywords=GLDAS
# "time_bnds"     
# "Lwnet_tavg" Net longwave radiation flux (W m-2) 
# "AvgSurfT_inst" Average surface skin temperature (K) 
# "SnowDepth_inst" Snow depth (m) 
# "SoilTMP40_100cm_inst" Soil temperature (40-100 cm underground) (K)
# "SoilTMP100_200cm_inst" Soil temperature (100-200 cm underground) (K)
# "Wind_f_inst" Wind speed (m s-1)         
# "Tair_f_inst" Air temperature (K)
# "Qair_f_inst" Specific humidity (kg/kg)
# "Rainf_f_tavg" Total precipitation rate (kg m-2 s-1)
# "SoilMoi40_100cm_inst" = Soil moisture content (40-100 cm underground) (kg m-2)
# SoilMoi100_200cm_inst = Soil moisture content (100-200 cm underground) (kg m-2)
# "Tmin" (for map)

library(ncdf4)
library(MALDIquant)
library(magrittr)
library(AOI)
library(humidity)

locs <- data.frame(row.names = c("WA", "CO", "PR"), 
                   "lon" = c(-118.5657, -104.7552, -66.98880), 
                   "lat" = c(47.0022, 40.8066, 18.15110), 
                   "offset" = c(-8, -7, -4))


# nc2 <- nc_open("GLDAS7_new/GLDAS_NOAH025_3H.A20170701.0000.021.nc4.SUB.nc4")
# ncvar2 <- ncvar_get(nc2, var = "Tair_f_inst")
# val2 <- ncvar2[lonInd, latInd]
# var = "Tair_f_inst"
# 
# filename <- "G:/Shared drives/TrEnCh/TSMVisualization/Data/Microclim/R/GLDAS_7/GLDAS_NOAH025_3H.A20170701.1200.021.nc4.SUB.nc4"
# nc <- nc_open("GLDAS7_new/GLDAS_NOAH025_3H.A20170711.1200.021.nc4.SUB.nc4")
# 
# nc <- nc_open(filename)
# nc <- nc_open(paste0("G:/Shared drives/TrEnCh/TSMVisualization/Data/Microclim/R/GLDAS_7/GLDAS_NOAH025_3H.A20170710.0000.021.nc4.SUB.nc4"))

grabGLDAS("Qair_f_inst", "WA", 1)

valGLDAS <- function(nc, var, loc) {
  ncvar <- ncvar_get(nc, varid = var)
  
  lonInd <- match.closest(locs[loc, "lon"], nc$dim$lon$vals)
  lat <- sort(nc$dim$lat$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$lat$vals))]
  latInd <- match(lat, nc$dim$lat$vals)
  
  val <- ncvar[lonInd, latInd]
  
  if (var %in% c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst")) { # K to C
    val <- val - 273.15
  } else if (var == "Rainf_f_tavg") {  # kg/m^2 s to mm
    val <- val * 60 * 60 * 3
  } else if (var == "SnowDepth_inst") { # m to mm
    val <- val * 1000
  } 
  # if (var %in% c("Swnet_tavg", "Lwnet_tavg")) {
  #   val <- -val
  # }
  return (val)
}


grabGLDAS <- function(var, loc, month) {
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  
  # var <- "Qair_f_inst"
  # loc = "WA"
  # month = 1
  # 
  array <- c()
  for (day in 1:31) {
    for (hour in seq(from = 0, to = 21, by = 3)) {
      
      char_day <- ifelse(day < 10, paste0("0", day), day)
      char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
      
      filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
      
      # filename <- paste0("G:/Shared drives/TrEnCh/Projects/Microclimate/R/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
      nc <- nc_open(filename)

      val <- valGLDAS(nc, var, loc)
      array <- c(array, val)
    }
  }
  
  if (var == "Qair_f_inst") {  # Convert specific humidity to relative humidity
    arrayTemp <- c()
    for (day in 1:31) {
      for (hour in seq(from = 0, to = 21, by = 3)) {
        
        char_day <- ifelse(day < 10, paste0("0", day), day)
        char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
        
        filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
        
        # filename <- paste0("G:/Shared drives/TrEnCh/Projects/Microclimate/R/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
        nc <- nc_open(filename)
        
        val <- valGLDAS(nc, "Tair_f_inst", loc)
        arrayTemp <- c(arrayTemp, val)
      }
    }
    array <- SH2RH(q = array, t = arrayTemp, isK = FALSE)
  }
  
  offset <- -locs[loc, "offset"] # Data are stored as UCT. So we need adjustment to be aligned to the local time.
  
  roundUp <- ceiling(offset / 3)

  df <- data.frame(Date = rep(days, each = 8), 
                   Hour = seq(from = roundUp * 3 - offset, to = 21 + (roundUp * 3 - offset), by = 3))
  
  df <- cbind(df[1 : (31 * 8 - roundUp), ], "Data" = array[(1 + roundUp) : length(array)])
  
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}


# mapGLDAS <- function(var, month, date, hour) {
#   
#   hour <- hour + 7
#   if (hour > 23) {
#     date <- date + 1
#     hour <- hour - 24
#   }
#   char_date <- ifelse(date < 10, paste0("0", date), date)
#   char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
#   
#   filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_date, ".", char_hour, "00.021.nc4.SUB.nc4")
#   
#   AOI = aoi_get(state = "CO")
#   
#   raster <- raster(filename, varname = var) %>%
#     crop(AOI)
#   
#   if (var %in% c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst")) {
#     raster <- raster - 273.15
#   }
#   
#   return (raster)
# }

mapGLDAS <- function(var, month, date) {
  
  if (var %in% c("Tair_f_inst", "Tmin")) {
    max <- -1000
    min <- 1000
    for (hour in seq(from = 0, to = 21, by = 3)) {
      
      date <- ifelse(hour < 9, date + 1, date)
      
      char_date <- ifelse(date < 10, paste0("0", date), date)
      char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
      filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_date, ".", char_hour, "00.021.nc4.SUB.nc4")
      AOI = aoi_get(state = "CO")
      
      raster <- raster(filename, varname = "Tair_f_inst") %>%
        crop(AOI)
      
      max <- max(raster, max)
      min <- min(raster, min)
    }
    
    if (var == "Tmin") {
      raster <- min
    } else {
      raster <- max
    }
  } else {
    ave <- 0
    for (hour in seq(from = 0, to = 21, by = 3)) {
      
      date <- ifelse(hour < 9, date + 1, date)
      
      char_date <- ifelse(date < 10, paste0("0", date), date)
      char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
      filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_date, ".", char_hour, "00.021.nc4.SUB.nc4")
      AOI = aoi_get(state = "CO")
      
      raster <- raster(filename, varname = var) %>%
        crop(AOI)
      
      ave <- ave + raster
      
    }
    raster <- ave / 8
  }
  
  if (var %in% c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst")) {
    raster <- raster - 273.15
  }
  
  return (raster)
}

