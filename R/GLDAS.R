# GLDAS 3-hourly
# Extent: Whole US + AK, HI -170.3, 19.5, -67.8, 71.6

# https://disc.gsfc.nasa.gov/data-access
# wget for Windows methods for download

# Variables: 
# array <- c()
# for (i in 1:nc$nvars) {
#   array <- c(array, nc$var[[i]]$name)
# }
# or, https://disc.gsfc.nasa.gov/datasets/GLDAS_NOAH025_3H_2.1/summary?keywords=GLDAS
# "time_bnds"     
# "AvgSurfT_inst" Average surface skin temperature (K) 
# "SWdown_f_tavg" Downward shortwave radiation flux (W m-2)
# "SnowDepth_inst" Snow depth (m) 
# "SoilTMP40_100cm_inst" Soil temperature (40-100 cm underground) (K)
# "SoilTMP100_200cm_inst" Soil temperature (100-200 cm underground) (K)
# "Wind_f_inst" Wind speed (m s-1)         
# "Tair_f_inst" Air temperature (K)
# "Qair_f_inst" Specific humidity (kg/kg)
# "Rainf_f_tavg" Total precipitation rate (kg m-2 s-1)
# "SoilMoi40_100cm_inst" = Soil moisture content (40-100 cm underground) (kg m-2)
# "SoilMoi100_200cm_inst" = Soil moisture content (100-200 cm underground) (kg m-2)
# "Tmin" (for map)

library(ncdf4)
library(MALDIquant)
library(magrittr)
library(AOI)
library(humidity)

grabGLDAS <- function(var, loc, month) {
  load(paste0("Data/GLDAS/",var,"_",loc,"_0",month,".Rda"))
  return (df)
}


# takes too long to process
mapGLDAS <- function(var, month) {
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()

  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  
  fullDf <- data.frame(Date = rep(days, each = 24),
                       Hour = 0:23)
  
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  fullDates <- fullDf$Date


  for (i in 1:nrow(stations)) {
    station <- stations$Name[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    offset <- -stations$Offset[i] # Data are stored as UCT. So we need adjustment to be aligned to the local time.
    
    roundUp <- ceiling(offset / 3)
    array <- c(rep(NA, mod(-offset, 3)))
    
    for (day in 1:31) {
      for (hour in seq(from = 0, to = 21, by = 3)) {
        char_day <- ifelse(day < 10, paste0("0", day), day)
        char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
        
        filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
        
        nc <- nc_open(filename)
        
        ncvar <- ncvar_get(nc, varid = var)
        
        lonInd <- match.closest(lon, nc$dim$lon$vals)
        lat <- sort(nc$dim$lat$vals)[match.closest(lat, sort(nc$dim$lat$vals))]
        latInd <- match(lat, nc$dim$lat$vals)
        
        val <- ncvar[lonInd, latInd]
        
        if (var %in% c("AvgSurfT_inst", "Tair_f_inst")) { # K to C
          val <- val - 273.15
        }
        array <- c(array, val, NA, NA)
      }
    }
    array <- array[(roundUp * 3 + 1) : (24 * 31)]
    
    df <- cbind(fullDates[1: (31 * 24 - roundUp * 3)], array %>% as.data.frame()) %>% 
      set_colnames(c("Date", station))
    
    fullDf <- merge(fullDf, df, by = "Date", all = T)
    # print(paste0(station, "added (no. ", i, ")"))
  }
  
  return (fullDf)
}
