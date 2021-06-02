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
# "Tair_f_inst" 2m air temperature (K)
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

