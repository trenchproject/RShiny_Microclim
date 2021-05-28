# microclimUS

library(ncdf4)
library(MALDIquant)


# Variables

# soil0cm_0pctShade: Surface temperature (C * 10) at 0% shade
# soil100cm_0pctShade: Substrate temperature (C * 10) at 0% shade
# TA1cm_0pctShade: Air temp 1cm height (C * 10)
# SOLR: Solar radiation (horizontal ground) W/m^2 * 10
# RH1cm_0pctShade: Humidity 1cm (% * 10)
# moist100cm_0pctShade: Soil moisture (% * 10)
# V1cm: wind speed 1 cm above ground
# SNOWDEP_0pctShade: Snow depth (cm * 10)


grabmicroUS <- function(var, loc, month) {
  load(paste0("Data/microclimUS/",var,"_",loc,"_0",month,".Rda"))
  return (df)
}

# grabmicroUS <- function(var, loc, month) {
# 
#   locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
#                      "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
#                      "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
#                      "offset" = c(-8, -7, -4, -7, -10))
#   
#   nc <- nc_open(paste0("Data/microclimUS/", var, "_2017.nc"))
# 
#   ncvar <- ncvar_get(nc)
# 
#   lonInd <- match.closest(locs[loc, "lon"], nc$dim$longitude$vals)
#   lat <- sort(nc$dim$latitude$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$latitude$vals))]
#   latInd <- match(lat, nc$dim$latitude$vals)
# 
#   vals <- c()
#   extra <- ifelse(month == 1, 0, 24 * 181)
#   for (i in 1 : (24 * 31)) {
#     vals <- c(vals, ncvar[lonInd, latInd, i + extra])
#   }
# 
#   days <- c()
#   for (i in 1:31) {
#     days <- c(days, paste0("2017-0", month, "-", i))
#   }
#   
#   if(var == "SNOWDEP_0pctShade"){
#     vals <- vals * 10
#   }
# 
#   df <- data.frame("Date" = rep(days, each = 24),
#                    "Hour" = 0:23,
#                    "Data" = vals / 10)
# 
#   df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
# 
#   return (df)
# }
# 
# vars <- c("soil0cm_0pctShade", "TA1cm_0pctShade", "soil100cm_0pctShade", "SOLR", "V1cm", "RH1cm_0pctShade", "moist100cm_0pctShade", "SNOWDEP_0pctShade")
# locs <- c("OR","CO","HI")
# months <- c(1,7)
# 
# for(var in vars){
#   for(loc in locs){
#     for(month in months){
#       df <- grabmicroUS(var,loc,month)
#       save(df, file = paste0("/Users/ameyer/RShiny_Microclim/Data/microclimUS/",var,"_",loc,"_0",month,".Rda"))
#     }
#   }
# }
# 
