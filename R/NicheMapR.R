# NicheMapR + microclima

# install_github('mrke/NicheMapR')
# install_github('ilyamaclean/microclima')


# for (loc in c("WA", "CO", "PR")) {
#   for (month in c(1, 7)) {
#     lonlat <- c(locs[loc, "lon"], locs[loc, "lat"]) # (longitude, latitude)
#     dstart <- paste0("01/0", month, "/2017") # start date
#     dfinish <- paste0("31/0", month, "/2017") # end date
# 
#     DEP <- c(0, 3, 5, 10, 15, 20, 30, 50, 100, 200) # specify depths. need 10
# 
#     micro <- micro_ncep(loc = lonlat, dstart = dstart, dfinish = dfinish, DEP = DEP,
#                         runmoist = 0, runshade = 0)
# 
#     filename <- paste0("NicheR_", loc, "_", month, ".RData")
# 
#     save(micro, file = filename)
#   }
# }


#library(RNCEP)
library(elevatr)
library(devtools)
library(NicheMapR)
#library(microclima)


# Variables

# metout/shadmet variables:
# 1 DOY - day-of-year
# 2 TIME - time of day (mins)
# 3 TALOC - air temperature (°C) at local height (specified by 'Usrhyt' variable)
# 4 TAREF - air temperature (°C) at reference height (specified by 'Refhyt', 2m default)
# 5 RHLOC - relative humidity (%) at local height (specified by 'Usrhyt' variable)
# 6 RH - relative humidity (%) at reference height (specified by 'Refhyt', 2m default)
# 7 VLOC - wind speed (m/s) at local height (specified by 'Usrhyt' variable)
# 8 VREF - wind speed (m/s) at reference height (specified by 'Refhyt', 2m default)
# 9 SNOWMELT - snowmelt (mm)
# 10 POOLDEP - water pooling on surface (mm)
# 11 PCTWET - soil surface wetness (%)
# 12 ZEN - zenith angle of sun (degrees - 90 = below the horizon)
# 13 SOLR - solar radiation (W/m2) (unshaded, adjusted for slope, aspect and horizon angle)
# 14 TSKYC - sky radiant temperature (°C)
# 15 DEW - dew presence (0 or 1)
# 16 FROST - frost presence (0 or 1)
# 17 SNOWFALL - snow predicted to have fallen (cm)
# 18 SNOWDEP - predicted snow depth (cm)
# 19 SNOWDENS - snow density (g/cm3)
# 
# soil and shadsoil variables:
# 1 DOY - day-of-year
# 2 TIME - time of day (mins)
# 3-12 D0cm ... - soil temperature (°C) at each of the 10 specified depths
# 
# soilmoist and shadmoist variables:
# 1 DOY - day-of-year
# 2 TIME - time of day (mins)
# 3-12 WC0cm ... - soil moisture (m3/m3) at each of the 10 specified depths


# metout
# TAREF - air temperature (°C) at reference height (specified by 'Refhyt', 2m default)
# 6 RH - relative humidity (%) at reference height (specified by 'Refhyt', 2m default)
# VREF - wind speed (m/s) at reference height (specified by 'Refhyt', 2m default)
# SOLR - solar radiation (W/m2) (unshaded, adjusted for slope, aspect and horizon angle)
# SNOWDEP - predicted snow depth (cm)

# soil
# D0cm - soil temperature (°C) at 0cm -> surface temperature
# D100cm - soil temperature (°C) at 100cm


grabNicheR <- function(var, loc, month) {
  
  locs <- data.frame(row.names = c("WA", "CO", "PR"), 
                     "lon" = c(-118.5657, -104.7552, -66.98880), 
                     "lat" = c(47.0022, 40.8066, 18.15110), 
                     "offset" = c(-8, -7, -4))
  
  # metout: The above ground micrometeorological conditions under the minimum specified shade
  # shadmet: The above ground micrometeorological conditions under the maximum specified shade
  # soil: Hourly predictions of the soil temperatures under the minimum specified shade
  # shadsoil: Hourly predictions of the soil temperatures under the maximum specified shade
  # soilmoist: Hourly predictions of the soil moisture under the minimum specified shade
  # shadmoist: Hourly predictions of the soil moisture under the maximum specified shade

  load(paste0("Data/NicheMapR/NicheR_", loc, "_", month, ".RData"))
  
  if (var %in% c("D0cm", "D100cm")) {
    vals <- micro$soil[, var]
  } else {
    vals <- micro$metout[, var]
  }
  
  if (var == "SNOWDEP") { # cm to mm
    vals <- vals * 10 
  }
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  offset <- -locs[loc, "offset"] # Data are stored as UCT. So we need adjustment to be aligned to the local time.
  
  
  df <- data.frame("Date" = rep(days, each = 24)[1 : (24 * 31 - offset)],
                   "Hour" = rep(0 : 23, 31)[1 : (24 * 31 - offset)],
                   "Data" = vals[(offset + 1) : (24 * 31)])
  
  
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  return (df)
}