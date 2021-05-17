## Utilized only to create stats table for spatial comparison. 
## Does not run dynamically with app.
## Uncomment last section to recreate stats table displayed in 
## spatial comparison.

grabMapData <- function(dataset, inputVar, month) {
  if (dataset == "SCAN") {
    data <- mapSCAN(inputVar, month)
  } else if (dataset == "ERA5") {
    data <- mapERA(inputVar, month)
  } else if (dataset == "GLDAS") {
    data <- mapGLDAS(inputVar, month)
  } else if (dataset == "GRIDMET") {
    data <- mapGRID(inputVar, month)
  } else if (dataset == "NOAA_NCDC") {
    data <- mapNOAA(inputVar, month)
  } else if (dataset == "microclimUS") {
    data <- mapmicroUS(inputVar, month) 
  } else if (dataset == "microclim") {
    data <- mapmicro(inputVar, month)
  } else if (dataset == "USCRN") {
    data <- mapUSCRN(inputVar, month)
  } else if (dataset == "micro_ncep") {
    data <- mapMicroNCEP(inputVar, month)
  } else if (dataset == "NCEP") {
    data <- mapNCEP(inputVar, month)
  }
  
  return (data)
}


# Extent 41.2, 33.5, -120.9, -115

# 1. 2m_temperature (K)
# 2. skin_temperature (surface temperature)
# 3. surface_net_solar_radiation (J/m^2)

# When processing grib files for a large region

# db <- brick(paste0("ERA_conus.grib"))
#
# stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
#
# for (i in 1 : nrow(stations)) {
#
#   c <- crop(db, c(stations$Lon[i] - 0.1, stations$Lon[i] + 0.1, stations$Lat[i] - 0.1, stations$Lat[i] + 0.1))
#   p <- rasterToPoints(c) %>% as.data.frame()
# 
#   fwrite(p, paste0("ERA_", stations$Name[i], ".csv"))
#   print(paste0("Done with ", stations$Name[i], ". (No.", i, ")"))
# }
# 10416 (7 variables * 2 months * 31 days * 24 hours)
mapERA <- function(varIndex, month) {
  
  varIndex <- ifelse(varIndex == 3, 1, ifelse(varIndex == 4, 2, 3)) # Converting index since this data file only contains three variables
  
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
    offset <- -stations$Offset[i]
    df <- fread(paste0("Data/ERA/ERA_", station, ".csv"))
    
    lon <- sort(df$x)[match.closest(lon, sort(df$x))]
    lat <- sort(df$y)[match.closest(lat, sort(df$y))]
    one_loc <- df[df$x == lon & df$y == lat, ] %>% as.data.frame()
    
    vals <- c()
    for (j in 0:1487) {  # 1488 = 2 months * 31 days * 24 hours
      vals <- c(vals, one_loc[, 2 + varIndex + j * 3])  # adding 2 because the first two columns are x and y. After that, the selected variable shows up every 3 columns.
    }
    
    # The data for July comes after the data for January in the data frame. Each month has 24 hours and 31 days of data.
    if (month == 1) {
      vals <- vals[(1 + offset) : 744]
    } else if (month == 7) {
      vals <- vals[(745 + offset) : 1488]
    }
    
    if (varIndex %in% c(1, 2)) { # K to C
      vals <- vals - 273.15
    } else if (varIndex == 3) { # J/m^2 to W/m^2 for 9-hourly accumulation
      vals <- vals / 32400
    }
    
    df <- cbind(fullDates[1: (31 * 24 - offset)], vals %>% as.data.frame()) %>% 
      set_colnames(c("Date", station))
    
    fullDf <- merge(fullDf, df, by = "Date", all = T)
  }
  
  return (fullDf)
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
    print(station)
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
        
        nc_close(nc)
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


mapGRID <- function(param, month) {
  
  AOI = aoi_get(state = "conus")
  
  p = getGridMET(AOI, param = param, startDate = paste0("2017-0", month, "-01"), endDate = paste0("2017-0", month, "-31"))
  
  df <- rasterToPoints(p[[1]]) %>% as.data.frame()
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  fullDf <- data.frame()
  for (i in 1:nrow(stations)) {
    station <- stations$Name[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    data <- df[df$x == sort(df$x)[match.closest(lon, sort(df$x))] & df$y == sort(df$y)[match.closest(lat, sort(df$y))], ]
    if (nrow(data) == 0) {
      data[1, ] <- NA
    }
    rownames(data) <- station
    
    fullDf <- rbind(fullDf, data)
  }
  
  t_fullDf <- transpose(fullDf[, 3 : length(fullDf)]) # first two columns are x and y
  setnames(t_fullDf, rownames(fullDf))
  
  if (param == "tmax") {
    t_fullDf <- t_fullDf - 273.15
  }
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  df <- cbind(Date = as.Date(days), t_fullDf)
  
  return (df)
}


mapmicro <- function(var, month) {
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  fullDf <- data.frame(Date = format(as.POSIXct(paste0("2017-0", month, "-15 ", 0:23, ":00")), format = "%Y-%m-%d %H:%M"))
  
  nc <- nc_open(paste0("Data/microclim/", var, "_", month, ".nc"))
  ncvar <- ncvar_get(nc)
  # dimension: 2159 * 852 * 24
  
  for (i in 1:nrow(stations)) {
    station <- stations$Name[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    
    lonInd <- match.closest(lon, nc$dim$longitude$vals)
    lat <- sort(nc$dim$latitude$vals)[match.closest(lat, sort(nc$dim$latitude$vals))]
    latInd <- match(lat, nc$dim$latitude$vals)
    
    array <- c()
    for (j in 1:24) {
      array <- c(array, ncvar[lonInd, latInd, j])
    }
    
    array <- array %>% as.data.frame() %>% set_colnames(station)
    
    fullDf <- cbind(fullDf, array)
  }
  
  return (fullDf)
}


mapmicroUS <- function(var, month) {
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
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
    station <- stations$Name[i]
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

mapNCEP <- function(var, month) {
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  nc <- nc_open(paste0("Data/NCEP/", var, ".2017.nc"))
  
  ncvar <- ncvar_get(nc)
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  fullDf <- data.frame(Date = rep(days, each = 4),
                       "Hour" = c(0,6,12,18))
  
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  for (i in 1:nrow(stations)) {
    station <- stations$Name[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    
    lonInd <- match.closest(mod(locs[loc, "lon"],360), nc$dim$lon$vals)
    lat <- sort(nc$dim$lat$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$lat$vals))]
    latInd <- match(lat, nc$dim$lat$vals)
    
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
    
    vals <- as.data.frame(vals) %>% set_colnames(station)
    fullDf <- cbind(fullDf, vals)
    
  }
  
  return (fullDf)
}


# DEP <- c(0, 3, 5, 10, 15, 20, 30, 50, 100, 200) # specify depths. need 10
# 
# for (i in 91 : nrow(stations)) {
#   lonlat <- c(stations$Lon[i], stations$Lat[i]) # (longitude, latitude)
#   for (month in c(1, 7)) {
#     dstart <- paste0("01/0", month, "/2017") # start date
#     dfinish <- paste0("31/0", month, "/2017") # end date
# 
#     micro <- micro_ncep(loc = lonlat, dstart = dstart, dfinish = dfinish, DEP = DEP,
#                         runmoist = 0, runshade = 0)
# 
#     filename <- paste0("NicheRmap_", stations$Name[i], "_", month, ".RData")
# 
#     save(micro, file = filename)
#     print(paste0("Done with no.", i))
#   }
# }


mapMicroNCEP <- function(var, month) {
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  fullDf <- data.frame(Date = rep(days, each = 24),
                       Hour = 0:23)
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  fullDates <- fullDf$Date
  
  for (i in 1 : nrow(stations)) {
    station <- stations$Name[i]
    offset <- -stations$Offset[i]
    
    load(paste0("Data/NicheMapR_map/NicheRmap_", station, "_", month, ".RData"))
    
    if (var %in% c("D0cm", "D100cm")) {
      vals <- micro$soil[, var]
    } else {
      vals <- micro$metout[, var]
    }
    
    df <- cbind(fullDates[1 : (24 * 31 - offset)], vals[(offset + 1) : (24 * 31)] %>% as.data.frame()) %>% 
      set_colnames(c("Date", station))
    
    fullDf <- merge(fullDf, df, by = "Date", all = T)
    
  }
  
  return (fullDf)
}


# Takes too long to run

mapNOAA <- function(var, month) {
  
  stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
  
  # station_data <- ghcnd_stations()
  # fwrite(station_data, "NOAA_stations.csv")
  
  station_data <- fread("NOAA_stations.csv")
  
  colnames(stations)[4] <- "id" # for meteo_nearby_stations, colname has to be "id"
  
  list <- meteo_nearby_stations(lat_lon_df = stations, 
                                station_data = station_data, 
                                lat_colname = "Lat", 
                                lon_colname = "Lon", 
                                limit = 10,
                                var = var)
  
  merged <- data.frame(Date = NA) # need an initial date column to merge later
  
  for (i in 1:nrow(stations)) {
    station <- stations$id[i]
    
    ids <- list[[i]]$id # the number of elements in the list and the number of rows in stations are equal 
    
    j = 0
    lgth = 0
    
    while (lgth == 0) { # starting from the nearest station, loops until the station contains the desired data
      j = j + 1
      data <- ncdc(datasetid = 'GHCND', 
                   stationid = paste0("GHCND:", ids[j]), 
                   token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
                   startdate = paste0("2017-0", month, "-01"), 
                   enddate = paste0("2017-0", month, "-31"),
                   datatypeid = var,
                   add_units = T)
      
      lgth <- length(data$data)
      
    }
    values <- data$data %>% as.data.frame() %>%
      plotly::select(date, value) %>% set_colnames(c("Date", "Value"))
    values$Value <- values$Value / 10
    
    merged <- merge(merged, values, by = "Date", all = T)
    colnames(merged)[i + 1] <- station
  }
  
  merged$Date <- format(as.POSIXct(merged$Date), format = "%Y-%m-%d")
  
  return (merged[-nrow(merged), ]) # the last row is just NAs
}


mapSCAN <- function(varIndex, month) {
  
  stations <- readxl::read_xlsx("SCAN_stations.xlsx") %>% as.data.frame()
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  fullDf <- data.frame(Date = rep(days, each = 24),
                       Hour = 0:23)
  
  fullDf$Date <- format(as.POSIXct(paste0(fullDf$Date, " ", fullDf$Hour, ":00")), format = "%Y-%m-%d %H:%M")
  
  for (i in 1 : nrow(stations)) {
    station <- stations$Station[i]
    data <- read.delim(paste0("Data/SCANmap/SCAN_", station, "_", month, ".txt"), sep = ",")
    
    df <- as.data.frame(data[, c(1, varIndex)]) %>% set_colnames(c("Date", station))
    
    # if (varIndex %in% c(3, 4, 18:22)) {
    #   df <- (df - 32) / 1.8 # degF to degC
    # } else if (varIndex %in% c(9, 10)) {
    #   df <- df * 0.44704  # mi/hr to m/s
    # } else if (varIndex == 5) { # in to mm
    #   df <- df * 25.4
    # }
    
    fullDf <- merge(fullDf, df, by = "Date", all = T)
    
  }
  
  if (varIndex %in% c(3, 4, 18:22)) {
    fullDf[, c(-1, -2)] <- (fullDf[, c(-1, -2)] - 32) / 1.8 # degF to degC
  }
  
  return (fullDf)
}




# variables <- c("Surface temperature", "Air temperature", "Soil temperature (1 m deep)", "Radiation", "Wind speed", "Precipitation", "Relative humidity", "Soil moisture", "Snow Depth")
# 
# varsDf <- data.frame(row.names = c(variables, "Tmin"),
#                      "ERA5" = c(4, 3, 6, 7, 1, 8, NA, NA, 5, 9),
#                      "GLDAS" = c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst", "SWdown_f_tavg", "Wind_f_inst", "Rainf_f_tavg", "Qair_f_inst", "SoilMoi40_100cm_inst", "SnowDepth_inst", "Tmin"),
#                      "GRIDMET" = c(NA, "tmax", NA, "srad", "wind_vel", "prcp", NA, NA, NA, "tmin"),
#                      "NOAA_NCDC" = c(NA, "TMAX", NA, NA, NA, "PRCP", NA, NA, "SNWD", "TMIN"),
#                      "microclimUS" = c("soil0cm_0pctShade", "TA200cm", "soil100cm_0pctShade", "SOLR", NA, NA, "RH200cm", "moist100cm_0pctShade", NA, "Tmin"),
#                      "microclim" = c("D0cm_soil_0", "TA120cm", "D100cm_soil_0", "SOLR", "V1cm", NA, "RH120cm", NA, NA, "Tmin"),
#                      "USCRN" = c("SUR_TEMP", "T_MAX", "SOIL_TEMP_100", "SOLARAD", NA, NA, "RH_HR_AVG", "SOIL_MOISTURE_100", NA, NA),
#                      "NCEP" = c("skt","air","tmp","csdsf","uwnd","prate",NA,"soilw",NA,NA),
#                      #"SNODAS" = c(NA, NA, NA, NA, NA, NA, NA, NA, "SNOWH", NA),
#                      "NicheMapR" = c("D0cm", "TAREF", "D100cm", "SOLR", "VREF", NA, "RH", NA, "SNOWDEP", NA))
# 
# colorsDf <- data.frame(row.names = c("color"),
#                        "ERA5" = c('#04d9ff'),
#                        "GLDAS" = c('#ff7f0e'),
#                        "GRIDMET" = c('#2ca02c'),
#                        "NOAA_NCDC" = c('39FF14'),
#                        "microclimUS" = c('ff00c8'),
#                        "microclim" = c('160ef0'),
#                        "USCRN" = c('#000000'),
#                        "NCEP" = c('#7f7f7f'),
#                        "NicheMapR" = c('#bcbd22'))
# 
# nameDf <- data.frame(row.names = variables,
#                      "ERA5" = c("Hourly skin temperature", "Hourly air temperature 2 m above ground", "Hourly soil temperature 28-100 cm below ground", "Hourly surface net solar radiation", "Hourly wind speed 10 m above ground", "Total precipitation", NA, NA, "Hourly snow depth"),
#                      "GLDAS" = c("3-hourly average surface skin temperature", "3-hourly average air temperature", "3-hourly average soil temperature 40-100 cm below ground", "3-hourly net longwave radiation flux", "3-hourly average wind speed", "Total precipitation", "3-hourly relative humidity", "3-hourly average soil moisture 40-100 cm below ground", "3-hourly snow depth"),
#                      "GRIDMET" = c(NA, "Daily Tmax and Tmin", NA, "Daily mean shortwave radiation at surface", "Daily mean wind speed", "Daily precipitation amount", NA, NA, NA),
#                      "NOAA_NCDC" = c(NA, "Daily Tmax and Tmin", NA, NA, NA, "Daily precipitation", NA, NA, "Daily snow Depth"),
#                      "microclimUS" = c("Hourly surface temperature (0% shade)", "Hourly air temperature 2 m above ground", "Hourly soil temperature 1 m below ground (0 % shade)", "Hourly solar radiation (horizontal ground)", NA, NA, "Hourly relative humidity 2 m above ground", "Hourly soil moisture 1 m below ground (0 % shade)", NA),
#                      "microclim" = c("Substrate temperature (soil surface 0 % shade)", "Air temperature 1.2 m above ground", "Soil temperature 1 m below ground", "Solar radiation", "Wind speed 1 cm above ground", NA, "Relative humidity 1.2 m above ground", NA, NA),
#                      "USCRN" = c("Hourly infrared surface temperature", "Hourly air temperature", "Hourly soil temperature 1m belowground", "Average global solar radiation received", NA, NA, "Hourly relative humidity", "Hourly soil moisture 1m belowground", NA),
#                      "NCEP" = c("Land Skin Temperature","Air temperature at 2m","Temperature between 10-200cm below ground level","Clear Sky Downward Solar Flux at surface","Wind speed at 10m","Daily Precipitation Rate at surface","Specific Humidity at 2m","Volumetric Soil Moisture between 10-200cm Below Ground Level",NA),
#                      #"SNODAS" = c(NA, NA, NA, NA, NA, NA, NA, NA, "Snow depth"),
#                      "NicheMapR" = c("Hourly soil temperature at 0cm", "Hourly air temperature 2 m above ground", "Hourly soil temperature 100 cm below ground", "Hourly solar radiation, unshaded", "Hourly wind speed 2 m above ground", NA, "Hourly relative humidity 2 m above ground", NA, "Hourly predicted snow depth"))
# 
# datasets <- colnames(varsDf)
# 
# stations <- fread("CRN_stations.csv", sep = ",") %>% as.data.frame()
# 
# for(mapDataset in c("NCEP")){
#   for(month in c(1,7)){
#     for(mapVar in c("Surface temperature", "Air temperature", "Radiation")){
#       CRN <- grabMapData("USCRN", varsDf[mapVar, "USCRN"], month)
# 
#       if (mapDataset %in% c("GRIDMET", "NOAA_NCDC")) {
#         CRN$Date <- as.Date(CRN$Date)
#         CRN <- aggregate(list(CRN[, c(-1, -2)]), by = list(CRN$Date), mean) %>%
#           set_colnames(c("Date", stations$Name))
#       }
# 
#       inputVar <- varsDf[mapVar, mapDataset]
# 
#       if(!is.na(inputVar)){
#         mapDf <- grabMapData(mapDataset, inputVar, month)
# 
#         stats <- cbind(stations, "Bias" = NA, "RMSE" = NA, "PCC" = NA)
# 
#         for (station in stations$Name) {
#           if(mapDataset == "NOAA_NCDC"){
#             CRN$Date <- format(as.Date(CRN$Date), format = "%Y-%m-%d")
#             mapDf$Date <- format(as.Date(mapDf$Date), format = "%Y-%m-%d")
#           }
#           merged <- merge(CRN[, c("Date", station)], mapDf[, c("Date", station)], by = "Date", all = T) %>%
#             set_colnames(c("Date", "Data1", "Data2")) %>%
#             na.omit()
#           if (nrow(merged) > 7) {
#             bias <- abs((sum(merged$Data1) - sum(merged$Data2)) / nrow(merged))
#             stats[stats$Name == station, "Bias"] <- bias
# 
#             RMSE <- sum((merged$Data1 - merged$Data2)^2) / nrow(merged) # Root mean square error
#             stats[stats$Name == station, "RMSE"] <- RMSE
# 
#             PCC <- cor.test(x = merged$Data1, y = merged$Data2, method = "pearson") # Pearson correlation coefficient
#             stats[stats$Name == station, "PCC"] <- unname(PCC$estimate)
#           } else {
#             stats[stats$Name == station, c("Bias", "RMSE", "PCC")] <- NA
#           }
#         }
# 
#         stats <- stats %>% na.omit()
# 
#         save(stats, file = paste0(mapDataset,"_0",month,"_",inputVar,".Rda"))
# 
#       }
#     }
#   }
# }
# 
# 
