# NOAA NCDC

# Variables (https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)
# TMAX, TMIN (tenths of degrees C)
# PRCP: Precipitation (tenths of mm)
# SNOW: Snowfall (mm)
# SNWD: Snow depth (mm)

library(rnoaa)
library(magrittr)


# # WA Lind 3 NE GHCND:USC00454679  (47.0022, -118.5657)
# ncdc_stations(extent = c(46.9, -118.7, 47.1, -118.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50)
# 
# 
# # PR MARICAO 2 SSW GHCND:RQC00665908 (18.15110, -66.98880)
# ncdc_stations(extent = c(18.1, -67.1, 18.2, -66.9), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")
# 
# 
# # CO NUNN 7 NNE GHCND:USW00094074 (40.8066, -104.7552)
# ncdc_stations(extent = c(40.1, -105.7, 40.3, -105.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")
# 
# ncdc_stations(extent = c(40.8, -104.8, 40.9, -104.6), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


# WA SPOKANE 17 SSW GHCND:USW00004136 (47.41740, -117.5867)
# ncdc_stations(extent = c(47.4, -117.6, 47.5, -117.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")

# JOHN DAY 0.6 NW, OR US GHCND:USC00354291 (44.4233, -118.9594) not 44.426, -118.957
# ncdc_stations(extent = c(44.423, -118.95, 44.424, -118.96), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")

# HILO INTERNATIONAL AIRPORT 87, HI US GHCND:USW00021504 (19.7191, -155.053)
# ncdc_stations(extent = c(19.71, -155, 19.72, -155.1), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")

#_____________________________________________________________________________________

grabNOAA <- function(var, loc, month) {
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  if (loc == "WA") {
    id = "GHCND:USC00454679"
  } else if (loc == "CO" && var == "SNWD") {
    id = "GHCND:US1COWE0345"
  } else if (loc == "CO") {
    id = "GHCND:USW00094074"
  } else if (loc == "PR") {
    id = "GHCND:RQC00665908"
  } else if (loc == "OR") {
    id = "GHCND:USC00354291"
  } else if (loc == "HI") {
    id = "GHCND:USW00021504"
  }
  
  data <- ncdc(datasetid = 'GHCND', 
               stationid = id, 
               token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
               startdate = paste0("2017-0", month, "-01"), 
               enddate = paste0("2017-0", month, "-31"),
               datatypeid = var,
               add_units = T)
  
  if (var %in% c("TMAX", "TMIN", "PRCP")) {
    data$data[, "value"] <- data$data[, "value"] / 10
  } else if (loc == "PR" || (loc == "CO" && month == 7) ) {
    # Case for summer in CO, and summer/winter in PR
    # wherein there is no snow data. Values set to 0.
    data <- ncdc(datasetid = 'GHCND', 
                 stationid = "GHCND:USC00454679", 
                 token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
                 startdate = paste0("2017-0", month, "-01"), 
                 enddate = paste0("2017-0", month, "-31"),
                 datatypeid = var)
    
    data$data[, "value"] <- 0
  }
  
  df <- data$data[, c("date", "value")] %>% as.data.frame() %>% 
    magrittr::set_colnames(c("Date", "Data"))
  
  return (df)
}


# CO NUNN 7 NNE GHCND:USW00094074 (40.8066, -104.7552)
# list <- ncdc_stations(extent = c(40.7, -104.8, 40.9, -104.7), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


mapNOAA <- function(var, month) {

  stations <- readxl::read_xlsx("SCAN_stations.xlsx") %>% as.data.frame()
  
  # station_data <- ghcnd_stations()
  # fwrite(station_data, "NOAA_stations.csv")
  
  station_data <- fread("NOAA_stations.csv")
  
  colnames(stations)[1] <- "id" # for meteo_nearby_stations, colname has to be "id"
  
  list <- meteo_nearby_stations(lat_lon_df = stations, 
                                station_data = station_data, 
                                lat_colname = "Lat", 
                                lon_colname = "Lon", 
                                limit = 10,
                                var = "TMAX")
  
  merged <- data.frame(date = NA) # need an initial date column to merge later

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
      plotly::select(date, value)
    
    merged <- merge(merged, values, by = "date", all = T)
    colnames(merged)[i + 1] <- station
    
  }
  
  return (merged[-nrow(merged), ]) # the last row is just NAs
}
