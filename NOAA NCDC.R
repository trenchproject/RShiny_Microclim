# NOAA NCDC

# Variables (https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)
# TMAX, TMIN (tenths of degrees C)
# PRCP: Precipitation (tenths of mm)
# SNOW: Snowfall (mm)
# SNWD: Snow depth (mm)


# Function: fullNOAA("var")

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



#_____________________________________________________________________________________

getData <- function(loc, month, para) {
  if (loc == "WA") {
    id = "GHCND:USC00454679"
  } else if (loc == "PR") {
    id = "GHCND:RQC00665908"
  } else if (loc == "CO") {
    id = "GHCND:USW00094074"
  }
  data <- ncdc(datasetid = 'GHCND', 
             stationid = id, 
             token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
             startdate = paste0("2017-0", month, "-01"), 
             enddate = paste0("2017-0", month, "-31"),
             datatypeid = para)
  if (para %in% c("TMAX", "TMIN")) {
    data$data[, "value"] <- data$data[, "value"] / 10
  }
  df <- data$data[, c("date", "value")] %>% as.data.frame()
  colnames(df)[1] <- "Date"
  df$Month <- month
  return (df)
}

getDataNOAA <- function(loc, para) {
  df <- rbind(getData(loc, 1, para), getData(loc, 7, para))
  colnames(df)[2] <- loc
  #df$date <- format(as.Date(df$date), format = "%Y-%m-%d")
  return (df)
}

fullNOAA <- function(var) {
  return (getDataNOAA("WA", var) %>% 
            merge(getDataNOAA("PR", var), by = c("Date", "Month"), all = T) %>% 
            merge(getDataNOAA("CO", var), by = c("Date", "Month"), all = T))
}




grabNOAA <- function(var, loc, month) {
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  if (loc == "WA") {
    id = "GHCND:USC00454679"
  } else if (loc == "PR") {
    id = "GHCND:RQC00665908"
  } else if (loc == "CO") {
    id = "GHCND:USW00094074"
  }
  data <- ncdc(datasetid = 'GHCND', 
               stationid = id, 
               token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
               startdate = paste0("2017-0", month, "-01"), 
               enddate = paste0("2017-0", month, "-31"),
               datatypeid = var)
  
  if (var %in% c("TMAX", "TMIN")) {
    data$data[, "value"] <- data$data[, "value"] / 10
  }
  
  df <- data$data[, c("date", "value")] %>% as.data.frame() %>% magrittr::set_colnames(c("Date", "Data"))
  
  return (df)
}
