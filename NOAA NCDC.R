# NOAA NCDC

# Variables (https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)
# TMAX, TMIN (tenths of degrees C)
# PRCP: Precipitation (tenths of mm)
# SNOW: Snowfall (mm)
# SNWD: Snow depth (mm)


# Function: fullDf("var")




library(rnoaa)
library(magrittr)



locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                   "lon" = c(-118.5657, -66.98880, -104.7552), 
                   "lat" = c(47.0022, 18.15110, 40.8066))


# WA Lind 3 NE GHCND:USC00454679  (47.0022, -118.5657)


ncdc_stations(extent = c(46.9, -118.7, 47.1, -118.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50)

ncdc(datasetid = 'GHCND', 
     stationid = "GHCND:USC00454679", 
     token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
     startdate = paste0("2017-01-01"), 
     enddate = paste0("2017-01-31")
)

datasets <- ncdc_datasets(datasetid = 'GHCND', 
              stationid = "GHCND:USC00454679", 
              token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
              startdate = paste0("2017-07-01"), 
              enddate = paste0("2017-07-31")
              )
unique(df$data$datatype)

# PR MARICAO 2 SSW GHCND:RQC00665908 (18.15110, -66.98880)
ncdc_stations(extent = c(18.1, -67.1, 18.2, -66.9), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


ncdc(datasetid = 'GHCND', 
     stationid = "GHCND:RQC00665908", 
     token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
     startdate = paste0("2017-07-01"), 
     enddate = paste0("2017-07-31"),
     datatypeid = "PRCP"
)

# CO NUNN 7 NNE GHCND:USW00094074 (40.8066, -104.7552)
ncdc_stations(extent = c(40.1, -105.7, 40.3, -105.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


ncdc_stations(extent = c(40.8, -104.8, 40.9, -104.6), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


ncdc(datasetid = 'GHCND', 
     stationid = "GHCND:USW00094074", 
     token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
     startdate = paste0("2017-07-01"), 
     enddate = paste0("2017-07-31")
)

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
  data$data[, "value"] <- data$data[, "value"] / 10
  df <- data$data[,c("date", "value")] %>% as.data.frame()
  colnames(df)[1] <- "Date"
  df$Month <- month
  return (df)
}

getDataFull <- function(loc, para) {
  df <- rbind(getData(loc, 1, para), getData(loc, 7, para))
  colnames(df)[2] <- loc
  #df$date <- format(as.Date(df$date), format = "%Y-%m-%d")
  return (df)
}

fullDf <- function(var) {
  return (getDataFull("WA", var) %>% 
            merge(getDataFull("PR", var), by = c("Date", "Month"), all = T) %>% 
            merge(getDataFull("CO", var), by = c("Date", "Month"), all = T))
}


tmaxNOAA <- fullDf("TMAX")
tminNOAA <- fullDf("TMIN")

