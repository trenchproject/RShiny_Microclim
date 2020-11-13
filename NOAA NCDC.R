# NOAA NCDC

library(rnoaa)
library(magrittr)


locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                   "lon" = c(-118.5657, -66.98880, -104.7552), 
                   "lat" = c(47.0022, 18.15110, 40.8066))


# WA Lind 3 NE GHCND:USC00454679  (47.0022, -118.5657)


ncdc_stations(extent = c(46.9, -118.7, 47.1, -118.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50)

df <- ncdc(datasetid = 'GHCND', 
     stationid = "GHCND:USC00454679", 
     token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
     startdate = paste0("2017-07-01"), 
     enddate = paste0("2017-07-31")
)
unique(df$data$datatype)

# PR MARICAO 2 SSW GHCND:RQC00665908 (18.15110, -66.98880)
ncdc_stations(extent = c(18.1, -67.1, 18.2, -66.9), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


# CO NUNN 7 NNE GHCND:USW00094074 (40.8066, -104.7552)
ncdc_stations(extent = c(40.1, -105.7, 40.3, -105.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


ncdc_stations(extent = c(40.8, -104.8, 40.9, -104.6), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


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
  data$data[, 4] <- data$data[, 4] / 10
  df <- data$data[,c(1,4)]
  df$month <- month
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
            merge(getDataFull("PR", var), by = c("date", "month"), all = T) %>% 
            merge(getDataFull("CO", var), by = c("date", "month"), all = T))
}


tmaxNOAA <- fullDf("TMAX")
tminNOAA <- fullDf("TMIN")

Jan <- tmax[tmax$month == 1,]
plot(as.Date(Jan$date), Jan$WA)
