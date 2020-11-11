library(rnoaa)
library(magrittr)

# WA Burnt mountain GHCND:USS0021B63S (47.04000, -121.9400)
ncdc_stations(extent = c(46.9, -122.1, 47.1, -121.76), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50)

df <- ncdc(datasetid = 'GHCND', 
     stationid = "GHCND:USS0021B63S", 
     token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
     startdate = paste0("2017-07-01"), 
     enddate = paste0("2017-07-31")
)
unique(df$data$datatype)

# PR MARICAO 2 SSW GHCND:RQC00665908 (18.15110, -66.98880)
ncdc_stations(extent = c(18.1, -67.1, 18.2, -66.9), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")

# CO Wild basin GHCND:USS0005J05S (40.20000, -105.6000)
ncdc_stations(extent = c(40.1, -105.7, 40.3, -105.5), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


#_____________________________________________________________________________________

getData <- function(loc, month, para) {
  if (loc == "WA") {
    id = "GHCND:USS0021B63S"
  } else if (loc == "PR") {
    id = "GHCND:RQC00665908"
  } else if (loc == "CO") {
    id = "GHCND:USS0005J05S"
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
  return (df)
}

fullDf <- function(var) {
  return (getDataFull("WA", var) %>% 
            merge(getDataFull("PR", var), by = c("date", "month"), all = T) %>% 
            merge(getDataFull("CO", var), by = c("date", "month"), all = T))
}


tmax <- fullDf("TMAX")
tmin <- fullDf("TMIN")
 

tmax
Jan <- tmax[tmax$month == 1,]
plot(as.Date(Jan$date), Jan$WA)
