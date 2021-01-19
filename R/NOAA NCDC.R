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


# CO NUNN 7 NNE GHCND:USW00094074 (40.8066, -104.7552)
# ncdc_stations(extent = c(40.7, -104.8, 40.9, -104.7), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")


# TX PANTHER JUNCTION GHCND:USC00416792 (29.3, -103.2)
# ncdc_stations(extent = c(29.3, -103.3, 29.4, -103.1), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 50, datasetid = "GHCND")



#_____________________________________________________________________________________

grabNOAA <- function(var, loc, month) {
  
  days <- c()
  for (i in 1:31) {
    days <- c(days, paste0("2017-0", month, "-", i))
  }
  
  if (loc == "WA") {
    id = "GHCND:USC00454679"
  } else if (loc == "CO") {
    id = "GHCND:USW00094074"
  } else if (loc == "TX") {
    id = "GHCND:USC00416792"
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
