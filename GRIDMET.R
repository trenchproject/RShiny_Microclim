# GRIDMET

locs <- data.frame(row.names = c("WA", "PR", "CO"), 
                   "lon" = c(-118.5657, -66.98880, -104.7552), 
                   "lat" = c(47.0022, 18.15110, 40.8066))

library(AOI)
library(climateR)
library(MALDIquant)
library(raster)


valsToArray <- function(loc, AOI, param, month) {
  array <- c()
  p = getGridMET(AOI, param = param, startDate = paste0("2017-0", month, "-01"), endDate = paste0("2017-0", month, "-31"))
  r = raster::brick(p)
  
  for (i in 1:31) {
    df <- rasterToPoints(r[[i]]) %>% as.data.frame()
    x <- sort(df$x)[match.closest(locs[loc, "lon"], sort(df$x))]
    y <- sort(df$y)[match.closest(locs[loc, "lat"], sort(df$y))]
    array <- c(array, df[df$x == x & df$y == y, 3])
  }
  return (array - 273.2)
}

fullArray <- function(loc, AOI, param) {
  full <- c(valsToArray(loc, AOI, param, 1), valsToArray(loc, AOI, param, 7))
  return (full)
}


fullDataFrame <- function(param) {
  AOIWA = aoi_get(state = "WA", county = "adams")
  AOICO = aoi_get(state = "CO", county = "weld")
  
  Jan <- c()
  Jul <- c()
  for (i in 1:31) {
    Jan <- c(Jan, paste0("2017-01-", i))
    Jul <- c(Jul, paste0("2017-07-", i))
  }
  dates <- as.Date(c(Jan, Jul))
  
  df <- data.frame(date = dates, WA = fullArray("WA", AOIWA, param), CO = fullArray("CO", AOICO, param))
  return (df)
}

tmaxGRID <- fullDataFrame("tmax")
tminGRID <- fullDataFrame("tmin")
