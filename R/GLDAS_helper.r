# valGLDAS <- function(nc, var, loc) {
#   
#   locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
#                      "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
#                      "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
#                      "offset" = c(-8, -7, -4, -8, -10))
#   
#   ncvar <- ncvar_get(nc, varid = var)
#   
#   lonInd <- match.closest(locs[loc, "lon"], nc$dim$lon$vals)
#   lat <- sort(nc$dim$lat$vals)[match.closest(locs[loc, "lat"], sort(nc$dim$lat$vals))]
#   latInd <- match(lat, nc$dim$lat$vals)
#   
#   i = 1
#   while(loc != "PR" &&is.na(ncvar[lonInd, latInd])){
#     if(!is.na(ncvar[lonInd+i, latInd+i])) {
#       lonInd = lonInd+i;
#       latInd = latInd+i;
#       break
#     }
#     if(!is.na(ncvar[lonInd, latInd+i]))  {
#       latInd = latInd+i;
#       break
#     }
#     if(!is.na(ncvar[lonInd+i, latInd]))  {
#       lonInd = lonInd+i;
#       break
#     }
#     if(!is.na(ncvar[lonInd-i, latInd-i]) && latInd-i!=0 && lonInd-i!=0)  {
#       lonInd = lonInd-i;
#       latInd = latInd-i;
#       break
#     }
#     if(!is.na(ncvar[lonInd-i, latInd]) && lonInd-i!=0)  {
#       lonInd = lonInd-i;
#       break
#     }
#     if(!is.na(ncvar[lonInd, latInd-i]) && latInd-i!=0)  {
#       latInd = latInd-i;
#       break
#     }
#     i = i + 1
#   }
#   
#   val <- ncvar[lonInd, latInd]
#   
#   if (var %in% c("AvgSurfT_inst", "Tair_f_inst", "SoilTMP40_100cm_inst")) { # K to C
#     val <- val - 273.15
#   } else if (var == "Rainf_f_tavg") {  # kg/m^2 s to mm
#     val <- val * 60 * 60 * 3
#   } else if (var == "SnowDepth_inst") { # m to mm
#     val <- val * 1000
#   } else if (var == "SoilMoi40_100cm_inst") { # kg/m^3 to % (Soil density =~ 1.6 g/cm^3 = 1600 kg/m^3. The measurement is over 60 cm)
#     val <- 1600 / val / 0.6
#   }
#   
#   return (val)
# }
# 
# 
# grabGLDAS <- function(var, loc, month) {
#   
#   locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
#                      "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
#                      "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
#                      "offset" = c(-8, -7, -4, -8, -10))
#   
#   days <- c()
#   for (i in 1:31) {
#     days <- c(days, paste0("2017-0", month, "-", i))
#   }
#   
#   
#   array <- c()
#   for (day in 1:31) {
#     for (hour in seq(from = 0, to = 21, by = 3)) {
#       
#       char_day <- ifelse(day < 10, paste0("0", day), day)
#       char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
#       
#       filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
#       
#       nc <- nc_open(filename)
#       val <- valGLDAS(nc, var, loc)
#       array <- c(array, val)
#     }
#   }
#   
#   if (var == "Qair_f_inst") {  # Convert specific humidity to relative humidity
#     arrayTemp <- c()
#     for (day in 1:31) {
#       for (hour in seq(from = 0, to = 21, by = 3)) {
#         
#         char_day <- ifelse(day < 10, paste0("0", day), day)
#         char_hour <- ifelse(hour < 10, paste0("0", hour), hour)
#         
#         filename <- paste0("Data/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
#         
#         filename <- paste0("~/GLDAS_", month, "/GLDAS_NOAH025_3H.A20170", month, char_day, ".", char_hour, "00.021.nc4.SUB.nc4")
#         nc <- nc_open(filename)
#         val <- valGLDAS(nc, "Tair_f_inst", loc)
#         arrayTemp <- c(arrayTemp, val)
#       }
#     }
#     array <- SH2RH(q = array, t = arrayTemp, isK = FALSE)
#     array[array > 100] <- 100 # Maximum humidity is 100%
#   }
#   
#   offset <- -locs[loc, "offset"] # Data are stored as UCT. So we need adjustment to be aligned to the local time.
#   
#   roundUp <- ceiling(offset / 3)
#   
#   df <- data.frame(Date = rep(days, each = 8), 
#                    Hour = seq(from = roundUp * 3 - offset, to = 21 + (roundUp * 3 - offset), by = 3))
#   
#   df <- cbind(df[1 : (31 * 8 - roundUp), ], "Data" = array[(1 + roundUp) : length(array)])
#   
#   df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")
#   
#   save(df, file = paste0(var,"_",loc,"_0",month,".Rda"))
# }
# 
# for(var in c("Qair_f_inst","Rainf_f_tavg","SoilMoi40_100cm_inst","SoilMoi100_200cm_inst")){
#   df <- grabGLDAS(var,"WA",1)
#   
#   df <- grabGLDAS(var,"WA",7)
#   
#   df <- grabGLDAS(var,"OR",1)
#   
#   df <- grabGLDAS(var,"OR",7)
#   
#   df <- grabGLDAS(var,"CO",1)
#   
#   df <- grabGLDAS(var,"CO",7)
#   
#   df <- grabGLDAS(var,"HI",1)
#   
#   df <- grabGLDAS(var,"HI",7)
#   
#   df <- grabGLDAS(var,"PR",1)
#   
#   df <- grabGLDAS(var,"PR",7)
# }
# 
# 
# 
# 
