# # Download micro_function data

# locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
#                    "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
#                    "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
#                    "offset" = c(-8, -7, -4, -8, -10))

# for (loc in c("OR", "CO", "HI")) {
#   for (month in c(1, 7)) {
#     lonlat <- c(locs[loc, "lon"], locs[loc, "lat"]) # (longitude, latitude)
#     dstart <- paste0("01/0", month, "/2017") # start date
#     dfinish <- paste0("31/0", month, "/2017") # end date
    
#     DEP <- c(0, 3, 5, 10, 15, 20, 30, 50, 100, 200) # specify depths. need 10
    
#     # micro_global()
#     micro <- micro_global(loc = lonlat, dstart = dstart, dfinish = dfinish, DEP = DEP,
#                        runmoist = 0, runshade = 0, Usrhyt = 0.01)
#     filename <- paste0("micro_global_", loc, "_", month, ".RData")
#     save(micro, file = filename)
    
#     # micro_USA()
#     micro <- micro_USA(loc = lonlat, dstart = dstart, dfinish = dfinish, DEP = DEP,
#                        runmoist = 0, runshade = 0, Usrhyt = 0.01)
#     filename <- paste0("micro_usa_", loc, "_", month, ".RData")
#     save(micro, file = filename)
    
#     # micro_era5()
#     micro <- micro_era5(loc = lonlat, dstart = dstart, dfinish = dfinish, DEP = DEP,
#                         runmoist = 0, runshade = 0, Usrhyt = 0.01)
#     filename <- paste0("micro_era5_", loc, "_", month, ".RData")
#     save(micro, file = filename)
    
#     # micro_ncep()
#     micro <- micro_ncep(loc = lo4nlat, dstart = dstart, dfinish = dfinish, DEP = DEP,
#                         runmoist = 0, runshade = 0, Usrhyt = 0.01)
#     filename <- paste0("micro_ncep_", loc, "_", month, ".RData")
#     save(micro, file = filename)
#   }
# }