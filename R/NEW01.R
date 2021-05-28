# NEW01

# Variables
# RAINFALL <- CLIMATE[,2:13] : Rainfall (mm/month)
# WNMAXX <- CLIMATE[,26:37]/10 : 10m windspeed max (m/s)
# WNMINN<-WNMAXX*0.1 # impose diurnal cycle : 10m windspeed min (m/s)
# TMINN <- CLIMATE[,38:49]/10 : Maximum temperature (C)
# TMAXX <- CLIMATE[,50:61]/10 : Minimum temperature (C)
# RHMINN <- CLIMATE[,62:73]/10 : Minimum relative humidity (%)
# RHMAXX <- CLIMATE[,74:85]/10 : Maximum relative humidity (%)

grabNEW01 <- function(var, loc, month) {
  load(paste0("Data/NEW01/",loc,".Rda"))
  
  if(var == "RAINFALL")    arr <- CLIMATE[,(25+strtoi(month))]
  if(var == "WNMAXX") arr <- CLIMATE[,(25+strtoi(month))]/10
  if(var == "TMAXX")  arr <- CLIMATE[,(49+strtoi(month))]/10
  if(var == "TMINN")  arr <- CLIMATE[,(37+strtoi(month))]/10
  if(var == "RHMAXX") arr <- CLIMATE[,(73+strtoi(month))]/10
  
  date <- paste0("2017-0", month, "-15")
  
  df <- data.frame("Date" = date, "Hour" = 12, "Data" = arr)
  
  df$Date <- format(as.POSIXct(paste0(df$Date, " ", df$Hour, ":00")), format = "%Y-%m-%d %H:%M")

  return (df)
}


# global_climate=brick("cru/global_climate.nc")
# 
# locs <- data.frame(row.names = c("WA", "CO", "PR", "OR", "HI"), 
#   "lon" = c(-118.5657, -104.7552, -66.98880, -119.65, -155.07), 
#   "lat" = c(47.0022, 40.8066, 18.15110, 44.55, 19.7), 
#   "offset" = c(-8, -7, -4, -7, -10))
#                      
# for(loc in c("OR","CO","HI")){
#   CLIMATE <- raster::extract(global_climate,cbind(locs[loc, "lon"],locs[loc, "lat"]))
#   save(CLIMATE, file = paste0("/Users/ameyer/RShiny_Microclim/Data/NEW01/",loc,".Rda"))
# }


#' build.global.climate = Used to download data
#'
#' Downloads and builds the global climate, elevation and soil moisture datasets used in the function micro_global. The global
#' climate dataset global.nc is generated from "New, M., Lister, D., Hulme, M. and Makin, I., 2002: A high-resolution
#' data set of surface climate over global land areas. Climate Research 21:1-25", specically 10' estimates of monthly
#' mean (over 1961-1990) preceipitation, wet-days, air temperature, diurnal temperature range, relative humidity and
#' wind speed. Cloud cover comes from a bilinear interpolation of a lower resolution version of this dataset (New, M.,
#' M. Hulme, and P. D. Jones. 1999. Representing twentieth century space-time climate variability. Part 1: development
#' of a 1961-90 mean monthly terrestrial climatology. Journal of Climate 12:829-856.). The micro_global function
#' optionaly uses a  global monthly soil moisture estimate from NOAA CPC Soil Moisture http://140.172.38.100/psd/
#' thredds/catalog/Datasets/cpcsoil/catalog.html
#' @param folder Path to the folder you want to install the global climate data in
#' @usage get.global.climate(folder)
#' @export
# build.global.climate <- function(folder="c:/globalclimate/"){
# ANSWER<-readline(prompt = "This function downloads and extracts 0.5 GB of data and takes about 30 mins, type 'y' if you want to continue: ")
# if(substr(ANSWER, 1, 1) == "y"){
#   if(dir.exists(folder)==FALSE){
#     dir.create(folder)
#   }
# 
# if(substring(folder,nchar(folder))!="/"){ # check for forward slash at end of folder name
# folder=paste(folder,"/",sep="")
# }
# 
# library(R.utils)
# library(raster)
# library(ncdf4)
# 
# # global_climate.nc construction
# 
# # global climate files to download:
# # "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_elv.dat.gz" # Elevation
# # "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_pre.dat.gz" # Precipitation
# # "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_rd0.dat.gz" # wet-days
# # "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_tmp.dat.gz" # Mean temperature
# # "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_dtr.dat.gz" # Mean diurnal temperature range
# # "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_reh.dat.gz" # Mean relative humidity
# # "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_wnd.dat.gz" # Mean wind speed
# # "http://ipcc-ddc.cru.uea.ac.uk/download_data/observed/ccld6190.zip" # Mean wind speed
# 
# sourcepath="https://crudata.uea.ac.uk/cru/data/hrg/tmc/"
# destfile1="grid_10min_"
# destfile2=".dat.gz"
# vars=c('elv','pre','rd0','wnd','tmp','dtr','reh')
# 
# # download and unzip all the data, construct rasters
# 
# gridout <- raster(ncol=2160, nrow=1080, xmn=-180, xmx=180, ymn=-90, ymx=90)
# global_climate=stack(replicate(97,gridout))
# cat('downloading and decompressing the data')
# for(i in 1:length(vars)){
#   destin<-paste(folder,destfile1,vars[i],destfile2,sep="")
#   download.file(paste(sourcepath,destfile1,vars[i],destfile2,sep=""), destin, mode="wb")
#   cat(paste(vars[i],destfile2,' downloaded \n',sep=""))
#   R.utils::gunzip(destin)
#   cat(paste(vars[i],destfile2,' decompressed \n',sep=""))
# }
# cat('reading in each variable, rasterising and storing in global_climate stack \n')
# for(i in 1:length(vars)){
#   data<-read.table(paste(folder,destfile1,vars[i],".dat",sep=""))
#   x<-cbind(data[,2],data[,1]) # list of co-ordinates
#   if(i==1){ # elevation, just one colum
#     global_climate[[1]] <- round(rasterize(x, gridout, data[,3])*1000,0) # convert to m, round off
#     cat(paste(vars[i],' done \n',sep=""))
#   }else{
#     for(j in 1:12){
#       if(i==2 | i==3){
#       global_climate[[1+(i-2)*12+j]] <- round(raster::rasterize(x, gridout, data[,2+j]),0)
#       }else{
#        global_climate[[1+(i-2)*12+j]] <- round(raster::rasterize(x, gridout, data[,2+j])*10,0)
#       }
#       cat(paste(vars[i],' month ',j,' done \n',sep=""))
#     }
#   }
# }
# 
# meantemps=global_climate[[38:49]] #save mean temps
# meandtrs=global_climate[[50:61]] #save mean diurnal temp ranges
# meanhums=global_climate[[62:73]] #save mean hums
# 
# # construct max/min temps
# for(i in 1:12){
#   global_climate[[37+i]] = round((meantemps[[i]] - meandtrs[[i]]/2),0)
#   global_climate[[49+i]] = round((meantemps[[i]] + meandtrs[[i]]/2),0)
# }
# 
# # construct max/min relative humidities at new tmin/tmax values from vapour pressure at mean temperature/humidity
# library(NicheMapR) # need functions WETAIR.rh and VAPPRS
# for(i in 1:12){
#   e=WETAIR.rh(rh=meanhums[[i]]/10,db=meantemps[[i]]/10)$e
#   global_climate[[61+i]] = round((e/VAPPRS(global_climate[[49+i]]/10))*100*10,0) # minhum = (e_meantemp/esat_mintemp)*100, but by 10 again to make integer variable
#   global_climate[[73+i]] = round((e/VAPPRS(global_climate[[37+i]]/10))*100*10,0) # maxhum = (e_meantemp/esat_maxtemp)*100, but by 10 again to make integer variable
# }
# # adjust values outside 0-100 range
# for(i in 1:12){
#   values(global_climate[[61+i]]) <- ifelse(values(global_climate[[61+i]])/10 < 0, 0, values(global_climate[[61+i]]))
#   values(global_climate[[61+i]]) <- ifelse(values(global_climate[[61+i]])/10 > 100, 1000, values(global_climate[[61+i]]))
#   values(global_climate[[73+i]]) <- ifelse(values(global_climate[[73+i]])/10 < 0, 0, values(global_climate[[73+i]]))
#   values(global_climate[[73+i]]) <- ifelse(values(global_climate[[73+i]])/10 > 100, 1000, values(global_climate[[73+i]]))
# }
# 
# cat(paste("writing global_climate.nc to ",folder))
# writeRaster(global_climate,paste(folder, "global_climate.nc",sep=""), datatype='INT2S',overwrite=TRUE)
# rm(global_climate)
# 
# files<-list.files(folder)
# file.remove(paste(folder,files[grep(pattern = ".dat",files)],sep=""))


#  } 
#}


