# SNODAS
devtools::install_github("seandavi/GEOquery")

library(GEOquery)

setwd("SNODAS")
snow <- untar("SNODAS_20201201.tar")

zz <- gunzip("us_ssmv01025SlL00T0024TTNATS2020120105DP001.dat.gz", remove = F)

zz <- gzfile("us_ssmv11050lL00T0024TTNATS2020120105DP000.txt.gz")
dat=read.csv(zz,header=F)

zz <- gunzip("us_ssmv11044bS__T0024TTNATS2020120105DP000.dat.gz", remove = F, overwrite = T)


zz <- gzfile("us_ssmv11050lL00T0024TTNATS2020120105DP000.dat.gz")


data <- read.table("us_ssmv01025SlL00T0024TTNATS2020120105DP001.dat")

readLines("us_ssmv11044bS__T0024TTNATS2020120105DP000.dat", n=10)

read.delim("us_ssmv11044bS__T0024TTNATS2020120105DP000.dat")