# SNODAS

# Variables
# "POSIXct" = Date
# "SNEQV"   = Snow Water Equivalent (mm)
# "SNOWH"   = Snow Depth (mm)

# Import
library(readr)

grabSNODAS <- function(var, loc, month) {
  
  if(month==1) m = "January"
  else         m = "July"
  
  import = read_csv(paste0("Data/SNODAS/snodas",m,loc,".csv"))
  
  df <- data.frame(Date = import$POSIXct, 
                   Data = import$SNOWH)
  return (df)
}