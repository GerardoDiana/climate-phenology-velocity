# Diana Gerardo
# August 1st
# New data (July 28 data)
# Vc: MAT for all


# libraries needed 
library(ncdf4)
library(raster)
library(maptools)
library(sp)

# sources needed
source("~/Master Project/July28-data/core_functions/functions.R")
source("~/Master Project/July28-data/core_functions/load masks.R")

# folder MAT-CVI
setwd("~/Master Project/July28-data/MAT-CVI")

# Vc: velocity of climate, which is V_MAT in magnitude
# X1,....,X25 are the time frames
# x,y: lon and lat
Vc <- brick("Vc.nc") # raster form
Vc.data <- as.data.frame(Vc, xy=TRUE) # data frame 
head(Vc.data)
dim(Vc.data)
 

# plot(Vc[[1]]) # outliers have not been removed here
# plot(crop_remove_outlier(Vc[[1]],extent(-180,180,30,70)))
# by using crop_remove_outlier funcion we remove any extreme points
# we will not save data with "removed outliers" yet
# we will like to see which and where these outliers are if possible


# the current data does not have the masks applied yet
# data currently contains ocean points
# and we must change extent to desired region

# we start again in raster form
Vc <- Vc*covermask0.5*NOSmask0.5*QAmask0.5 # masks are now applied
Vc.data <- as.data.frame(Vc, xy=TRUE) # data frame
head(Vc.data)
names(Vc.data) <- c("lon","lat",seq(1,25))
Vc.data <- melt(Vc.data, id=c("lon","lat"))
names(Vc.data) <- c("lon","lat","t","V_MAT")
Vc.data$t<-as.numeric(Vc.data$t)
head(Vc.data)
# Vc.data %>% as.tibble %>% write_csv('Vc_MAT.csv')


# We will now do the same thing for Vp
# Vp: velocity of phenology, V_CVI in magnitude

Vp <- brick("Vp.nc") # raster form
Vp.data <- as.data.frame(Vp, xy=TRUE) # data frame 


Vp <- Vp*covermask0.5*NOSmask0.5*QAmask0.5 # masks are now applied
Vp.data <- as.data.frame(Vp, xy=TRUE) # data frame
head(Vp.data)
names(Vp.data) <- c("lon","lat",seq(1,25))
Vp.data <- melt(Vp.data, id=c("lon","lat"))
names(Vp.data) <- c("lon","lat","t","V_CVI")
Vp.data$t<-as.numeric(Vp.data$t)
head(Vp.data)
#Vp.data %>% as.tibble %>% write_csv('Vp_CVI.csv')


# ----------------------------------------------------------------------
setwd("~/Master Project/July28-data/MAT-LOS")

Vp <- brick("Vp.nc") 
Vp <- Vp*covermask0.5*NOSmask0.5*QAmask0.5 
Vp.data <- as.data.frame(Vp, xy=TRUE) 
head(Vp.data)
names(Vp.data) <- c("lon","lat",seq(1,25))
Vp.data <- melt(Vp.data, id=c("lon","lat"))
names(Vp.data) <- c("lon","lat","t","V_LOS")
Vp.data$t<-as.numeric(Vp.data$t)
head(Vp.data)
#Vp.data %>% as.tibble %>% write_csv('Vp_LOS.csv')

# ----------------------------------------------------------------------
setwd("~/Master Project/July28-data/MAT-EOS")

Vp <- brick("Vp.nc") 
Vp <- Vp*covermask0.5*NOSmask0.5*QAmask0.5 
Vp.data <- as.data.frame(Vp, xy=TRUE) 
head(Vp.data)
names(Vp.data) <- c("lon","lat",seq(1,25))
Vp.data <- melt(Vp.data, id=c("lon","lat"))
names(Vp.data) <- c("lon","lat","t","V_EOS")
Vp.data$t<-as.numeric(Vp.data$t)
head(Vp.data)
#Vp.data %>% as.tibble %>% write_csv('Vp_EOS.csv')

# ----------------------------------------------------------------------
setwd("~/Master Project/July28-data/MAT-SOS")

Vp <- brick("Vp.nc") 
Vp <- Vp*covermask0.5*NOSmask0.5*QAmask0.5 
Vp.data <- as.data.frame(Vp, xy=TRUE) 
head(Vp.data)
names(Vp.data) <- c("lon","lat",seq(1,25))
Vp.data <- melt(Vp.data, id=c("lon","lat"))
names(Vp.data) <- c("lon","lat","t","V_SOS")
Vp.data$t<-as.numeric(Vp.data$t)
head(Vp.data)
#Vp.data %>% as.tibble %>% write_csv('Vp_SOS.csv')


# ----------------------------------------------------------------------

Vc_MAT <- read.csv("~/Master Project/July28-data/MAT-CVI/Vc_MAT.csv")
Vp_CVI <- read.csv("~/Master Project/July28-data/MAT-CVI/Vp_CVI.csv")
Vp_LOS <- read.csv("~/Master Project/July28-data/MAT-LOS/Vp_LOS.csv")
Vp_EOS <- read.csv("~/Master Project/July28-data/MAT-EOS/Vp_EOS.csv")
Vp_SOS <- read.csv("~/Master Project/July28-data/MAT-SOS/Vp_SOS.csv")

Velocity_new <- data.frame(Vc_MAT, Vp_CVI$V_CVI, Vp_LOS$V_LOS,
                           Vp_EOS$V_EOS, Vp_SOS$V_SOS)
head(Velocity_new)
names(Velocity_new) <- c("lon", "lat", "t", "V_MAT", "V_CVI", "V_LOS",
                         "V_EOS", "V_SOS") 
Velocity_new <- na.omit(Velocity_new)



# ----------------------------------------------------------------------
# here we will now remove ocean points

# subset my region
extent = Velocity_new[Velocity_new$lon >= -180 & Velocity_new$lon <= 180 &
                        Velocity_new$lat >= 25 & Velocity_new$lat <= 90, ]

## Find which points fall over land
data(wrld_simpl)
pts <- SpatialPoints(extent, proj4string=CRS(proj4string(wrld_simpl)))
vel_new <- pts[!is.na(over(pts, wrld_simpl)$FIPS)]@coords

setwd("~/Master Project/July28-data")
#vel_new %>% as.tibble %>% write_csv('July28_data.csv')






