# Diana Gerardo
# August 8th

# In this Rscript I will be extracting Vc_y and Vp_y variables for
# the upper half of the globe. Then I will using a shape file to get
# the data corresponding to a small region of russia while removing 
# ocean points.

# clear out global environment
rm(list=ls())

# libraries needed 
libs = c('geoR','sf','fields', 'ncdf4','tidyverse', 'raster', 'reshape2',
         'maptools')
sapply(libs, require, character.only = TRUE)


# sources needed
source("~/UCSC_capstone/July28-data/core_functions/functions.R")
source("~/UCSC_capstone/July28-data/core_functions/load masks.R")

######################################################################
######################################################################
# Exracting variables VMAT,VCVI,VLOS,VEOS,VSOS ---------------------
file <- c("~/UCSC_capstone/July28-data/MAT-CVI",
             "~/UCSC_capstone/July28-data/MAT-LOS",
             "~/UCSC_capstone/July28-data/MAT-EOS",
             "~/UCSC_capstone/July28-data/MAT-SOS")

# The VMAT data is the same for each file. So we only need to extract
# this variable once from any of the 4 files.
setwd(file[1])
Vcy <- brick("Vcy.nc") # VMAT data in the nc file
Vcy <- Vcy*covermask0.5*NOSmask0.5*QAmask0.5 
Vc_MAT <- as.data.frame(Vcy, xy=TRUE) 
names(Vc_MAT) <- c("lon","lat",seq(1,25))
Vc_MAT <- melt(Vc_MAT, id=c("lon","lat"))
names(Vc_MAT) <- c("lon","lat","t","V_MAT")
Vc_MAT$t<-as.numeric(Vc_MAT$t)
head(Vc_MAT)
tail(Vc_MAT)

# Here we will extract variables V_CVI, VLOS, VEOS, VSOS
Vpy.data <- list()
Vp <- c("V_CVI", "V_LOS", "V_EOS", "V_SOS") # same order as file

for(i in 1:length(file)){
  setwd(file[i])
  Vpy <- brick("Vpy.nc")
  Vpy <- Vpy*covermask0.5*NOSmask0.5*QAmask0.5 
  Vpy.data[[i]] <- as.data.frame(Vpy, xy=TRUE) 
  names(Vpy.data[[i]]) <- c("lon","lat",seq(1,25))
  Vpy.data[[i]] <- melt(Vpy.data[[i]], id=c("lon","lat"))
  names(Vpy.data[[i]]) <- c("lon","lat","t",Vp[i])
  Vpy.data[[i]]$t<-as.numeric(Vpy.data[[i]]$t)
}

# 16 warnings but they're not serious. We still retrieved the
# data if you take the time to quickly check.
warnings()
i=1
head(Vpy.data[[i]])
tail(Vpy.data[[i]])

#####################################################################
#####################################################################
# Remove NAs -------------------------------------------------------
Vp_CVI <- Vpy.data[[1]]
Vp_LOS <- Vpy.data[[2]]
Vp_EOS <- Vpy.data[[3]]
Vp_SOS <- Vpy.data[[4]]

Velocity_new <- data.frame(Vc_MAT,
                           Vp_CVI$V_CVI,
                           Vp_LOS$V_LOS, 
                           Vp_EOS$V_EOS,
                           Vp_SOS$V_SOS)
head(Velocity_new)
names(Velocity_new) <- c("lon", "lat", "t", "V_MAT", "V_CVI", "V_LOS",
                         "V_EOS", "V_SOS") 
Velocity_new <- na.omit(Velocity_new)
head(Velocity_new)
row.names(Velocity_new) <- c(1:length(Velocity_new$lon))
head(Velocity_new)

########################################################################
########################################################################
# Remove Ocean pts --------------------------------------------------

# We have two choices. One, we can use the built in R packages to remove 
# all ocean pts. Two, we can instead use a shape file to remove all ocean
# pts. Here I remove all ocean points using a shape file it is more up to
# date thus more accurate. I will also only be extracting a small region of 
# Russia.

setwd("~/UCSC_capstone/DATA")

# dissolve world_countries_boundaries to yield landmass
land = read_sf(
  dsn = "UIA_World_Countries_Boundaries", 
  layer = "UIA_World_Countries_Boundaries") %>% 
  mutate(bleh = 'bleh') %>% 
  group_by(bleh) %>% 
  summarise()

# subsets data to geography in question; looking specifically at observations 
# on land.
get_data = function(path, minlat, maxlat, minlon, maxlon){
  # subset land data to area in question
  # build bounding box to subset the landmass data
  box_raw = list( rbind(c(minlon, minlat), c(minlon, maxlat), 
                        c(maxlon, maxlat), c(maxlon, minlat), 
                        c(minlon, minlat) 
                        )
                  )
  
  box_poly = st_polygon(box_raw, dim = 'XY')
  
  box = st_sfc(
    list(box_poly), 
    crs = "+proj=longlat +datum=WGS84 +no_defs") %>% st_sf
  
  # intersect landmass data with bounding box
  land_in_box = st_intersection(land, box)
  
  # open data, pull and filter data to yield points inside bounding box. 
  data = tibble(
    lon = as.vector(path[,1]),
    lat = as.vector(path[,2]),
    t = as.vector(path[,3]),
    V_MAT = as.vector(path[,4]),
    V_CVI = as.vector(path[,5]),
    V_LOS = as.vector(path[,6]),
    V_EOS = as.vector(path[,7]),
    V_SOS = as.vector(path[,8]),
  ) %>% filter(between(lon, minlon, maxlon) & 
                 between(lat, minlat, maxlat) ) 
  
  # create points from coordinates; intersect with land in box
  points = data %>% 
    st_as_sf(
      coords = c('lon','lat'), 
      crs = "+proj=longlat +datum=WGS84 +no_defs"
    )
  ol = st_intersection(points, land_in_box)[c('geometry', 't','V_MAT','V_CVI',
                                              'V_LOS','V_EOS','V_SOS')]
  tab_of_ol = cbind(st_coordinates(ol), ol$t, ol$V_MAT, ol$V_CVI, ol$V_LOS,
                    ol$V_EOS, ol$V_SOS)
  colnames(tab_of_ol) = c('lon','lat', 't','V_MAT','V_CVI',
                          'V_LOS','V_EOS','V_SOS')
  return(tab_of_ol)
}

path <- Velocity_new
minlon = 75; maxlon = 135 ; minlat = 60; maxlat = 90
mydata <- get_data(path, minlat, maxlat, minlon, maxlon)
mydata <- as.data.frame(mydata)
head(mydata)
dim(mydata)
length(which(mydata$t==1))

# Here we will save the data corresponding to a small portion of Russia.
# This excludes all ocean points. However data may still contain outliers
setwd("~/UCSC_capstone/DATA")
#mydata %>% as.tibble %>% write_csv('Velocity_latlon.csv')

##############################################################################
##############################################################################
# AEQD Projection ------------------------------------------------------------

# clear out global environment
rm(list=ls())

# proper spatial analysis requires use the AEQD projection
data <- read.csv("~/UCSC_capstone/DATA/Velocity_latlon.csv")

# project lonlat (degrees) to aeqd (meters)
# Should I center the projection on Russia: +lat_0=65 +lon_0=97
# Should I center it from the North Pole: +lat_0=0 +lon_0=0
ss <- data.frame(lon = data$lon, lat = data$lat)
coordinates(ss) <- c("lon","lat")
proj4string(ss) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
aeqd.s <- spTransform(ss, CRS.new)

# add aeqd column to your existing data Russia where x is aeqd lon 
# and y is aeqd lat
aeqd.ss <- aeqd.s@coords
upRuss <- cbind(aeqd.ss, data)
names(upRuss) <- c('x','y','lon','lat','t','V_MAT','V_CVI',
                   "V_LOS", "V_EOS", "V_SOS")
head(upRuss)
class(upRuss)

# See differences in plots
plot(ss, axes = TRUE, main = "Russia: Lat-Long Coordinates", 
     ylab="Latitude", xlab="Longitude", col="darkgreen")
plot(aeqd.ss, axes = TRUE, main = "Russia: AEQD Conic Projection", 
     col = "darkgreen", ylab="Northing", xlab="Easting")

# save aeqd data
setwd("~/UCSC_capstone/DATA")
#upRuss %>% as.tibble %>% write_csv('Velocity_AEQD.csv')
