# Diana Gerardo
# KNOTS


# Yields 85 knots after filter (where to predict)---------------------------
grid <- as.matrix(expand.grid(seq(75, 135, by=3.35), seq(60, 90, by=3.35)))
# Yields 108 knots after filter (where to predict)--------------------------
grid <- as.matrix(expand.grid(seq(75, 135, by=3), seq(60, 90, by=3)))
# Yields 209 knots after filter (where to predict)--------------------------
grid <- as.matrix(expand.grid(seq(75, 135, by=2.15), seq(60, 90, by=2.15)))

dim(grid)
grid <- data.frame(grid)
names(grid) <- c('lon', 'lat')


# Remove ocean knots

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
    lat = as.vector(path[,2])
  ) %>% filter(between(lon, minlon, maxlon) & 
                 between(lat, minlat, maxlat) ) 
  
  # create points from coordinates; intersect with land in box
  points = data %>% 
    st_as_sf(
      coords = c('lon','lat'), 
      crs = "+proj=longlat +datum=WGS84 +no_defs"
    )
  ol = st_intersection(points, land_in_box)[c('geometry')]
  tab_of_ol = cbind(st_coordinates(ol))
  colnames(tab_of_ol) = c('lon','lat')
  return(tab_of_ol)
}

minlon = 75; maxlon = 135 ; minlat = 60; maxlat = 90
mydata <- get_data(grid, minlat, maxlat, minlon, maxlon)
mydata <- as.data.frame(mydata)
head(mydata)
dim(mydata)

knots <- mydata
coordinates(knots) <- c("lon", "lat")
proj4string(knots) <- CRS("+init=epsg:4326")
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
               +datum=WGS84 +units=km +no_defs")
aeqd.knots <- spTransform(knots, CRS.new)
knots <- aeqd.knots@coords 

#knots %>% as.tibble %>% write_csv('85knots_aeqd.csv')
#knots %>% as.tibble %>% write_csv('108knots_aeqd.csv')
#knots %>% as.tibble %>% write_csv('209knots_aeqd.csv')
