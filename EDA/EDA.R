# Diana Gerardo
# Exploratory Data analysis

# clear out global environment
rm(list=ls())

# libraries needed
libs = c('gstat','ggplot2','tidyverse', 'sp' )
sapply(libs,require,character.only=TRUE)

###########################################################################
###########################################################################

velocity <- read.csv("~/UCSC_capstone/DATA/Velocity_AEQD_OutRem.csv")

velocity_t1 <- velocity[velocity$t==1,]

ss <- data.frame(lon = velocity_t1$lon, lat = velocity_t1$lat)
aeqd <- data.frame(x = velocity_t1$x, y = velocity_t1$y)

# Globe visual of 108 knots-------------------------------------------------
knots <- read.csv("~/UCSC_capstone/DATA/108knots.csv")
ggplot(ss, aes(lon, lat)) +
  geom_point(size = .5, alpha=1, col="grey") +
  geom_point(data= knots, colour="black") +
  borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0)) +
  labs(title = "108 Knots", x="lon", y="lat")

# Randomly sample A=1000 number of locations-------------------------------

# since I randomly sampled 1000 location points. I saved them
# so that you can run the same 1000 locations

# A <- 1000
# russia.A <- velocity_t1[sample(nrow(velocity_t1), A), ]
russia.A <- read.csv("~/UCSC_capstone/DATA/1000locations.csv")
russia.ss <- data.frame(lon = russia.A$lon, lat = russia.A$lat)
russia.aeqd <- cbind(x = russia.A$x, y=russia.A$y)

ggplot(ss, aes(lon, lat)) +
  geom_point(size = 1, alpha=1, col="grey") +
  geom_point(data = russia.A, color="black", size = 1)+
  borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0)) +
  labs(title = "Upper Russia: 1000 Location Points", x="lon", y="lat")

setwd('~/UCSC_capstone/DATA')
#russia.A %>% as.tibble %>% write_csv("1000locations.csv")

#clear global enviroment
rm(list=ls())

# save same 1000 locations for all t---------------------------------------
velocity <- read.csv("~/UCSC_capstone/DATA/Velocity_AEQD_OutRem.csv")
russ1 <- read.csv("~/UCSC_capstone/DATA/1000locations.csv")
r1.ss <- data.frame(lon = russ1$lon,lat = russ1$lat) 

sub1000 <- rbind()
for(i in 1:length(r1.ss$lon)){
  sub <- velocity[which(velocity$lon == r1.ss[i,1]
                      & velocity$lat == r1.ss[i,2]),]
  sub1000 <- rbind(sub1000,sub)
}

setwd("~/UCSC_capstone/DATA")
#sub1000 %>% as.tibble %>% write_csv('Vel1000_AEQD_OutRem.csv')

###########################################################################
###########################################################################
# Variograms --------------------------------------------------------------

# clear global environment
rm(list=ls())

# function to plot nice variograms
plot_variogram <- function(v, m, title) {
  preds = variogramLine(m, maxdist = max(v$dist))
  ggplot() + 
    geom_point(data = v, aes(x = dist, y = gamma), size=3, colour="grey26") +
    geom_line(data = preds, aes(x = dist, y = gamma), size=1) +
    ylim(0,max(v$gamma)) +
    labs(x = "distance (km)", y="semivariance", title=title)+
    geom_hline(yintercept= m$psill[1]+m$psill[2], linetype="dashed", colour = "blue", size=1)+
    geom_vline(xintercept= (3*m$range[2]), linetype="dashed", color = "forestgreen", size=1)+
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = m$psill[1]), colour = "red", size=1)+
    geom_segment(aes(x = 0, y = 0, xend = m$range[2], yend = 0), colour = "purple", size=1)
}

# read in data 
vel1000 <- read.csv("~/UCSC_capstone/DATA/Vel1000_AEQD_OutRem.csv")

# create SpatialPointsDataFrame
coordinates(vel1000) <- c("x","y")
class(vel1000)


# Expontential Variograms--------------------------------------------------

# Use Time Periods 
# i in 1:4
# i in 6:10
# i = 12
# i = 16
# i in 18:25
for(i in 25){
  
  # pull time frame i only
  vel <- vel1000[vel1000$t==i,]
  
  # compute pooled variogram
  v <- variogram(object = V_CVI ~ V_MAT, vel)

  # Exponential Variogram
  m <- fit.variogram(v, vgm(30, "Exp", 100, 50))
  print(
    plot_variogram(v,m, paste("Exponential Variogram: Time Period ",i))+
    annotate("text", x = 1000, y = 30, size=3.5, color="forestgreen",
             hjust=0,label = paste("p.range = ", signif(3*m$range[2],6))) +
    annotate("text", x = 1000, y = 35, size=3.5, color="red", hjust=0,
             label = paste("nugget = ", signif(m$psill[1],6)) ) +
    annotate("text", x = 1000, y = 40, size=3.5, color="purple", hjust=0,
             label = paste("range = ", signif(m$range[2],6))) +
    annotate("text", x = 1000, y = 45, size=3.5, color="blue", hjust=0,
             label = paste("sill =", signif(m$psill[1]+m$psill[2],6))) +
    annotate("text", x = 1000, y = 25, size = 3.5, color = "black",
             hjust=0, label = paste("SSErr = ", signif(attr(m, "SSErr"),6)))
  )
  # print(
  #   plot_variogram(v,m, paste("Exponential Variogram: Time Period ",i))+
  #     annotate("text", x = 750, y = 35, size=3.5, color="forestgreen",
  #              hjust=0,label = paste("p.range = ", signif(3*m$range[2],6))) +
  #     annotate("text", x = 750, y = 50, size=3.5, color="red", hjust=0,
  #              label = paste("nugget = ", signif(m$psill[1],6)) ) +
  #     annotate("text", x = 750, y = 65, size=3.5, color="purple", hjust=0,
  #              label = paste("range = ", signif(m$range[2],6))) +
  #     annotate("text", x = 750, y = 80, size=3.5, color="blue", hjust=0,
  #              label = paste("sill =", signif(m$psill[1]+m$psill[2],6))) +
  #     annotate("text", x = 750, y = 20, size = 3.5, color = "black",
  #              hjust=0, label = paste("SSErr = ", signif(attr(m, "SSErr"),6)))
  # )
  #print(signif(attr(m, "SSErr"),6))
  print(signif(m$psill[1],6));print(signif(m$psill[2],6))
  print(signif(3/m$range[2],6))
}

# Time period 5
i=5
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel, cutoff=1000)
m <- fit.variogram(v, vgm(600, "Exp", 100, 80))
signif(attr(m, "SSErr"),6)

# Time period 11
i=11
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(10000, "Exp", 4000, 80))
signif(attr(m, "SSErr"),6)

# Time period 13
i=13
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(800, "Exp", 100, 50))
signif(attr(m, "SSErr"),6)

# Time periods 14 and 15
i=15
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(15000, "Exp", 100, 100))
#signif(attr(m, "SSErr"),6)
print(signif(m$psill[1],6));print(signif(m$psill[2],6))
print(signif(3/m$range[2],6))

# Time period 17
i=17
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(500, "Exp", 100, 50))
signif(attr(m, "SSErr"),6)


################################################################################
################################################################################
# Matern Variograms-------------------------------------------------------------

# read in data 
vel1000 <- read.csv("~/UCSC_capstone/DATA/Vel1000_AEQD_OutRem.csv")

# create SpatialPointsDataFrame
coordinates(vel1000) <- c("x","y")
class(vel1000)

#-----------------------------------------
# Kappa = 1, 1.5, 2.5 -------------------
#-----------------------------------------
# i in 1:4
# i = 6
# i in 8:10  

j = 1
for(i in 21){
  # pull time frame i only
  vel <- vel1000[vel1000$t==i,]
  
  # compute pooled variogram
  v <- variogram(object = V_CVI ~ V_MAT, vel)
  
  # Matern Variogram
  m <- fit.variogram(v, vgm(30, "Mat", 200, 80,kappa=j))
  
  print(
    plot_variogram(v,m, paste("Matern Variogram: Time Period", i, "with kappa =", j))+ 
      annotate("text", x = 1100, y = 30, size=3.5, color="forestgreen", 
               hjust=0,label = paste("p.range = ", signif(3*m$range[2],6))) +
      annotate("text", x = 1100, y = 35, size=3.5, color="red", hjust=0, 
               label = paste("nugget = ", signif(m$psill[1],6)) ) +
      annotate("text", x = 1100, y = 40, size=3.5, color="purple", hjust=0, 
               label = paste("range = ", signif(m$range[2],6))) +
      annotate("text", x = 1100, y = 45, size=3.5, color="blue", hjust=0,
               label = paste("sill =", signif(m$psill[1]+m$psill[2],6))) +
      annotate("text", x = 1100, y = 25, size = 3.5, color = "black",
               hjust=0, label = paste("SSErr = ", signif(attr(m, "SSErr"),6)))
  )
  #print(signif(attr(m, "SSErr"),6))
  print(signif(m$psill[1],6));print(signif(m$psill[2],6))
  print(signif(3/m$range[2],6))
}
  
# Time period 5-----------
j=2.5
i=5
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel, cutoff=1000)
m <- fit.variogram(v, vgm(30, "Mat", 200, 80,kappa=j))
signif(attr(m, "SSErr"),6)

plot_variogram(v,m, paste("Matern Variogram: Time Period", i, "with kappa =", j))+ 
  annotate("text", x = 400, y = 300, size=3.5, color="forestgreen", 
           hjust=0,label = paste("p.range = ", signif(3*m$range[2],6))) +
  annotate("text", x = 400, y = 350, size=3.5, color="red", hjust=0, 
           label = paste("nugget = ", signif(m$psill[1],6)) ) +
  annotate("text", x = 400, y = 400, size=3.5, color="purple", hjust=0, 
           label = paste("range = ", signif(m$range[2],6))) +
  annotate("text", x = 400, y = 450, size=3.5, color="blue", hjust=0,
           label = paste("sill =", signif(m$psill[1]+m$psill[2],6))) +
  annotate("text", x = 400, y = 250, size = 3.5, color = "black",
           hjust=0, label = paste("SSErr = ", signif(attr(m, "SSErr"),6)))

# Time period 7-----------
j=1.5
i=7
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(300, "Mat", 100, 100,kappa=j))
signif(attr(m, "SSErr"),6);signif(m$psill[2],6)

# Time period 11-----------
j=2.5
i=11
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(10000, "Mat", 200, 80,kappa=j))
signif(attr(m, "SSErr"),6)
print(signif(m$psill[1],6));print(signif(m$psill[2],6))
print(signif(3/m$range[2],6))

# Time period 13-----------
j=2.5
i=13
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(800, "Mat", 200, 50,kappa=j))
#signif(attr(m, "SSErr"),6)
print(signif(m$psill[1],6));print(signif(m$psill[2],6))
print(signif(3/m$range[2],6))

# Time period 14 and 15-----------
j=2.5
i=14
vel <- vel1000[vel1000$t==i,]
v <- variogram(object = V_CVI ~ V_MAT, vel)
m <- fit.variogram(v, vgm(15000, "Mat", 100, 100,kappa=j))
#signif(attr(m, "SSErr"),6)
print(signif(m$psill[1],6));print(signif(m$psill[2],6))
print(signif(3/m$range[2],6))
