# Diana Gerardo

# clear out global environment
rm(list=ls())

# I have just retrieved 'Velocity_AEQD.csv' using the Rscripts 'MATy_RussiaData'.
upRuss <- read.csv("~/UCSC_capstone/DATA/Velocity_AEQD.csv")

# There are 25 times frames (t) and 3276 locations. Here we analyze V_MAT and 
# v_CVI at t=1.
data_t1 <- upRuss[upRuss$t==1,]
ss <- data.frame(lon=data_t1$lon, lat=data_t1$lat) #latlon coords

# creating and adding a location index to upRuss
s <- c()
for(i in 1:length(ss$lon)){ ind <- rep(i,25); s <- c(s,ind) }
upRuss.copy <- data.frame(s, upRuss)

# Checking the summary statistic with VCVI as the response and VMAT as a 
# predictor variable at t=1. We see that V_MAT is not significant which 
# is strange.
lm1 <- lm(data_t1$V_CVI ~ data_t1$V_MAT)
summary(lm1)

# V_MAT : velocity of mean annual temperature
# V_CVI : velocity of cumulative vegitation index
V_MAT <- data_t1$V_MAT
V_CVI <- data_t1$V_CVI

# density plots of the predictor and response variable, respectively
# Highly concentrated near zero with long tails on both sides
plot(density(V_MAT), main = "Density of V_MAT with outliers")
plot(density(V_CVI), main = "Density of V_CVI with outliers")

# Global visual plot of the observed data at t=1. We can see a slight 
# spatial pattern of V_CVI. However, we cannot see a spatial pattern
# of V_MAT, it is mostly purple. But recall from our density plots that
# that there were many outliers extending as far as -100 and 100 when a
# majority of points are near 0.
ggplot(ss, aes(lon, lat)) +
  geom_point(size = 1, alpha=1) +
  borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
  geom_point(aes(colour=V_CVI), size = 1, alpha=0.8) +
  scale_colour_viridis_c(option="inferno")+
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0)) +
  labs(title = "Russia: V_CVI at t = 1", x="lon", y="lat", col="V_CVI") 
ggplot(ss, aes(lon, lat)) +
  geom_point(size = 1, alpha=1) +
  borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
  geom_point(aes(colour=V_MAT), size = 1, alpha=0.8) +
  scale_colour_viridis_c(option="inferno")+
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0)) +
  labs(title = "Russia: V_MAT at t = 1", x="lon", y="lat", col="V_MAT")

#############################################################################
#############################################################################
# Removing Outliers----------------------------------------------------------

# A=73, t=23, 
# d[-c(78,201,206,886,1074,1467,1653,1771,1772,1901,2134,2338,2438,2439,2849),]

A <- 73
d <- upRuss.copy[upRuss.copy$t==23,] 
d <- d[d$V_MAT <= A & d$V_MAT >= -A, ]
s1 <- data.frame(lon=d$lon, lat=d$lat)

dim(d)
# qqnorm(d$V_CVI)
# qqline(d$V_CVI, col="red")
# #identify(qqnorm(d$V_CVI))
# #d <- d[-c(885,948,1046,1185,1489,1826,1959),]
# #[1]  885  948 1046 1185 1489 1826 1959
# 
# qqnorm(d$V_CVI)
# qqline(d$V_CVI, col="red")
# #identify(qqnorm(d$V_CVI))
# d <- d[-c(78,201,206,886,1074,1467,1653,1771,1772,1901,2134,2338,2438,2439,2849),]
# 
# qqnorm(d$V_CVI)
# qqline(d$V_CVI, col="red")
# dim(d)
# summary(lm(d$V_CVI~d$V_MAT))
# 
# s1 <- data.frame(lon=d$lon, lat=d$lat)

ggplot(s1, aes(lon, lat)) +
  geom_point(size = 1, alpha=1) +
  borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
  geom_point(aes(colour=d$V_CVI), size = 1, alpha=0.8) +
  scale_colour_viridis_c(option="inferno")+
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0)) +
  labs(title = "Russia: V_CVI at t = 23", x="lon", y="lat", col="V_CVI")
ggplot(s1, aes(lon, lat)) +
  geom_point(size = 1, alpha=1) +
  borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
  geom_point(aes(colour=d$V_MAT), size = 1, alpha=0.8) +
  scale_colour_viridis_c(option="inferno")+
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0)) +
  labs(title = "Russia: V_MAT at t = 23", x="lon", y="lat", col="V_MAT")


# saving russia with most outliers removed
upRuss_no <- rbind()
for(i in 1:length(s1$lon)){
  sub <- upRuss[which(upRuss$lon == s1[i,1]
                      & upRuss$lat == s1[i,2]),]
  upRuss_no <- rbind(upRuss_no,sub)
}

row.names(upRuss_no) <- c(1:length(upRuss_no$lon))

# Since I was also working in the console to remove outliers,
# I saved the data labeled "Velocity_AEQD_OutRem.csv" 
# so that way you can run the same data

# clear global environment
rm(list=ls())

setwd('~/UCSC_capstone/DATA')
#upRuss_no %>% as.tibble %>% write_csv('Velocity_AEQD_OutRem.csv')
