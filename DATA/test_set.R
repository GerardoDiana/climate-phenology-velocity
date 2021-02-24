# Diana Gerardo
# August 20th, 2019

# get test set
upRuss <- read.csv("~/UCSC_capstone/DATA/Velocity_AEQD_OutRem.csv")
upRuss.loc<- data.frame(lon = upRuss$lon, lat = upRuss$lat)

# data frame of the same 1000 locations per time frame
sub1000 <- read.csv("~/UCSC_capstone/DATA/Vel1000_AEQD_OutRem.csv")
russia.A <- sub1000[sub1000$t==2,] 
ss <- data.frame(lon = russia.A$lon, lat = russia.A$lat)

# obtain test set for a all time periods
sub <- upRuss[which(!(upRuss$lon == ss[1,1] & upRuss$lat == ss[1,2])),]
for(i in 2:length(ss$lon)){
  sub <- sub[which(!(sub$lon == ss[i,1] & sub$lat == ss[i,2])),]
}

dim(sub[sub$t==2,])
#sub %>% as.tibble %>% write_csv('testSet.csv')