#0.5 deg
file <- c("~/Master Project/July28-data/masks/Masks_0.5deg.nc")#World
masks0.5 <- brick(file)
covermask0.5<-masks0.5[[1]]
QAmask0.5<-masks0.5[[2]]
NOSmask0.5<-masks0.5[[3]]

#0.05 deg
file <- c("~/Master Project/July28-data/masks/Masks_0.05deg.nc")#World
masks0.05 <- brick(file)
covermask0.05<-masks0.05[[1]]
QAmask0.05<-masks0.05[[2]]
NOSmask0.05<-masks0.05[[3]]