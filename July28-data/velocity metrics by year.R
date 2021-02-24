source("~/Master Project/July28-data/core_functions/functions.R")
source("~/Master Project/July28-data/core_functions/packages needed.R")
source("~/Master Project/July28-data/core_functions/load masks.R")

####Select files
Cvar<-"Tmin"   #change MAT to Tmax or Tmin if needed
file <- paste("~/Master Project/July28-data/TerraClimate/",Cvar,"_0.5degree.nc",sep="")
Cfull <- brick(file)

Pvar<-"SOS"   #change LOS to SOS, EOS, or CVI if needed
file <- paste("~/Master Project/July28-data/Phenology/",Pvar,"_World_0.5deg_QA.nc",sep="")
Pfull <- brick(file)

####Calculate velocity metrics for every time period (run this only once)
for (n in 1990:2014) {
  V<-velocitiesatYear(n,10)
  writeRaster(V, paste("~/Master Project/July28-data/",Cvar,"-",Pvar,"/V_",n-10+1,"-",n,sep=""), overwrite=TRUE, format="CDF", varname="V", varunit="", longname="Velocity metrics", xname="lon",   yname="lat",zname="layer")
}

####Retrieve velocity metrics 
Cvar<-"Tmin"   #change MAT to Tmax or Tmin if needed
Pvar<-"SOS"   #change LOS to SOS, EOS, or CVI if needed

files<-list.files(paste("~/Master Project/July28-data/",Cvar,"-",Pvar,sep=""),pattern = "V_",full.names = T)
layer<-vector("list",length = length(files))
metric_list<-c("Vc","Vcx","Vcy","Vp","Vpx","Vpy","projection","mismatch","anglediff")
for (i in 1:length(metric_list)) {
  metric<-metric_list[[i]]
  for (n in 1:25) {
    if (metric=="Vc") layer[[n]] <- brick(files[[n]])[[3]]
    if (metric=="Vcx") layer[[n]] <- brick(files[[n]])[[5]]
    if (metric=="Vcy") layer[[n]] <- brick(files[[n]])[[6]]
    if (metric=="Vp") layer[[n]] <- brick(files[[n]])[[9]]
    if (metric=="Vpx") layer[[n]] <- brick(files[[n]])[[11]]
    if (metric=="Vpy") layer[[n]] <- brick(files[[n]])[[12]]
    if (metric=="projection") layer[[n]] <- brick(files[[n]])[[13]]
    if (metric=="mismatch") layer[[n]] <- brick(files[[n]])[[14]]
    if (metric=="anglediff") layer[[n]] <- brick(files[[n]])[[15]]
  }
  stacked<-stack(layer)
  writeRaster(stacked, paste("~/Master Project/July28-data/",Cvar,"-",Pvar,"/",metric,sep=""), overwrite=TRUE, format="CDF",     varname=metric, varunit="", longname="", xname="lon",   yname="lat",zname="time")
}

###Plot and check
bri<-brick("./Data/Acceleration/MAT-EOS/Vc.nc")
plot(crop_remove_outlier(bri[[1]],extent(bri[[1]])))
