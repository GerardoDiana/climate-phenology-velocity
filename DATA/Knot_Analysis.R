# Diana Gerardo
# Using splm per time period 
# Parallel version

####################################################################################
# SETUP ----------------------------------------------------------------------------
####################################################################################

rm(list = ls())

libs = c('parallel','ggplot2','rgdal', 'Matrix', 'spBayes')
sapply(libs, require, character.only = TRUE)

# Calculate the number of cores. We need 1 free core in order
# to still use the computer while the n_cores are still running
n_cores <- detectCores() - 1

# read in the data. Notice I did not apply the same AEQD projection
# (units = km) on the knots
vel1000 <- read.csv('~/UCSC_capstone/DATA/Vel1000_AEQD_OutRem.csv')
knots <- read.csv('~/UCSC_capstone/DATA/85knots.csv')
head(knots)

# AEQD Projections for the knots
coordinates(knots) <- c("lon", "lat")
proj4string(knots) <- CRS("+init=epsg:4326")
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
aeqd.knots <- spTransform(knots, CRS.new)
knots <- aeqd.knots@coords 

####################################################################################
# spLM: Predictive Process ---------------------------------------------------------
####################################################################################

#Subset the data per time frame
vel1000.t1 <- vel1000[vel1000$t==1,]
ss <- data.frame(lon = vel1000.t1$lon, lat = vel1000.t1$lat)
aeqd <- cbind(x = vel1000.t1$x, y = vel1000.t1$y)
y <-vel1000.t1$V_CVI
V_MAT <- vel1000.t1$V_MAT
X <- as.matrix(V_MAT)

# number of betas
p <- 2 

# Inputs for predictive process
starting <- list("beta" = c(1,1), "sigma.sq" = 40, "tau.sq" = 60, "phi" = .01)
priors <- list("sigma.sq.ig" = c(3,2), "tau.sq.ig" = c(2,2),"phi.Unif" = c(0.001,1),
               "beta.Norm" = list(rep(0,p), diag(1000,p)))
tuning <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)

# Predictive Process
set.seed(8000)
system.time(
  pp <- spLM(y ~ X, coords=aeqd, n.samples = 40000, cov.model = "exponential",
             priors = priors, tuning = tuning, starting = starting, knots = knots,
             modified.pp = F)
)

# 85 knots/5000 iter
# user  system elapsed 
# 73.89    0.00   73.94/60
# 85 knots/40000 iter
# user  system elapsed 
# 596.37    0.10  598.12 

# 108 knots/5000 iter
# user  system elapsed 
# 107.61    0.00  107.71/60 

# 209 knots/5000 iter
#    user  system elapsed 
# 357.06    0.00  357.50/60 
