
#############################function for testing significance
calcslopemask<-function(rx, divisor = 1, na.rm = TRUE, year=year, sigmask=TRUE, sigt = null ){
    icell <- 1:ncell(rx)
	lonlat <- xyFromCell(rx, icell)
    y <- t(getValues(rx))
    x <- row(y)
    #x <- row(x)
    x <- x/divisor
    x1 <- y
    x1[is.na(x1) == F] <- 1
    N <- apply(x1, 2, sum, na.rm = na.rm)
    x <- x * x1
    rm(x1)
    xy <- x*y
    sxy <- apply(xy, 2, sum, na.rm = na.rm)
    rm(xy)
    x2 <- x*x
    sx2 <- apply(x2, 2, sum, na.rm = na.rm)
    rm(x2)
    sx <- apply(x, 2, sum, na.rm = na.rm)
    sy <- apply(y, 2, sum, na.rm = na.rm)
    slope <- (sxy-(sx*sy/N))/(sx2-((sx^2)/N))
    intercept <- (sy*sx2-sx*sxy)/(N*sx2-(sx)^2)
    rep.row<-function(x,n){matrix(rep(x,each=n),nrow=n)}
    slopemat<-rep.row(slope,year)
    interceptmat<-rep.row(intercept,year)
    fit <- slopemat*x+interceptmat
    yerr <- (y-fit)^2
    yerrsum <- apply(yerr, 2, sum, na.rm = na.rm)
    xmean <- apply(x, 2, mean, na.rm = na.rm)
    xmeanmat <- rep.row(xmean,year)
    xerr<- (x-xmeanmat)^2
    xerrsum<-apply(xerr, 2, sum, na.rm = na.rm)
    se <- sqrt(yerrsum/(N-2))/sqrt(xerrsum)
    tstat<-slope/se
    tstat[abs(tstat)<sigt] <- 0
    tstat[abs(tstat)>sigt] <- 1
    if(sigmask==TRUE){slope<-slope*tstat}
    data.frame(slope=slope, sig=tstat, N = N, lonlat, icell)
	}

######new calcvelocity (with corrected angle and XY decomposition)
calcvelocityXY <- function(grad, slope, y_dist = 111.325){
  slope$w <- y_dist * cos(CircStats::rad(slope$y))
  grd <- data.frame(NSold = grad$NS, WEold = grad$WE)
  grd$NS <- ifelse(is.na(grd$NSold) == TRUE, 0, grd$NSold)
  grd$WE <- ifelse(is.na(grd$WEold) == TRUE, 0, grd$WEold)
  grd$NAsort <- ifelse(abs(grd$NS)+abs(grd$WE) == 0, NA, 1)
  grd$Grad <- grd$NAsort * sqrt((grd$WE^2) + (grd$NS^2))
  grd$NS <- grd$NAsort * grd$NS
  grd$WE <- grd$NAsort * grd$WE
  velocity <- data.frame(x = slope$x, y = slope$y, temporal_trend = slope$slope, spatial_gradient = grd$Grad, NSgrad = grad$NS, WEgrad = grad$WE, angle = grad$angle, w = slope$w, icell = grad$icell)
  velocity$velocity <- with(velocity, temporal_trend/spatial_gradient)
  velocity$anglenew <- ifelse( velocity$temporal_trend < 0, (velocity$angle+180)%%360, (velocity$angle)%%360)
  velocity$velocityX <- -velocity$velocity*(velocity$WEgrad/sqrt(velocity$WEgrad^2+velocity$NSgrad^2))
  velocity$velocityY <- -velocity$velocity*(velocity$NSgrad/sqrt(velocity$WEgrad^2+velocity$NSgrad^2))    
  return(velocity)
}

#####color palette
zhu_col <- function(n = 100) { # strech low values

  col.str <- fields::tim.colors(n)
  n.str   <- length(col.str)
  col.string <- character()
  for (i in 1:n.str) {
    col.string <- c(col.string, rep(col.str[i], i)) # rep numbers to stretch color bar
  }
  col.func <- colorRampPalette(col.string)
    
  col.func(n)

}

col_pal_red<-colorRampPalette(c(rgb(1,0,0,0), rgb(1,0,0,1)), alpha = TRUE)(7)
col_pal_blue<-colorRampPalette(c(rgb(0,0,1,0), rgb(0,0,1,1)), alpha = TRUE)(7)
col_pal_redblue<-c(colorRampPalette(c(rgb(1,0,0,1), rgb(1,0,0,0.2)), alpha = TRUE)(6),
                   colorRampPalette(c(rgb(0,0,1,0.2), rgb(0,0,1,1)), alpha = TRUE)(6))

#####For calculating acceleration

####Function to calculate Vc and Vp
velocitiesatYear <- function (end,tempscale) {
  start <- end-tempscale+1
  
  C <- Cfull[[as.numeric(start-1957):as.numeric(end-1957)]]
  slopedat <- calcslope(C, divisor = 1)
  allyears <- rep(1, nlayers(C))
  mn <- stackApply(C, indices = allyears, fun = mean)
  spatx <- spatialgrad(mn,y_dist = c(111.325, 111.325), y_diff = res(C)[1])
  velodf <- calcvelocityXY(spatx, slopedat)
  rtrendC <- rgradC <- rvoccC <- rangleC <- rvoccCX <- rvoccCY <- raster(C)
  rgradC[spatx$icell] <- sqrt((spatx$NS)^2+ (spatx$WE)^2)
  rtrendC[slopedat$icell] <- slopedat$slope
  rvoccC[velodf$icell] <- abs(velodf$velocity)
  rangleC[velodf$icell] <- velodf$anglenew
  rvoccCX[velodf$icell] <- velodf$velocityX
  rvoccCY[velodf$icell] <- velodf$velocityY
  
  P <- Pfull[[as.numeric(start-1980):as.numeric(end-1980)]]
  slopedat <- calcslope(P, divisor = 1) 
  allyears <- rep(1, nlayers(P))
  mn <- stackApply(P, indices = allyears, fun = mean)
  spatx <- spatialgrad(mn,y_dist = c(111.325, 111.325), y_diff = res(P)[1])
  velodf <- calcvelocityXY(spatx, slopedat)
  rtrendP <- rgradP <- rvoccP <- rangleP <- rvoccPX <- rvoccPY <- raster(P)
  rgradP[spatx$icell] <- sqrt((spatx$NS)^2+ (spatx$WE)^2)
  rtrendP[slopedat$icell] <- slopedat$slope
  rvoccP[velodf$icell] <- abs(velodf$velocity)
  rangleP[velodf$icell] <- velodf$anglenew
  rvoccPX[velodf$icell] <- velodf$velocityX
  rvoccPY[velodf$icell] <- velodf$velocityY
  
  projection <-(rvoccCX*rvoccPX+rvoccCY*rvoccPY)/rvoccC
  mismatch <- (rvoccC-projection)
  anglediff <- abs(((rangleC-rangleP) + 180) %% 360 - 180)
  
  compiled<-brick(rtrendC, rgradC, rvoccC, rangleC, rvoccCX, rvoccCY,
                  rtrendP, rgradP, rvoccP, rangleP, rvoccPX, rvoccPY,
                  projection, mismatch, anglediff)
  
  return(compiled)
}

DifferenceatYear <- function (end,tempscale) {
  start <- end-tempscale+1
  
  TMP <- brick(TMPfull[[as.numeric(start-1900):as.numeric(end-1900)]])
  slopedat_TMP <- calcslope(TMP, divisor = 1)
  allyears_TMP <- rep(1, nlayers(TMP))
  mnTMP <- stackApply(TMP, indices = allyears_TMP, fun = mean)
  spatx_TMP <- spatialgrad(mnTMP,y_dist = c(111.325, 111.325), y_diff = 0.5)
  velodf_TMP <- calcvelocityXY(spatx_TMP, slopedat_TMP)
  rvoccC <- rvoccCX <- rvoccCY <- raster(TMP)
  rvoccC[velodf_TMP$icell] <- abs(velodf_TMP$velocity)
  rvoccCX[velodf_TMP$icell] <- velodf_TMP$velocityX
  rvoccCY[velodf_TMP$icell] <- velodf_TMP$velocityY
  
  P <- brick(Pfull[[as.numeric(start-1980):as.numeric(end-1980)]])
  slopedat_P <- calcslope(P, divisor = 1) 
  allyears_P <- rep(1, nlayers(P))
  mnP <- stackApply(P, indices = allyears_P, fun = mean)
  spatx_P <- spatialgrad(mnP,y_dist = c(111.325, 111.325), y_diff = 0.5)
  velodf_P <- calcvelocityXY(spatx_P, slopedat_P)
  rvoccPX <- rvoccPY <- raster(P)
  rvoccPX[velodf_P$icell] <- velodf_P$velocityX
  rvoccPY[velodf_P$icell] <- velodf_P$velocityY
  
  projection <-(rvoccCX*rvoccPX+rvoccCY*rvoccPY)/rvoccC
  difference <- projection-rvoccC
  
  return(difference)
}

ProjectionatYear <- function (end,tempscale) {
  start <- end-tempscale+1
  
  TMP <- brick(TMPfull[[as.numeric(start-1900):as.numeric(end-1900)]])
  slopedat_TMP <- calcslope(TMP, divisor = 1)
  allyears_TMP <- rep(1, nlayers(TMP))
  mnTMP <- stackApply(TMP, indices = allyears_TMP, fun = mean)
  spatx_TMP <- spatialgrad(mnTMP,y_dist = c(111.325, 111.325), y_diff = 0.5)
  velodf_TMP <- calcvelocityXY(spatx_TMP, slopedat_TMP)
  rvoccC <- rvoccCX <- rvoccCY <- raster(TMP)
  rvoccC[velodf_TMP$icell] <- abs(velodf_TMP$velocity)
  rvoccCX[velodf_TMP$icell] <- velodf_TMP$velocityX
  rvoccCY[velodf_TMP$icell] <- velodf_TMP$velocityY
  
  P <- brick(Pfull[[as.numeric(start-1980):as.numeric(end-1980)]])
  slopedat_P <- calcslope(P, divisor = 1) 
  allyears_P <- rep(1, nlayers(P))
  mnP <- stackApply(P, indices = allyears_P, fun = mean)
  spatx_P <- spatialgrad(mnP,y_dist = c(111.325, 111.325), y_diff = 0.5)
  velodf_P <- calcvelocityXY(spatx_P, slopedat_P)
  rvoccPX <- rvoccPY <- raster(P)
  rvoccPX[velodf_P$icell] <- velodf_P$velocityX
  rvoccPY[velodf_P$icell] <- velodf_P$velocityY
  
  projection <-(rvoccCX*rvoccPX+rvoccCY*rvoccPY)/rvoccC
  
  return(projection)
}

RatioatYear <- function (end,tempscale) {
  start <- end-tempscale+1
  
  TMP <- brick(TMPfull[[as.numeric(start-1900):as.numeric(end-1900)]])
  slopedat_TMP <- calcslope(TMP, divisor = 1)
  allyears_TMP <- rep(1, nlayers(TMP))
  mnTMP <- stackApply(TMP, indices = allyears_TMP, fun = mean)
  spatx_TMP <- spatialgrad(mnTMP,y_dist = c(111.325, 111.325), y_diff = 0.5)
  velodf_TMP <- calcvelocityXY(spatx_TMP, slopedat_TMP)
  rvoccC <- rvoccCX <- rvoccCY <- raster(TMP)
  rvoccC[velodf_TMP$icell] <- abs(velodf_TMP$velocity)
  rvoccCX[velodf_TMP$icell] <- velodf_TMP$velocityX
  rvoccCY[velodf_TMP$icell] <- velodf_TMP$velocityY
  
  P <- brick(Pfull[[as.numeric(start-1980):as.numeric(end-1980)]])
  slopedat_P <- calcslope(P, divisor = 1) 
  allyears_P <- rep(1, nlayers(P))
  mnP <- stackApply(P, indices = allyears_P, fun = mean)
  spatx_P <- spatialgrad(mnP,y_dist = c(111.325, 111.325), y_diff = 0.5)
  velodf_P <- calcvelocityXY(spatx_P, slopedat_P)
  rvoccPX <- rvoccPY <- raster(P)
  rvoccPX[velodf_P$icell] <- velodf_P$velocityX
  rvoccPY[velodf_P$icell] <- velodf_P$velocityY
  
  projection <-(rvoccCX*rvoccPX+rvoccCY*rvoccPY)/rvoccC
  ratio<-projection/rvoccC
  
  return(ratio)
}


#####plot without outliers
crop_remove_outlier<-function(ras,e) {
  ras_crop<-crop(ras,e)
  ras_crop[ras_crop<quantile(ras_crop,0.01)]<-NA
  ras_crop[ras_crop>quantile(ras_crop,0.99)]<-NA
  return(ras_crop)
}


#####compile velocity metrics into a single raster brick
velocity_compile<-function(data) {
  
  slopedat <- calcslope(data,divisor = 1)
  
  allyears <- rep(1, nlayers(data))
  mn <- stackApply(data, indices = allyears, fun = mean)
  spatx <- spatialgrad(mn,y_dist = c(111.325, 111.325), y_diff = res(data)[1])
  
  velodf <- calcvelocityXY(spatx, slopedat)
  
  rtrend <- rgrad <- rvocc <- rangle <- rvoccX <- rvoccY <- raster(data)
  rgrad[spatx$icell] <- sqrt((spatx$NS)^2+ (spatx$WE)^2)
  rtrend[slopedat$icell] <- slopedat$slope
  rvocc[velodf$icell] <- abs(velodf$velocity)
  rangle[velodf$icell] <- velodf$anglenew
  rvoccX[velodf$icell] <- velodf$velocityX
  rvoccY[velodf$icell] <- velodf$velocityY
  
  compiled<-brick(rtrend, rgrad, rvocc, rangle, rvoccX, rvoccY)
}

####crop to urban and rural extents
crop_urban_rural<-function(layer,box,option) {
  if (option=="urban") mask<-crop(urban,extent(box))
  if (option=="rural") mask<-crop(rural,extent(box))
  layer<-crop(layer,extent(box))
  layer<-disaggregate(layer,fact=res(layer)[1]/res(urban)[1])
  layer<-resample(layer,mask,method="ngb")
  layer<-layer*mask
  return(layer)
}

####label legend at selected values
labelpts <- function(layer,vector) {
  labelpts<-rep(NA,length(vector))
  for (i in 1:length(vector)) {
    labelpts[i]<-mean(na.omit(values(layer)) < vector[i])
  }
  return (labelpts)
}
