# Diana Gerardo
# Using splm per time period 
# parallel version

####################################################################################
# SETUP ----------------------------------------------------------------------------
####################################################################################

# clear the global environment
rm(list = ls())

# packages needed
libs = c('foreach','doParallel','ggplot2','rgdal', 'Matrix', 'spBayes',
         'coda', 'graphics',"tidyverse", 'viridis')
sapply(libs, require, character.only = TRUE)

# read in the data
vel1000 <- read.csv('~/UCSC_capstone/DATA/Vel1000_AEQD_OutRem.csv')
knots <- read.csv('~/UCSC_capstone/DATA/108knots_aeqd.csv')
knots <- as.matrix(knots)

####################################################################################
# spLM: Predictive Process ---------------------------------------------------------
####################################################################################
# I'm working on Windows so I can't use parallel with the forking Thus, I want 
# to make clusters where in one cluster I'll have a model running for t=1 and 
# in another cluster I'll have a model running for t=2.

# Calculate the number of cores. I want at least 1 free core in order
# to still use the computer while the n_cores are still running.
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(spBayes))
registerDoParallel(cl)

source("~/UCSC_capstone/spLM-PTP/pp-initial-inputs.R")

# run predictive process model
time.period <- 1:25
system.time( 
        model.t <- foreach(i = time.period,
                .combine = list,
                .multicombine = TRUE) %dopar%{
        #Subset the data per time frame
        vel1000.t <- vel1000[vel1000$t == i,]
        aeqd <- cbind(x = vel1000.t$x, y = vel1000.t$y) # aeqd coords at t
        
        # label response, predictor, and design matrix
        y <- vel1000.t$V_CVI
        V_MAT <- vel1000.t$V_MAT
        X <- as.matrix(V_MAT)
        
        # Predictive Process
        set.seed(8000)
        pp <- spLM(y ~ X, coords=aeqd, n.samples = 10000, cov.model = cov.model[i],
                   priors = priors[[i]], tuning = tuning[[i]],
                   starting = starting[[i]], knots = knots, modified.pp = F)
        # Recover spatial effects w's
        spRecover(pp, start = 5000)
        } 
)
stopCluster(cl)   # close the cluster. very important

# user  system elapsed 
# 6.67   16.60 6915.98/~2hrs


# quantiles of the spatial effects w's
pp.w.summary <- NULL       
for(k in time.period){
        pp.w.summary[[k]] <- summary(mcmc(t(model.t[[k]]$p.w.recover.samples)))$quantiles[,c(3,1,5)]
}

####################################################################################
# spLM: Model.t Results-------------------------------------------------------------
####################################################################################

# Trace plots of all posterior parameters
# traceplot(pp$p.theta.recover.samples)
setwd('~/UCSC_capstone/spLM-PTP/traceplots')
for(j in time.period){
        pdf(paste("traceplot", j, ".pdf", sep = ""))
        par(mfrow=c(3,2))
        plot(0:4, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
        text(x=2.5, y = 2, labels = paste("Time Period", j, sep = " "), adj = NULL,
             pos = NULL, offset = NULL, vfont = NULL,
             cex = 1.5, col = 'firebrick3', font = 1)
        plot.ts(model.t[[j]]$p.beta.recover.samples[,1], ylab=expression(beta[0]),
                main=expression("Trace of "~beta[0]))
        plot.ts(model.t[[j]]$p.beta.recover.samples[,2], ylab=expression(beta[1]),
                main=expression("Trace of "~beta[1]))
        plot.ts(model.t[[j]]$p.theta.recover.samples[,1], ylab=expression(sigma^2),
                main=expression("Trace of "~sigma^2))
        plot.ts(model.t[[j]]$p.theta.recover.samples[,2], ylab=expression(tau^2),
                main=expression("Trace of "~tau^2))
        plot.ts(model.t[[j]]$p.theta.recover.samples[,3], ylab=expression(phi),
                main=expression("Trace of "~phi))
        dev.off()
        
}

# saving posterior parameters for t = 1
setwd('~/UCSC_capstone/spLM-PTP/Posterior_Parameters')
for (h in time.period) {
        sigma.t <- c(model.t[[h]]$p.theta.recover.samples[,1])
        sigma.t <- data.frame(sigma = sigma.t)
        sigma.t %>% as.tibble %>% write_csv(paste('sigma_t',h,'.csv', sep = ''))
        
        tau.t <- c(model.t[[h]]$p.theta.recover.samples[,2])
        tau.t <- data.frame(tau = tau.t)
        tau.t %>% as.tibble %>% write_csv(paste('tausq_t',h,'.csv', sep = ''))
        
        phi.t <- c(model.t[[h]]$p.theta.recover.samples[,3])
        phi.t <- data.frame(phi = phi.t)
        phi.t %>% as.tibble %>% write_csv(paste('phi_t',h,'.csv', sep = ''))
        
        beta0.t <- c(model.t[[h]]$p.beta.recover.samples[,1])
        beta0.t <- data.frame(beta0 = beta0.t)
        beta0.t %>% as.tibble %>% write_csv(paste('beta0_t',h,'.csv', sep = ''))
        
        beta1.t <- c(model.t[[h]]$p.beta.recover.samples[,2])
        beta1.t <- data.frame(beta1 = beta1.t)
        beta1.t %>% as.tibble %>% write_csv(paste('beta1_t',h,'.csv', sep = ''))
}


# ggplots in the plot window
setwd('~/UCSC_capstone/spLM-PTP/Images')
for(g in time.period){
        vel1000.ss <- data.frame(lon = vel1000[vel1000$t==g, ]$lon,
                            lat = vel1000[vel1000$t==g, ]$lat )
        y <- vel1000[vel1000$t==g, ]$V_CVI
        
        # Spatial effects Image
        spat.eff <- ggplot(vel1000.ss, aes(lon, lat)) +
                geom_point(size = 1, alpha=1) +
                borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
                geom_point(aes(colour=pp.w.summary[[g]][,1]), size = 1, alpha=0.8) +
                scale_colour_viridis_c(option="inferno")+
                scale_y_continuous(breaks = (-2:2) * 30) +
                scale_x_continuous(breaks = (-4:4) * 45) +
                coord_map("ortho", orientation=c(61, 90, 0)) +
                labs(title = paste("Russia: Spatial Effects at t =", g, "for 1000 locations", sep = ' '),
                     x="lon", y="lat", col="w(s)")
        ggsave(paste('spatialeffects-t',g,'.png',sep = ''), device = 'png')
        
        # V_CVI posterior image
        vcvi.t <- ggplot(vel1000.ss, aes(lon, lat)) +
                geom_point(size = 1, alpha = 1) +
                borders("world", xlim = c(-180,180) , ylim = c(40,90)) +
                geom_point(aes(colour = y), size = 1, alpha = 0.8) +
                scale_colour_viridis_c(option = "inferno")+
                scale_y_continuous(breaks = (-2:2) * 30) +
                scale_x_continuous(breaks = (-4:4) * 45) +
                coord_map("ortho", orientation=c(61, 90, 0)) +
                labs(title = paste("Russia: V_CVI at t =",  g, "for 1000 locations", sep = " "),
                     x = "lon", y = "lat", col = "V_CVI")
        ggsave(paste('obs-t',g,'-vcvi-1000.png',sep = ''), device = 'png')
        
}


####################################################################################
# spPredict: Test Set --------------------------------------------------------------
####################################################################################

# read in test set. The test set is the rest of the 'Velocity_AEQD_OutRem.csv'
# that does not correspond to the locations in 'Vel1000_AEQD_OutRem.csv'
testSet <- read.csv("~/UCSC_capstone/DATA/testSet.csv")
#model.predict <- NULL

cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(spBayes))
registerDoParallel(cl)

system.time(
        model.predict <- foreach(z = time.period,
                                 .combine = list,
                                 .multicombine = TRUE) %dopar%{
                        TestSet <- testSet[testSet$t == z,]
                        B <- 1000
                        set.seed(3500 + z)
                        test1000 <- TestSet[sample(nrow(TestSet), B), ]
                        X.test <- cbind(1,test1000$V_MAT)
                        aeqd.test <- cbind(x = test1000$x, y=test1000$y)
                        spPredict(model.t[[z]], pred.coords = aeqd.test, pred.covars = X.test)
                        }        
)

stopCluster(cl)   # close the cluster. very important
# user   system  elapsed 
# 31.09   114.71 39298.14/ ~11hrs

####################################################################################
# spPredict: Results ---------------------------------------------------------------
####################################################################################

 test.y <- NULL
 test.ss <- NULL
 test.aeqd <- NULL

# Accuracy Checking Business
for(f in time.period){
        testSet <- read.csv("~/UCSC_capstone/DATA/testSet.csv")
        testSet <- testSet[testSet$t == f,]
        B <- 1000
        set.seed(3500 + f)
        test1000 <- testSet[sample(nrow(testSet), B), ]
        test.y[[f]] <- test1000$V_CVI
        test.ss[[f]] <- data.frame(lon = test1000$lon, lat = test1000$lat)
        test.aeqd[[f]] <- cbind(x = test1000$x, y=test1000$y)
                            
        pred.int <- apply(model.predict[[f]]$p.y.predictive.samples, 1, 
                          function(x) quantile(x, c(0.025,0.975)))
        
        # proportion of observatins that fall inside the prediction interval
        print(
            1-(sum(test.y[[f]] > pred.int[2,] | test.y[[f]] < pred.int[1,])/nrow(test1000))
        )
}
 # [1] 0.971    [11] 0.998      [21] 0.982     
 # [2] 0.964    [12] 0.968      [22] 0.962
 # [3] 0.947    [13] 0.992      [23] 0.958
 # [4] 0.954    [14] 0.997      [24] 0.959
 # [5] 1        [15] 0.992      [25] 0.968
 # [6] 0.999    [16] 0.961
 # [7] 0.998    [17] 0.971
 # [8] 0.969    [18] 0.969
 # [9] 0.954    [19] 0.968
 # [10] 0.951   [20] 0.967


# quantile plot
quant <- function(x){quantile(x, prob=c(0.5, 0.025, 0.975))}
test.y.pred <- NULL
setwd('~/UCSC_capstone/spLM-PTP/Images')
for(u in time.period){
        test.y.p <- apply(model.predict[[u]]$p.y.predictive.samples, 1,quant)
        test.y.pred[[u]] <- t(test.y.p)
        #png(paste("pred-quantile-t", u, ".png", sep = ""))
        setEPS()
        postscript(paste("pred-quantile-t", u, ".eps", sep = ""))
        plot(test.y[[u]],test.y.pred[[u]][,1], pch=19, cex=0.5, xlab="observed",
             ylab="predicted", main=paste("Observed vs. Predicted V_CVI at t = ", u, sep=''),
             ylim=c(-300,300), xlim = c(-300,300))
        lines(-300:300, -300:300, col="blue")
        arrows(test.y[[u]], test.y.pred[[u]][,1], test.y[[u]], test.y.pred[[u]][,3], length=0.02, angle=90)
        arrows(test.y[[u]], test.y.pred[[u]][,1], test.y[[u]], test.y.pred[[u]][,2], length=0.02, angle=90)
        dev.off()
}

#ylim=c(-100,50), xlim = c(-100,50)
#lines(-100:50, -100:50, col="blue")

# Test Set Maps
rng <- lapply(X = time.period, function(x){range(c((test.y[[x]]), (test.y.pred[[x]])))}) 
setwd('~/UCSC_capstone/spLM-PTP/Images')
for(u in time.period){
        obs.test.map <- ggplot(test.ss[[u]], aes(lon, lat)) +
                geom_point(size = 1, alpha=1) +
                borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
                geom_point(aes(colour=test.y[[u]]), size = 1, alpha=0.8) +
                scale_colour_gradientn(colours = inferno(15), 
                                       limits=c(floor(rng[[u]][1]), ceiling(rng[[u]][2])))+
                scale_y_continuous(breaks = (-2:2) * 30) +
                scale_x_continuous(breaks = (-4:4) * 45) +
                coord_map("ortho", orientation=c(61, 90, 0)) +
                labs(title = paste("Test Set: Observed V_CVI at t =", u, sep = ' '), x="lon", y="lat", col="V_CVI")
        ggsave(paste('test-obsmap-t', u, '.png',sep = ''), device = 'png')
        
        pred.test.map <- ggplot(test.ss[[u]], aes(lon, lat)) +
                geom_point(size = 1, alpha=1) +
                borders("world", xlim=c(-180,180) , ylim=c(40,90)) +
                geom_point(aes(colour=test.y.pred[[u]][,1]), size = 1, alpha=0.8) +
                scale_colour_gradientn(colours = inferno(15), 
                                       limits=c(floor(rng[[u]][1]), ceiling(rng[[u]][2])))+
                scale_y_continuous(breaks = (-2:2) * 30) +
                scale_x_continuous(breaks = (-4:4) * 45) +
                coord_map("ortho", orientation=c(61, 90, 0)) +
                labs(title = paste("Test Set: Predicted V_CVI at t =", u, sep = ' '), x="lon", y="lat", col="V_CVI")
        ggsave(paste('test-predmap-t',u,'.png',sep=''), device = 'png')
}





