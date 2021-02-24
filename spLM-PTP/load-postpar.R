# Diana Gerardo

setwd("~/UCSC_capstone/spLM-PTP")

# posterior data-----------------------------------------------------------------------
beta0 <- NULL
beta1 <- NULL
phi <- NULL
sigma <- NULL
tausq <- NULL

# read in data
for(i in 1:25){
  beta0[[i]] <- read.csv(
    paste("~/UCSC_capstone/spLM-PTP/Posterior_Parameters/beta0_t", i, ".csv", sep = "")
  )
  beta1[[i]] <- read.csv(
    paste("~/UCSC_capstone/spLM-PTP/Posterior_Parameters/beta1_t", i, ".csv", sep = "")
  )
  phi[[i]] <- read.csv(
    paste("~/UCSC_capstone/spLM-PTP/Posterior_Parameters/phi_t", i, ".csv", sep = "")
  )
  sigma[[i]] <- read.csv(
    paste("~/UCSC_capstone/spLM-PTP/Posterior_Parameters/sigma_t", i, ".csv", sep = "")
  )
  tausq[[i]] <- read.csv(
    paste("~/UCSC_capstone/spLM-PTP/Posterior_Parameters/tausq_t", i, ".csv", sep = "")
  )
  
  colnames(beta0[[i]]) <- paste("t", i, sep = "")
  colnames(beta1[[i]]) <- paste("t", i, sep = "")
  colnames(phi[[i]]) <- paste("t", i, sep = "")
  colnames(sigma[[i]]) <- paste("t", i, sep = "")
  colnames(tausq[[i]]) <- paste("t", i, sep = "")
}

# create dataframe
beta0.df <- do.call(cbind.data.frame, beta0)
beta1.df <- do.call(cbind.data.frame, beta1)
phi.df <- do.call(cbind.data.frame, phi)
sigma.df <- do.call(cbind.data.frame, sigma)
tausq.df <- do.call(cbind.data.frame, tausq)


# Quantiles -----------------------------------------------------------------------------
quant <- function(x){quantile(x, prob=c(0.5, 0.025, 0.975))}
N.t <- 25

b0 <- apply(beta0.df, 2, quant)
plot(1:N.t, b0[1,], pch=19, cex=0.5, xlab="Time Period", ylab= expression(beta[0]),
     ylim=range(b0))
arrows(1:N.t, b0[1,], 1:N.t, b0[3,], length=0.02, angle=90)
arrows(1:N.t, b0[1,], 1:N.t, b0[2,], length=0.02, angle=90)
abline(h=0, col="red", lty="dashed")

b1 <- apply(beta1.df, 2, quant)
b1plot(1:N.t, b1[1,], pch=19, cex=0.5, xlab="Time Period", ylab=expression(beta[1]),
     ylim=range(b1))
arrows(1:N.t, b1[1,], 1:N.t, b1[3,], length=0.02, angle=90)
arrows(1:N.t, b1[1,], 1:N.t, b1[2,], length=0.02, angle=90)
abline(h=0, col="red", lty="dashed")

sig <- apply(sigma.df, 2, quant)
plot(1:N.t, sig[1,], pch=19, cex=0.5, xlab="Time Period", ylab=expression(sigma^2), 
     ylim=range(sig))
arrows(1:N.t, sig[1,], 1:N.t, sig[3,], length=0.02, angle=90)
arrows(1:N.t, sig[1,], 1:N.t, sig[2,], length=0.02, angle=90)
abline(h=0, col="red", lty="dashed")


tau <- apply(tausq.df, 2, quant)
plot(1:N.t, tau[1,], pch=19, cex=0.5, xlab="Time Period", ylab=expression(tau^2), 
     ylim=range(tau))
arrows(1:N.t, tau[1,], 1:N.t, tau[3,], length=0.02, angle=90)
arrows(1:N.t, tau[1,], 1:N.t, tau[2,], length=0.02, angle=90)
abline(h=0, col="red", lty="dashed")

phis <- apply(phi.df, 2, quant)
plot(1:N.t, phis[1,], pch=19, cex=0.5, xlab="Time Period", ylab=expression(phi), 
     ylim=range(phis))
arrows(1:N.t, phis[1,], 1:N.t, phis[3,], length=0.02, angle=90)
arrows(1:N.t, phis[1,], 1:N.t, phis[2,], length=0.02, angle=90)
abline(h=0, col="red", lty="dashed")


x=15
b0[,x]; b1[,x] ; sig[,x]; tau[,x] ;phis[,x]
   
   