# predictive process initial inputs
# the inputs that I will put in spLM

starting <- NULL
priors <- NULL
tuning <- NULL
cov.model <- NULL

p <- 2    # number of betas, this never changes

# t = 1-----------------------------------------------------------------------------
starting[[1]] <- list("beta" = c(1,1), "sigma.sq" = 40, "tau.sq" = 60, "phi" = .01)
priors[[1]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)),
                    "sigma.sq.ig" = c(3,2), 
                    "tau.sq.ig" = c(2,2),
                    "phi.Unif" = c(0.001,1))
tuning[[1]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[1] <- 'exponential'

# t = 2 ----------------------------------------------------------------------------
starting[[2]] <- list("beta"= c(1,1), "sigma.sq" = 59, "tau.sq" = 39, "phi" = .02)
priors[[2]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                    "sigma.sq.ig" = c(3, 2), 
                    "tau.sq.ig" = c(2, 2),
                    "phi.Unif" = c(0.001, 1))
tuning[[2]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model [2] <- 'exponential'

# t = 3 ----------------------------------------------------------------------------
starting[[3]] <- list("beta"= c(1,1), "sigma.sq" = 28, "tau.sq" = 39, "phi" = .009)
priors[[3]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                    "sigma.sq.ig" = c(3, 2), 
                    "tau.sq.ig" = c(2, 2),
                    "phi.Unif" = c(0.001, 1))
tuning[[3]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[3] <- 'exponential'

# t = 4 ----------------------------------------------------------------------------
starting[[4]] <- list("beta" = c(1,1), "sigma.sq" = 43, "tau.sq" = 47, "phi" = .006)
priors[[4]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                    "sigma.sq.ig" = c(3,2),
                    "tau.sq.ig" = c(2,2), 
                    "phi.Unif" = c(0.001,1))
tuning[[4]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[4] <- 'exponential'

# t = 5 ----------------------------------------------------------------------------
# no convergence
starting[[5]] <- list("beta" = c(1,1), "sigma.sq" = 800, "tau.sq" = 1, "phi" = .14,
                 "nu"=2.5)
priors[[5]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
               "sigma.sq.ig" = c(3,2), 
               "tau.sq.ig" = c(2,2), 
               "phi.Unif" = c(0.001,1),
               "nu.Unif"=c(.8,4))
tuning[[5]] <- list("sigma.sq" = .03, "tau.sq" = .1, "phi" = .05, "nu"=0)
cov.model[5] <- 'matern'

# t = 6 ----------------------------------------------------------------------------
# no convergence
starting[[6]] <- list("beta"= c(1,1), "sigma.sq" = 1500, "tau.sq" = 2, "phi" = 0.06,
                 "nu"=1)
priors[[6]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
               "sigma.sq.ig" = c(3, 2), 
               "tau.sq.ig" = c(2, 2),
               "phi.Unif" = c(0.01, 1), 
               "nu.unif"=c(0.5,4))
tuning[[6]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .01, "nu"=0)
cov.model[6] <- 'matern'

# t = 7 ----------------------------------------------------------------------------
# no convergence
starting[[7]] <- list("beta"= c(1,1), "sigma.sq" = 700, "tau.sq" = 0.5, "phi" = 0.05,
                 "nu"=1.5)
priors[[7]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
               "sigma.sq.ig" = c(3, 2), 
               "tau.sq.ig" = c(2, 2),
               "phi.Unif" = c(0.01, 1), 
               "nu.Unif"=c(0.8,4))
tuning[[7]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .01, "nu"=0)
cov.model[7] <- 'matern'

# t = 8 ----------------------------------------------------------------------------
starting[[8]] <- list("beta" = c(1,1), "sigma.sq" = 150, "tau.sq" = 305, "phi" = .01)
priors[[8]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                    "sigma.sq.ig" = c(3,2),
                    "tau.sq.ig" = c(2,2), 
                    "phi.Unif" = c(0.001,1))
tuning[[8]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[8] <- 'exponential'
#great

# t = 9 ----------------------------------------------------------------------------
starting[[9]] <- list("beta" = c(1,1), "sigma.sq" = 100, "tau.sq" = 305, "phi" = .01)
priors[[9]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                    "sigma.sq.ig" = c(3,2),
                    "tau.sq.ig" = c(2,2), 
                    "phi.Unif" = c(0.001,1))
tuning[[9]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .1)
cov.model[9] <- 'exponential'
#great

# t = 10 ---------------------------------------------------------------------------
starting[[10]] <- list("beta" = c(1,1), "sigma.sq" = 200, "tau.sq" = 500, "phi" = .03)
priors[[10]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                     "sigma.sq.ig" = c(3,2),
                     "tau.sq.ig" = c(2,2), 
                     "phi.Unif" = c(0.001,1))
tuning[[10]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .1)
cov.model[10] <- 'exponential'

# t = 11 ---------------------------------------------------------------------------
# no convergence
starting[[11]] <- list("beta"= c(1,1), "sigma.sq" = 9000, "tau.sq" = .2, "phi" = 0.6,
                 "nu"=2.5)
priors[[11]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1),
                     "nu.unif"=c(0.8,4))
tuning[[11]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .5, "nu"=0)
cov.model[11] <- 'matern'

# t = 12 ---------------------------------------------------------------------------
# no convergence
starting[[12]] <- list("beta"= c(1,1), "sigma.sq" = 2500, "tau.sq" = 0.6, "phi" = 0.2,
                 "nu"=2.5)
priors[[12]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1),
                     "nu.unif"=c(1,4))
tuning[[12]] <- list("sigma.sq" = .01, "tau.sq" = .07, "phi" = .03, "nu"=0)
cov.model[12] <- 'matern'

# t = 13 ---------------------------------------------------------------------------
# no convergence
starting[[13]] <- list("beta" = c(1,1), "sigma.sq" = 8300, "tau.sq" = .5, "phi" = .3,
                 "nu"=2.5)
priors[[13]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                     "sigma.sq.ig" = c(3,2),
                     "tau.sq.ig" = c(2,2), 
                     "phi.Unif" = c(0.001,1),
                     "nu.Unif"=c(.8,4))
tuning[[13]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .1, "nu"=0)
cov.model[13] <- 'matern'

# t = 14 ---------------------------------------------------------------------------
# no convergence
starting[[14]] <- list("beta"= c(1,1), "sigma.sq" = 9000, "tau.sq" = .5, "phi" = 0.3,
                 "nu"=2.5)
priors[[14]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1),
                     "nu.Unif"=c(0.8,4))
tuning[[14]] <- list("sigma.sq" = .01, "tau.sq" = .1, "phi" = .1, "nu"=0)
cov.model[14] <- 'matern'

# t = 15 ---------------------------------------------------------------------------
# no convergence
starting[[15]] <- list("beta"= c(1,1), "sigma.sq" = .5, "tau.sq" = 13000, "phi" = 0.6)
priors[[15]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1))
tuning[[15]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[15] <- 'exponential'

# t = 16 ---------------------------------------------------------------------------
starting[[16]] <- list("beta"= c(1,1), "sigma.sq" = 200, "tau.sq" = 650, "phi" = 0.004)
priors[[16]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1))
tuning[[16]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .1)
cov.model[16] <- 'exponential'

# t = 17 ---------------------------------------------------------------------------
starting[[17]] <- list("beta" = c(1,1), "sigma.sq" = 40, "tau.sq" = 500, "phi" = .004,
                 "nu"=2.5)
priors[[17]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                     "sigma.sq.ig" = c(3,2),
                     "tau.sq.ig" = c(2,2), 
                     "phi.Unif" = c(0.001,1),
                     "nu.Unif"=c(0.5,4))
tuning[[17]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1, "nu"=0)
cov.model[17] <- 'matern'
# "nu.Unif"=c(0.8,4)

# t = 18 ---------------------------------------------------------------------------
starting[[18]] <- list("beta"= c(1,1), "sigma.sq" = 62, "tau.sq" = 161, "phi" = 0.03)
priors[[18]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1))
tuning[[18]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[18] <- 'exponential'

# t = 19 ---------------------------------------------------------------------------
starting[[19]] <- list("beta"= c(1,1), "sigma.sq" = 65, "tau.sq" = 50, "phi" = .02)
priors[[19]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
               "sigma.sq.ig" = c(3, 2), "tau.sq.ig" = c(2, 2),
               "phi.Unif" = c(0.001, 1))
tuning[[19]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[19] <- 'exponential'

# t = 20 ---------------------------------------------------------------------------
starting[[20]] <- list("beta"= c(1,1), "sigma.sq" = 51, "tau.sq" = 107, "phi" = 0.01,
                 "nu"=1.5)
priors[[20]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1),
                     "nu.unif"=c(0.5,4))
tuning[[20]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1, "nu"=0)
cov.model[20] <- 'matern'

# t = 21 ---------------------------------------------------------------------------
# no convergence
starting[[21]] <- list("beta" = c(1,1), "sigma.sq" = 650, "tau.sq" = 0.3, "phi" = .013)
priors[[21]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3,2),
                     "tau.sq.ig" = c(2,2),
                     "phi.Unif" = c(0.001,1))
tuning[[21]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .01)
cov.model[21] <- 'exponential'

# t = 22 ---------------------------------------------------------------------------
starting[[22]] <- list("beta"= c(1,1), "sigma.sq" = 51, "tau.sq" = 107, "phi" = 0.01,
                 "nu"=1.5)
priors[[22]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1),
                     "nu.unif"=c(0.5,4))
tuning[[22]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1, "nu"=0)
cov.model[22] <- 'matern'

# t = 23 ---------------------------------------------------------------------------
starting[[23]] <- list("beta" = c(1,1), "sigma.sq" = 110, "tau.sq" = 173, "phi" = .01)
priors[[23]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                     "sigma.sq.ig" = c(3,2),
                     "tau.sq.ig" = c(2,2), 
                     "phi.Unif" = c(0.001,1))
tuning[[23]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .1)
cov.model[23] <- 'exponential'

# t = 24 ---------------------------------------------------------------------------
starting[[24]] <- list("beta"= c(1,1), "sigma.sq" = 33, "tau.sq" = 124, "phi" = .007)
priors[[24]] <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                     "sigma.sq.ig" = c(3, 2),
                     "tau.sq.ig" = c(2, 2),
                     "phi.Unif" = c(0.001, 1))
tuning[[24]] <- list("sigma.sq" = .05, "tau.sq" = .005, "phi" = .2)
cov.model[24] <- 'exponential'

# t = 25 ---------------------------------------------------------------------------
# no converence
starting[[25]] <- list("beta" = c(1,1), "sigma.sq" = 160, "tau.sq" = .5, "phi" = .018)
priors[[25]] <- list("beta.Norm" = list(rep(0,p), diag(1000,p)), 
                     "sigma.sq.ig" = c(3,2),
                     "tau.sq.ig" = c(2,2), 
                     "phi.Unif" = c(0.001,1))
tuning[[25]] <- list("sigma.sq" = .05, "tau.sq" = .05, "phi" = .01)
cov.model[25] <- 'exponential'

