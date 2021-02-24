## Diana Gerardo
## Adjusting for Multiplicity
## Linear Regressions

# clear global environment
rm(list = ls())

# read in the data
vel1000 <- read.csv('~/UCSC_capstone/DATA/Vel1000_AEQD_OutRem.csv')
head(vel1000)

# make a linear regression for all 25 time frames. 
lin.model <- lapply(X = 1:25, 
                   FUN = function(x){
                     data <- vel1000[which(vel1000$t==x),]
                     lm(V_CVI ~ V_MAT, data=data, x=T,y=T) } ) 

# 25 sets of betas
betas <- matrix(NA, nrow = 25, ncol = 2)
for(i in 1:25){ betas[i,] <- lin.model[[i]]$coefficients }
dim(betas); head(betas)

# 25 sets of p.value 
p.value <- matrix(NA, nrow = 25, ncol = 2)
for(i in 1:25){ p.value[i,] <- coef(summary(lin.model[[i]]))[, 4] }
dim(p.value); head(p.value)

# Multiple comparisons / Bonferroni correction
# The Bonferroni correction sets the significance cut-off at alpha/n
# My signif codes will now be:
# 0/25  '***'  0.001/25  '**'  0.01/25  '*'  0.05/25  '.'  0.1/25  ' '  1/25
 
bonf.b0 <- matrix(NA, nrow = 25, ncol = 1)
bonf.b1 <- matrix(NA, nrow = 25, ncol = 1)

for(i in 1:25){
  if(p.value[i,1] > (1/25) ){bonf.b0[i] <- " "}
  if(p.value[i,1] < (1/25) ){bonf.b0[i] <- " "}
  if(p.value[i,1] < (0.1/25) ){bonf.b0[i] <- "."}
  if(p.value[i,1] < 0.05/25 ){bonf.b0[i] <- "*"}
  if(p.value[i,1] < 0.01/25 ){bonf.b0[i] <- "**"}
  if(p.value[i,1] < 0.001/25 ){bonf.b0[i] <- "***"}
  
  if(p.value[i,2] > (1/25) ){bonf.b1[i] <- " "}
  if(p.value[i,2] < 1/25 ){bonf.b1[i] <- " "}
  if(p.value[i,2] < 0.1/25 ){bonf.b1[i] <- "."}
  if(p.value[i,2] < 0.05/25 ){bonf.b1[i] <- "*"}
  if(p.value[i,2] < 0.01/25 ){bonf.b1[i] <- "**"}
  if(p.value[i,2] < 0.001/25 ){bonf.b1[i] <- "***"}
}

# brief summary for the 25 regressions
bonferroni.summary <- data.frame(b0=betas[,1], b1=betas[,2], 
                         p.b0=p.value[,1], bonf.b0,
                         p.b1=p.value[,2], bonf.b1 )
dim(bonferroni.summary); head(bonferroni.summary)

bonferroni.summary[which(bonferroni.summary$bonf.b0=="*" | 
                           bonferroni.summary$bonf.b0=="**" |
                           bonferroni.summary$bonf.b0=="***"|
                           bonferroni.summary$bonf.b1=="*" | 
                           bonferroni.summary$bonf.b1=="**" |
                           bonferroni.summary$bonf.b1=="***"), ]




