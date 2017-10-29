######################################################################
rm(list=ls (all=TRUE))
library("AER", lib.loc="/home/agraj/R/x86_64-pc-linux-gnu-library/2.14")
library("gmm", lib.loc="/home/agraj/R/x86_64-pc-linux-gnu-library/2.14")
library("boot", lib.loc="/usr/lib/R/library")

setwd("/home/agraj/Documents/R_ECON582") 
hprice1 <- read.table('hprice1.csv',header=TRUE,sep=",") 
attach(hprice1)
ols <- lm(Price ~ Bedrooms + Lot + Size + Colonial)
summary(ols)

# 95% CI for estimated coefficients
CI = mat.or.vec(4,2)
# CI for bedrooms coefficient
CI[1,1] <- coef(summary(ols))[2] - sqrt(diag(vcov(ols)))[2]*qnorm(0.975)
CI[1,2] <- coef(summary(ols))[2] - sqrt(diag(vcov(ols)))[2]*qnorm(0.025)

# CI for lot size coefficient
CI[2,1] <- coef(summary(ols))[3] - sqrt(diag(vcov(ols)))[3]*qnorm(0.975)
CI[2,2] <- coef(summary(ols))[3] - sqrt(diag(vcov(ols)))[3]*qnorm(0.025)

# CI for style coefficient
CI[3,1] <- coef(summary(ols))[4] - sqrt(diag(vcov(ols)))[4]*qnorm(0.975)
CI[3,2] <- coef(summary(ols))[4] - sqrt(diag(vcov(ols)))[4]*qnorm(0.025)

# CI for house size coefficient
CI[4,1] <- coef(summary(ols))[5] - sqrt(diag(vcov(ols)))[5]*qnorm(0.975)
CI[4,2] <- coef(summary(ols))[5] - sqrt(diag(vcov(ols)))[5]*qnorm(0.025)


############################################################
##############Bootstrapping#################################
# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}


# bootstrapping with 1000 replications
results <- boot(data=hprice1, statistic=bs,
                R=1000, formula=Price ~ Bedrooms + Lot + Size + Colonial)


# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # bedrooms
boot.ci(results, type="bca", index=3) # lot 
boot.ci(results, type="bca", index=4) # size
boot.ci(results, type="bca", index=5) # colonial 