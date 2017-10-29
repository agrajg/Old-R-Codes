rm(list=ls (all=TRUE))
library("mnormt")
library("MASS", lib.loc="/usr/lib/R/library")
library("graphics")
###############################################################
n<-10
mu_x <- 0
sigmasq_x <- 6
theta0 <- exp(mu_x)
B <- 100



x<- rnorm(n, mean = mu_x, sd = sigmasq_x^0.5)
thetan_hat <- exp(mean(x))
x_resamp <- mat.or.vec(B, n)
thetanb_hat <- mat.or.vec(B, 1)
###############################################################
for(b in 1:B)
{
  x_resamp[b,] <- sample(x, n, replace = TRUE, prob = NULL)
  thetanb_hat[b] <- mean(x_resamp[b,])
}
thetanb_hat_bar <- mean(thetanb_hat)
taun_hat <- thetanb_hat_bar - thetanb_hat
## Bias Correction 
theta_tilda <- thetan_hat - taun_hat
################################################################
#repeating 1000 times


# Original Bias
o_bias <- mean(thetan_hat - theta0)
o_variance <- var(thetan_hat)
o_MSE <- mean((thetan_hat - theta0)^2)
################################################################
# Bootstrap Bias
o_bias <- mean(theta_tilda - theta0)
o_variance <- var(theta_tilda)
o_MSE <- mean((theta_tilda - theta0)^2)
