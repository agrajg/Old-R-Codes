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
Nreps <- 1000

thetan_hat<-mat.or.vec(Nreps, 1)
thetanb_hat_bar <- mat.or.vec(Nreps, 1)
taun_hat <- mat.or.vec(Nreps, 1)
theta_tilda <- mat.or.vec(Nreps, 1)

for (reps in 1:Nreps)
{
  
  x<- rnorm(n, mean = mu_x, sd = sigmasq_x^0.5)
  thetan_hat[reps] <- exp(mean(x))
  x_resamp <- mat.or.vec(B, n)
  thetanb_hat <- mat.or.vec(B, 1)
  ###############################################################
  for(b in 1:B)
  {
    x_resamp[b,] <- sample(x, n, replace = TRUE, prob = NULL)
    thetanb_hat[b] <- exp(mean(x_resamp[b,]))
  }
  thetanb_hat_bar[reps] <- mean(thetanb_hat)
  taun_hat[reps] <- thetanb_hat_bar[reps] - thetan_hat[reps]
  ## Bias Correction 
  theta_tilda[reps] <- thetan_hat[reps] - taun_hat[reps]
  ################################################################
  #repeating 1000 times
}  



# Original Bias
o_bias <- mean(thetan_hat - theta0)
o_variance <- var(thetan_hat)
o_MSE <- mean((thetan_hat - theta0)^2)
################################################################
# Bootstrap Bias
bs_bias <- mean(theta_tilda - theta0)
bs_variance <- var(theta_tilda)
bs_MSE <- mean((theta_tilda - theta0)^2)


  
 

