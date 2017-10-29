rm(list = ls())
setwd("F:/Agraj Gupta Documents/STAT535/HW3")
library("MASS", lib.loc="~/R/R-3.2.2/library")
library("class", lib.loc="~/R/R-3.2.2/library")
library("stats", lib.loc="~/R/R-3.2.2/library")
library("nnet", lib.loc="~/R/R-3.2.2/library")
library("pracma", lib.loc="~/R/R-3.2.2/library")
library("reshape", lib.loc="~/R/R-3.2.2/library")
# Start the clock!
ptm <- proc.time()

N <- 1000
K <- 2
mu <- matrix(c(0,0), nrow = K, ncol = 1, byrow = FALSE)
Sigma <- matrix(c(1,0.5,0.5,1),  nrow = K, ncol = K, byrow = FALSE)
X <- mvrnorm(n = N, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
x0 <-  matrix(seq(1:1, by=0),  nrow = N, ncol = 1, byrow = FALSE)
X <- cbind(x0,X)

##############FUNCTIONS######################################
fx <- function(x,y,W) # x one observation
{
  w1 <- matrix(W[1:(length(x)*(m))], ncol=(m), byrow=FALSE)
  w2 <- matrix(W[((length(x)*m)+1):((length(x)*m)+m+1)], nrow=(m+1))  
  x.w1 <- (sigmoid((x%*%w1), a = 1, b = 0))
  x.w1 <- cbind(x0[pick],x.w1)
  f  <- sigmoid((x.w1%*%w2),a=1,b=0)
  return(f)
}

#############################################################



hessian(func, x, method="Richardson", method.args=list(), ...)
