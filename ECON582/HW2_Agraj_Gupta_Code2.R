rm(list=ls (all=TRUE))
library("mnormt")
library("MASS", lib.loc="/usr/lib/R/library")
library("graphics")
beta0 <- 1
####################WEAK INSTRUMENT#######################
### n=100, repetitions = 10000, pi = 0.01##################
##########################################################
n <- 100
Z = rnorm(n, mean = 0, sd = 1)
pi1 <- 0.01 # weak instrument
mu <- matrix(c(0,0),2,1)
Sigma <- matrix(c(1, 0.90,1,0.90),2,2)
reps = 10000 # number of repetitions for simulating data
beta_hat1 <- mat.or.vec(reps, 1)
for (i in 1:reps)
{
  E<- mvrnorm(n,mu,Sigma)
  X <- Z*pi1 + E[,2]
  Y <- X*beta0 + E[,1]
  ZX <-t(Z) %*% X
  ZX1 <-solve(ZX)
  ZY <-t(Z) %*% Y
  beta_hat1[i] <-ZX1 %*% ZY # Beta GMM for exactly dentified case with weak instrument
}
bias_beta_hat1 <- mean(beta_hat1)-1
hist(beta_hat1, breaks=100000 ,xlab='Beta hat', xlim = (c(-5,5)), main = 'Beta Hat with Weak Instrument(pi=0.1), n=100')


####################WEAK INSTRUMENT#######################
### n=10000, repetitions = 10000, pi = 0.01 ###############
##########################################################
n1 <- 10000
i<-0
Z = rnorm(n1, mean = 0, sd = 1)
pi2 <- 0.01 # weak instrument
mu <- matrix(c(0,0),2,1)
Sigma <- matrix(c(1, 0.90,1,0.90),2,2)
reps = 10000 # number of repetitions for simulating data
beta_hat2 <- mat.or.vec(reps, 1)
for (i in 1:reps)
{
  E<- mvrnorm(n,mu,Sigma)
  X <- Z*pi2 + E[,2]
  Y <- X*beta0 + E[,1]
  ZX <-t(Z) %*% X
  ZX1 <-solve(ZX)
  ZY <-t(Z) %*% Y
  beta_hat2[i] <-ZX1 %*% ZY # Beta GMM for exactly dentified case with weak instrument
}
bias_beta_hat2 <- mean(beta_hat2)-1
hist(beta_hat2,breaks=100000 ,xlab='Beta hat', xlim = (c(-5,5)),main = 'Beta Hat with Weak Instrument(pi=0.01), n=10000')

####################WEAK INSTRUMENT#######################
### n=100, repetitions = 10000, pi = 1 ###################
##########################################################
n <- 100
i<-0
Z = rnorm(n, mean = 0, sd = 1)
pi3 <- 1# Strong instrument
mu <- matrix(c(0,0),2,1)
Sigma <- matrix(c(1, 0.90,1,0.90),2,2)
reps = 10000 # number of repetitions for simulating data
beta_hat3 <- mat.or.vec(reps, 1)
for (i in 1:reps)
{
  E<- mvrnorm(n,mu,Sigma)
  X <- Z*pi3 + E[,2]
  Y <- X*beta0 + E[,1]
  ZX <-t(Z) %*% X
  ZX1 <-solve(ZX)
  ZY <-t(Z) %*% Y
  beta_hat3[i] <-ZX1 %*% ZY # Beta GMM for exactly dentified case with Strong instrument
}
bias_beta_hat3 <- mean(beta_hat3)-1
hist(beta_hat3, breaks=100000 ,xlab='Beta hat', xlim = (c(-5,5)),main = 'Beta Hat with Strong Instrument(pi=1), n=100')