rm(list=ls (all=TRUE))
library("AER", lib.loc="/home/agraj/R/x86_64-pc-linux-gnu-library/2.14")
library("gmm", lib.loc="/home/agraj/R/x86_64-pc-linux-gnu-library/2.14")
library("boot", lib.loc="/usr/lib/R/library")

beta <- 1
n <- 100   #sample size
B <- n
X <- rnorm(n, mean = 0, sd = 1)
e <- rnorm(n, mean = 0, sd = 1)
Y <- beta*X + e
ols <- lm(Y ~ X)
summary(ols)
beta_hat <- coef(summary(ols))[2]
e_hat <- residuals(ols)
I <- 1:n

I_samp <- sample(I, n, replace = TRUE, prob = NULL)
X_star <- X[I_samp]
ehat_star <- e[I_samp]
Y_star <- beta*X_star + ehat_star
olsBS1 <- lm(Y_star ~ X_star)

# Non Parametric Bootstrap
Y_starNP <- Y[I_samp]
olsBS2 <- lm(Y_starNP ~ X_star)
summary(olsBS1)
summary(olsBS2)


