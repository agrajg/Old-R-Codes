#Problem 2 code
rm(list=ls (all=TRUE))
p<-0.6
n<-15
reps <- 10000
xmean <-seq(-1,-1, length.out = reps)
for (i in 1:reps ) {
  x<-rbinom(n,1,p)
  xmean[i] <- mean(x)
}
#(b) histogram
hist(xmean)

#(c) mean and variance of sample mean
empericalxmean <- mean(xmean)
empericalxvariance <-  var(xmean)
#YES, It dor agree with our answer in (a)
#YES, the distribution does look like a normal. If the bins of the histogram were
#smaller we could have observerved a more Normal like Binomial distribution.
#This is because a binomial distibution sample asymptotically follows a Normal Distribution

#(d) Mean squared error
P <- seq(0.6, 0.6, length.out = reps)
sqerror <- (xmean-P)^2
meansqerror <- mean(sqerror)
