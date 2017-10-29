rm(list=ls (all=TRUE))
pr <- sort(runif(1000, min=0, max=1))
X <- function(pr) {log((pr/(1 - pr)), base = exp(1))}
ecdflogistic <- ecdf(log((pr/(1 - pr)), base = exp(1)))
plot(X(pr),pr,type='l', col="black",  main = "ECDF and Logistic",ylim=c(0,1), xlim=c(-6.0,6.0))
plot(ecdflogistic,add=TRUE, col="grey")
legend(x=2,y=0.4,legend=c("empirical CDF","Logistic CDF"),lty=c(1.1),col=c("grey","black"))
