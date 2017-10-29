rm(list=ls (all=TRUE))
n<-1000
x <- 1:n
normalsample <- sort(rnorm(n, mean = 0, sd = 1))
ecdfnormal <- ecdf(normalsample)
plot(ecdfnormal, col="black", xlab="X", ylab="F(x)", main="ECDF and Standard Normal")
curve(pnorm, add=TRUE, col="grey")
legend(x=0,y=0.4,legend=c("empirical CDF","Standard Normal CDF"),lty=c(1.1),col=c("black","grey"))
