#Problem 1 code
rm(list=ls (all=TRUE))
#CDF

psi <- seq(0, 0.25, by=0.005)
cdf <- (1- (sqrt(1-4*psi)))
pdf <- 2/(sqrt(1-4*psi))
plot(psi,pdf, xlab="psi", ylab="PDF",main="Density of psi", col="black", type="l")
#lines(psi, cdf, col="black", type="l")
