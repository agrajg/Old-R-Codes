#Problem 7 code
rm(list=ls (all=TRUE))

anse <- 1-pbeta(0.5,8,14,lower.tail=TRUE)
ansf <- qbeta(0.5,8,14)
ansg1 <- qbeta(0.025,8,14, lower.tail=TRUE)
ansg2 <- qbeta(0.025,8,14, lower.tail=FALSE)
