rm(list=ls (all=TRUE))
reps <- 10000
n <- 50
p <- 0.1
CIl <- seq(-1,-1, length.out = reps)
CIu <- seq(-1,-1, length.out = reps)
count <-0


for(i in 1:reps) {
  seq <- rbinom(n, 1, p)
  Xbar <- mean(seq)
  CIl[i] <- Xbar - 1.96*sqrt(Xbar*(1-Xbar)/n)
  CIu[i] <- Xbar + 1.96*sqrt(Xbar*(1-Xbar)/n)
if (p>CIl[i] && p<CIu[i]) {
  count <- count+1 
}
} 
fraction <- count/reps