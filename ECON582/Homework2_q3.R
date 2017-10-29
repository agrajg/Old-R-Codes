rm(list=ls (all=TRUE))
k<-8
poissionpop = rpois(10^k, 2)
binomialpop = rbinom(10^k, 80, 0.2)

simulation <- function(population,k)
  {
    expt = matrix(nrow=k, ncol=3)
    for(i in 1:k)
    {
      samp <- sample(population,(10^i),replace = FALSE, prob = NULL)
      expt[i,1] <- 10^i
      expt[i,2] <- mean(samp)
      expt[i,3] <- var(samp)
    }
    return (expt)
  }
poisexpt=simulation(poissionpop,k)
binoexpt=simulation(binomialpop,k)
