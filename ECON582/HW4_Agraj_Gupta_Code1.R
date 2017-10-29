rm(list=ls (all=TRUE))
u0 <- matrix(c(1,0,0), nrow=3, ncol=1, byrow = TRUE)   
u <- matrix(c(0,0,0), nrow=3, ncol=1, byrow = TRUE) 
A <- matrix(c(0.5, 0, 0, 0.1,0.1,0.3,0,0.2,0.3), nrow=3, ncol=3, byrow = TRUE) 
Y <- matrix(
Y0 <- u0
T <- 100
for(i in 1:T)
{
  for(j in 1:3)
  {
    Y[,i]<- A%*%Y[,i-1] + u
  }
  
}