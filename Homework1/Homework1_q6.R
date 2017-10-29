# code for problem 6
rm(list=ls (all=TRUE))
k <- 5

comb1 <- t(matrix(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), nrow=5, ncol=32,byrow = TRUE) )

rv = matrix(nrow=2^k, ncol=1)
pr = matrix(nrow=2^k, ncol=1)
sortrv = matrix(nrow=2^k, ncol=1)
cdfrangelb = matrix(nrow=2^k, ncol=1)
cumprob = matrix(nrow=2^k, ncol=1)
cdfrangeub = matrix(nrow=2^k, ncol=1)
alpha <- 0 
pralpha <- 0



for (i in 1:2^k)
{
  x<-0
  for (j in 1:k)
  {
    x <- x+(comb1[i,j]*2/(3^j))    #calculating random variables for each probability
  }
  rv[i] <- x
  pr[i] <- 1/(2^k)                #probability will be same for each instance
  pralpha <- pralpha + pr[i]      #cumulating the probability
  cumprob[i] <- pralpha           #storing it to an new vector
  }
sortrv <- sort(rv)                #sorting the vector from smallest to largest
plot(rv,pr)
for (count in 1:2^k)
{
  cdfrangelb[count] <- sortrv[count]
  cdfrangeub[count] <- sortrv[count+1]
}
pmf <-cbind(pr, sortrv)                 # arranging probability and random variable for pnf
cdf <- rbind(0,cbind(cumprob, cdfrangelb, cdfrangeub))  #arranging cumulative probability and their
                                                        #lower and upper bound
plot.ecdf(sortrv)