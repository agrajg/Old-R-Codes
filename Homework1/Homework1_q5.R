rm(list=ls (all=TRUE))
funcm <- function(n,k) #function that return the value of m such that atleast on in sample has disease
  {
    x <- 1            #initialising x as it will contain multiplicative  values
    ppl = matrix(nrow=(n-k-1), ncol=1)
    dpr = matrix(nrow=(n-k-1), ncol=1)  #vectors to store random variable values and thier probability
    for (j in 0:(n-k))
    {
      x=x*((n-k-j)/(n-j))              #using the rule from the part a 
      ppl[j+1] = j+1
      
      dpr[j+1] = 1-x                    #feeding value to probability calculated using the rule.
      if(dpr[j+1]>0.75)
      {
        m <- ppl[j+1]                    #checking for right m
        break
      }
    }
    return(m)
  }

mc <- funcm(10000,100)             # when n=10000 and k= 100
mb <- funcm(1000,10)               # when n=1000 and k=10

