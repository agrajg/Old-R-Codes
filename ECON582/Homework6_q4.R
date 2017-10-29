#Problem 4 code
rm(list=ls (all=TRUE))
mu0 <- 10 #mean of prior distribution
tau20 <- 0.01 #precision of prior distribution
# Prior for precision phi2
nu0 <- 1
phi20 <- 1

######
par(mfrow=c(1,2))
plot(seq(-30,50,by=0.1),(dnorm(seq(-30,50,by=0.1),mu0,1/sqrt(tau20))), type="l",xlab="mu",ylab="Density",main="Density for prior on mu")
plot(seq(0,12,by=0.01),(dgamma(seq(0,12,by=0.01),nu0/2,nu0/(2*phi20))), type="l",xlab="phi^2",ylab="Density",main="Density for prior on precision phi^2")

### Discrete approximation ###
### Data: 9 observations
y <- c(1.76, 5.32, 6.14, 3.19, 4.90, 4.69, 5.08, 4.68, 3.82)

G <- 100
H <- 100
mu.grid <- seq(0,9,length=G)  
phi2.grid <- seq(0.01,2.0,length=H)
post.grid <- matrix(nrow=G,ncol=H)  #to store posterior
for(g in 1:G){
  for(h in 1:H){
    post.grid[g,h] <-
      dnorm(mu.grid[g],mu0,1/sqrt(tau20))*
      dgamma(phi2.grid[h],nu0/2,nu0/(2*phi20))*
      prod(dnorm(y,mu.grid[g],1/sqrt(phi2.grid[h])))
  }
}

post.grid <- post.grid / sum(post.grid)  # normalizing

par(mfrow=c(1,1))
# Image of joint posterior
image(mu.grid,phi2.grid,post.grid,main="Heat map for Posterior on (mu,phi^2)",xlab="mu",ylab="phi^2")

# Compute marginal posterior for mu
post.mu <- apply(post.grid,1,sum)
plot(mu.grid,post.mu,type="l",main="Marginal posterior for mu",xlab="mu",ylab="Density")

# Compute marginal posterior for phi2
post.phi2 <- apply(post.grid,2,sum)

plot(phi2.grid,post.phi2,type="l",main="Marginal posterior for phi2",xlab="phi2",ylab="Density")

### All three plots:
par(mfrow=c(1,3))
image(mu.grid,phi2.grid,post.grid,main="Heat map for Posterior on (mu,phi^2)",xlab="mu",ylab="phi^2")

plot(mu.grid,post.mu,type="l",main="Marginal posterior for mu",xlab="mu",ylab="Density")

plot(phi2.grid,post.phi2,type="l",main="Marginal posterior for phi^2",xlab="phi^2",ylab="Density")

### GIBBS SAMPLING ###

mean.y <- mean(y)
var.y <- var(y)
n <- length(y)

# Specify chain length
S <- 2000  # generate 1000 dependent samples
samples <- matrix(nrow=S,ncol=2) # to store samples
# col 1 is mu samples
# col 2 is phi^2 samples

### Starting values
samples[1,] <- c(mean.y,1/var.y)

#alternative (less good) starting values
# to show burn in:
# samples[1,] <- c(0,0.6)

set.seed(1) #to ensure random numbers are always the same
for(s in 2:S){
  
  ### Generate a new value of mu from f(mu | prev value of phi2, data)
  prev.phi2 <- samples[s-1,2] 
  mustar <- (mu0*tau20+n*mean.y*prev.phi2)/(tau20 +n*prev.phi2)
  phi2star <- tau20 +n*prev.phi2
  mu.new <- rnorm(1,mustar,1/sqrt(phi2star))
  
  ### Generate a new value of phi2 from f(phi2 | new value of mu, data)
  astar <- (nu0 + n)/2
  bstar <- (nu0/phi20 + (n-1)*var.y + n*(mean.y-mu.new)^2)/2
  phi2.new <- rgamma(1,astar,bstar)
  samples[s,] <- c(mu.new,phi2.new)
}

par(mfrow=c(1,1))
image(mu.grid,phi2.grid,post.grid,main="Heat map for Posterior on (mu,phi^2)", xlab="mu",ylab="phi^2")
points(samples[,1],samples[,2],pch="+")


###Quantiles
quantile(samples[,1],c(0.025,0.5,0.975)) # For mu
quantile(samples[,2],c(0.025,0.5,0.975)) # For phi^2
quantile(1/sqrt(samples[,2]),c(0.025,0.5,0.975)) # Quantiles for the population sd
#CODE ENDS HERE

> ###Quantiles
  > quantile(samples[,1],c(0.025,0.5,0.975)) # For mu
2.5%      50%    97.5% 
3.440927 4.426706 5.398341 

> quantile(samples[,2],c(0.025,0.5,0.975)) # For phi^2
2.5%       50%     97.5% 
0.1863012 0.5640539 1.3149070 

> quantile(1/sqrt(samples[,2]),c(0.025,0.5,0.975)) # Quantiles for the population sd
2.5%       50%     97.5% 
0.8720723 1.3314955 2.3168198 



