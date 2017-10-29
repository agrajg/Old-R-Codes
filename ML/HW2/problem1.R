rm(list = ls())
setwd("F:/Agraj Gupta Documents/STAT535/HW2")
library("MASS", lib.loc="~/R/R-3.2.2/library")
library("class", lib.loc="~/R/R-3.2.2/library")
library("stats", lib.loc="~/R/R-3.2.2/library")
############################################################################


N.tilda <- 1000
B<-100
N<-100
PrY.plus1 <- 0.5
muX.givenYplus1 <- matrix(c(1.6, 0), nrow=2) 
muX.givenYminus1 <- matrix(c(-1.6, 0), nrow=2)
sigma <- matrix(c(1,0,0,1), nrow=2, byrow=TRUE)


K.seq <- seq(1, 41, by = 2)


Kdummy <- 1
lhat.K<- matrix(data=NA,nrow=length(K.seq),ncol=1)
L.K <- matrix(data=NA,nrow=length(K.seq),ncol=1)
V.K <- matrix(data=NA,nrow=length(K.seq),ncol=1)
lhat.std <- matrix(data=NA,nrow=length(K.seq),ncol=1)
Lhat.std <- matrix(data=NA,nrow=length(K.seq),ncol=1)

for(K in K.seq)
  {
  y <- rbinom(N.tilda, 1, 0.5)
  ############################################################################
  Y<-matrix(data=NA,nrow=N.tilda,ncol=1)
  X_givenY<-matrix(data=NA,nrow=N.tilda,ncol=2)
  ############################################################################
  for(k in 1:N.tilda)
  {
    if(y[k] == 1)
    {
      Y[k,1] = 1
      X_givenY[k,] <- mvrnorm(n = 1, muX.givenYplus1, sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
    }
    else
    {
      Y[k,1] = -1
      X_givenY[k,] <- mvrnorm(n = 1, muX.givenYminus1, sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
    }      
  }
  x1<-X_givenY[,1]
  x2<-X_givenY[,2]
  D.tilda <- data.frame(Y, x1, x2)  
  ##############################################################################
  Lhat <- matrix(data=NA,nrow=B,ncol=1)
  lhat <- matrix(data=NA,nrow=B,ncol=1)
  BigY <- matrix(data=NA,nrow=N.tilda,ncol=B)
  bigY <- matrix(data=NA,nrow=N.tilda,ncol=B)
  ##############################################################################
  for(b in 1:B)
  {
    y.b <- rbinom(N, 1, 0.5)
    ############################################################################
    Y.b<-matrix(data=NA,nrow=N,ncol=1)
    X_givenY.b<-matrix(data=NA,nrow=N,ncol=2)
    
    ############################################################################
    for(k in 1:N)
    {
      if(y.b[k] == 1)
      {
          Y.b[k,1] = 1
        X_givenY.b[k,] <- mvrnorm(n = 1, muX.givenYplus1, sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
        
      }
      else
      {
        Y.b[k,1] = -1
        X_givenY.b[k,] <- mvrnorm(n = 1, muX.givenYminus1, sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
      }      
    }
    x1.b<-X_givenY.b[,1]
    x2.b<-X_givenY.b[,2]
    D.b <- data.frame(Y.b, x1.b, x2.b)
    #paste ('D',b, sep = "", collapse = NULL) <- data.frame(Y.b, x1.b, x2.b) 
  
    train <- cbind(D.b$x1, D.b$x2)
    test <- cbind(D.tilda$x1,D.tilda$x2)
    cl <- D.b$Y.b
    Yhat.b <- knn(train, test, cl, k = K, l = 0, prob = FALSE, use.all = TRUE)
    attributes(.Last.value)
    lhat.b <- (1/N)*sum(D.b$Y != knn(train, train, cl, k = K, l = 0, prob = FALSE, use.all = TRUE))
    Lhat.b <- (1/N.tilda)*sum(D.tilda$Y != knn(train, test, cl, k = K, l = 0, prob = FALSE, use.all = TRUE))
    lhat[b,1] <- lhat.b
    Lhat[b,1] <- Lhat.b
    bigY[,b] <- Yhat.b
  }
  
  #(b)
  L <- mean(Lhat)       # Average of Expected loss
  var.L <-var(Lhat)     # variance of Expected loss
  
  #(c)
  BigY <- replace(bigY, bigY == 1, -1)
  BigY <- replace(BigY, BigY == 2,  1)

  #(empirical) probability that point ~xi is labeled +.
  p.i <- (rowSums(BigY)/(2*B))+0.5

  #(empirical) variance of the labeling of i, i.e. the averaged variance of f(~xi)
  V <- (1/N.tilda)*sum(p.i - p.i*p.i)
  #(d)
  
  lhat.mean <- mean(lhat)
  
    
  #(e)
  
  
  lhat.K[Kdummy,1] <- lhat.mean 
  V.K[Kdummy,1] <- V
  L.K[Kdummy,1] <- L
  
  lhat.std[Kdummy,1]<-sqrt(var(lhat))  
  Lhat.std[Kdummy,1]<-sqrt(var(Lhat)) 
  Kdummy <- Kdummy+1
}


lhat.l = lhat.K - lhat.std
lhat.u = lhat.K + lhat.std

Lhat.l = L.K - Lhat.std
Lhat.u = L.K + Lhat.std

#Plot

xrange <- range(K.seq)
yrange <- c(min(c(min(lhat.K),min(V.K), min(L.K), min(lhat.l),min(lhat.u),min(Lhat.l),min(Lhat.u))),max(c(max(lhat.K),max(V.K), max(L.K),max(lhat.l),max(lhat.u),max(Lhat.l),max(Lhat.u))))
ntrees = 3;
# set up the plot 
plot(xrange ,yrange, type="n", xlab="K",     ylab="L ;l_hat ; V" ) 
colors <- c("black", "black", "gray")
linetype <- c(1:ntrees) 
plotchar <- seq(18,18+ntrees,1)

lines(K.seq,lhat.K, type="o", lwd=2.5,      lty=linetype[1], col=colors[1], pch=plotchar[1]) 
lines(K.seq,lhat.l, type="b", lwd=1.5,      lty=linetype[3], col=colors[1], pch=plotchar[1]) 
lines(K.seq,lhat.u, type="b", lwd=1.5,      lty=linetype[3], col=colors[1], pch=plotchar[1]) 
lines(K.seq, V.K, type="l", lwd=2.5,      lty=linetype[2], col=colors[2], pch=plotchar[2])
lines(K.seq, L.K, type="o", lwd=2.5,      lty=linetype[1], col=colors[3], pch=plotchar[3])
lines(K.seq, Lhat.l, type="b", lwd=1.5,      lty=linetype[3], col=colors[3], pch=plotchar[3])
lines(K.seq, Lhat.u, type="b", lwd=1.5,      lty=linetype[3], col=colors[3], pch=plotchar[3])

# add a title and subtitle 
title("Average and variance of the expected losses" )

# add a legend 
legend("bottomright", c("l_hat","V","L"), title = "l_hat/V/L",  lwd=c(2.5,2.5,2.5),lty=c(linetype[1],linetype[2],linetype[1]), col=colors[1:3], pch=plotchar[1:3])
