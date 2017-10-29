rm(list = ls())
setwd("F:/Agraj Gupta Documents/STAT535/HW3")
library("MASS", lib.loc="~/R/R-3.2.2/library")
library("class", lib.loc="~/R/R-3.2.2/library")
library("stats", lib.loc="~/R/R-3.2.2/library")
library("nnet", lib.loc="~/R/R-3.2.2/library")
library("pracma", lib.loc="~/R/R-3.2.2/library")
library("reshape", lib.loc="~/R/R-3.2.2/library")
# Start the clock!
ptm <- proc.time()
#################### DATA ###################################
data <- read.table("hw3-nn-train-100.dat", quote="\"", comment.char="")
data <- rename(data, c(V3="y"))
data <- rename(data, c(V2="x2"))
data <- rename(data, c(V1="x1"))
attach(data)
class(data)
#############################################################
m<-5
x0<-seq(1,by=0, length.out = length(x1))
X.train <- matrix(c(x0,x1,x2), ncol = 3,  byrow = FALSE)
y.train <- replace(y, y == -1, 0)
# W1 <- matrix(runif((dim(X.train)[2]*(m)), min =-10, max = 10), ncol=(m))
# W2 <- matrix(runif((m+1), min = -10, max = 10), nrow=(m+1))
# W0 <- c(W1,W2)
# W0 <- c(-2.0493124, -7.3832490,  0.8006477,  7.2705676, -9.0331851, -2.4714033,  1.7562234 , 1.9431037 ,-6.8893178, -3.5737563 ,-5.1748239, 0.4774065,  6.2061998, -4.3559198, -7.8843287, -2.4843127, -1.3428676,  7.4322973,  7.9670852,  1.4805731 ,-0.1690451)
W0 <- c(-1.87 , -7.05,  -0.51 ,  3.81, -13.21,  -0.20,  -1.05 ,  3.97, -10.20,  -2.19,  -4.32,   0.73,  8.96,  -5.52,  -8.65,   4.11,  -0.93, 11.45, 12.12,  0.90,-13.30)
########INPUTS##############################################
K <- 50
c <- 0.75
lambda <- 0.3
alpha <-0.2

tol <- 0.0001


##############FUNCTIONS######################################
fx <- function(x,y,W) # x one observation
{
  w1 <- matrix(W[1:(length(x)*(m))], ncol=(m), byrow=FALSE)
  w2 <- matrix(W[((length(x)*m)+1):((length(x)*m)+m+1)], nrow=(m+1))  
  x.w1 <- (sigmoid((x%*%w1), a = 1, b = 0))
  x.w1 <- cbind(x0[pick],x.w1)
  f  <- sigmoid((x.w1%*%w2),a=1,b=0)
  return(f)
}
#############################################################
w.cur <- W0 
w.bar <- W0
iter <- 1

#while(check > tol)
  
  for(k in 1:K)
  {
    # 1(a)
    pick<-sample(1:length(y.train), 1, replace = TRUE)
    # 1(b)
    d.k <- grad(fx, x=X.train[pick,], y=y.train[pick],W =w.cur) # direction of steepest descent
    # 2. update ??
    w.next <- w.cur + (c/(lambda*K))*d.k
    # 3.  
    if(k > (1-alpha)*K){w.bar <- w.bar + w.next}
  }






