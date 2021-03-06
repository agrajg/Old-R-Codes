rm(list = ls())
setwd("F:/Agraj Gupta Documents/STAT535/HW4")
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
 W0 <- c(-2.0493124, -7.3832490,  0.8006477,  7.2705676, -9.0331851, -2.4714033,  1.7562234 , 1.9431037 ,-6.8893178, -3.5737563 ,-5.1748239, 0.4774065,  6.2061998, -4.3559198, -7.8843287, -2.4843127, -1.3428676,  7.4322973,  7.9670852,  1.4805731 ,-0.1690451)
# W0 <- c(-1.87 , -7.05,  -0.51 ,  3.81, -13.21,  -0.20,  -1.05 ,  3.97, -10.20,  -2.19,  -4.32,   0.73,  8.96,  -5.52,  -8.65,   4.11,  -0.93, 11.45, 12.12,  0.90,-13.30)
########INPUTS##############################################
K <- 1000
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
iter <- 0
W.MAT <- matrix(data = NA, ncol = length(W0))
d.k.mat <-  matrix(data = NA, ncol = length(W0))
# while(check > tol)

for(k in 1:K)
{
  # 1(a)
  pick<-sample(1:length(y.train), 1, replace = TRUE)
  # 1(b)
  d.k <- grad(fx, x=X.train[pick,], y=y.train[pick],W =w.cur) # direction of steepest descent
  d.k.mat <- rbind(d.k.mat,d.k)
  # 2. update ??
  w.next <- w.cur + (c/(lambda*K))*d.k
  
  # 3.  
  if(k > (1-alpha)*K){w.bar <- w.bar + w.next}
  W.MAT <- rbind(W.MAT, w.bar)
}
W.MAT <- W.MAT[-1,]
d.k.mat <- d.k.mat[-1,]

write.table(W.MAT, "WmatN1_100.txt", sep="\t")

############################PLOTTING###########################
w.opt.data <- w.bar
#write.table(w.mat.iter, "w_mat_data1.txt", sep="\t")
w1.opt <- matrix(w.next[1:(dim(X.train)[2]*m)], ncol=m, byrow=FALSE)
w2.opt <- matrix(w.next[((dim(X.train)[2]*m)+1):((dim(X.train)[2]*m)+m+1)], nrow=(m+1)) 
my.fun <- function(x1.plot,x2.plot)
{
  x0.plot <-x0
  x.plot <- matrix(cbind(x0.plot,x1.plot, x2.plot), ncol = 3)
  X.w1.plot <- (sigmoid((x.plot%*%w1.opt), a = 1, b = 0))
  X.w1.plot <- cbind(x0,X.w1.plot)
  z  <- sigmoid((X.w1.plot%*%w2.opt),a=1,b=0)-0.5
}
x1.plot <-seq(0,1,length=100)
x2.plot <-seq(0,1,length=100)
z<-outer(x1.plot,x2.plot,my.fun)
contour(x1.plot,x2.plot,z,level=0,  main = "Scatter plot of x1 vs x2 for different classes in y and Decision boundary", xlab ="x1", ylab="x2")
points(x1[y.train==1], x2[y.train==1], pch=1, col = "grey")
points(x1[y.train==0], x2[y.train==0], pch=1, col = 4)
legend(x=0.9, y=1, legend = c("+1", "-1"), fill=c("grey",4))
#############################################################




