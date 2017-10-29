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
#############################################################
#############################################################
m<-5
x0<-seq(1,by=0, length.out = length(x1))
X.train <- matrix(c(x0,x1,x2), ncol = 3,  byrow = FALSE)
y.train <- replace(y, y == -1, 0)
W0 <- c(-1.87 , -7.05,  -0.51 ,  3.81, -13.21,  -0.20,  -1.05 ,  3.97, -10.20,  -2.19,  -4.32,   0.73,  8.96,  -5.52,  -8.65,   4.11,  -0.93, 11.45, 12.12,  0.90,-13.30)
eta <- c(seq(0.1, by=0, length.out = length(W0)))
c<-0.75
lambda <- 2
K<-50
alpha <- 0.2
check <- c(seq(0.3, by=0, length.out = length(W0)))
tol <- 0.0001
iter  <- 1
#############################################################
w.mat.iter<-W0  
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
w.next <- W0
W.bar <- 0*W0
for(iter in 1:K)    #SGD
  {
  pick<-sample(1:length(y.train), 1, replace = TRUE)
  w.cur <- w.next
  d.k <- grad(fx, x=X.train[pick,], y=y.train[pick],W =w.cur) # direction of steepest descent
  w.next <- w.cur - (c/(lambda*iter))*eta*d.k
  if(iter>(1-alpha)*K){
    W.bar <- W.bar + w.cur
  }
  W.bar <- W.bar/(alpha*K)
  
  #w.next.vec <-matrix(w.next, ncol = ((dim(X.train)[2]*m)+m+1))
  #w.mat.iter <- rbind(w.mat.iter,w.next.vec)
  #check <- (1 - ((w.next)/l.logit(w.cur)))
  # w.dum <- w.cur
}

###############################################################
############################PLOTTING###########################
w.opt.data <- w.next
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

# Stop the clock
proc.time() - ptm

