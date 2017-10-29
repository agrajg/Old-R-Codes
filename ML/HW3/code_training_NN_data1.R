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

#################### DATA #############################
data <- read.table("hw3-nn-train-100.dat", quote="\"", comment.char="")
data <- rename(data, c(V3="y"))
data <- rename(data, c(V2="x2"))
data <- rename(data, c(V1="x1"))
attach(data)
class(data)
# plot(x1,x2, type = "n", main = "Scatter plot of x1 vs x2 for different classes in y", xlab ="x1", ylab="x2")
# points(x1[y==1], x2[y==1], pch=16, col = 2)
# points(x1[y==-1], x2[y==-1], pch=1, col = 4)
# legend(x=0.9, y=1, legend = c("+1", "-1"), fill=c(2,4))
#######################################################


################## Parameters #########################
m<-5
x0<-seq(1,by=0, length.out = length(x1))
X <- matrix(c(x0,x1,x2), ncol = 3,  byrow = FALSE)
y <- replace(y, y == -1, 0)
# W1 <- matrix(runif((dim(X)[2]*(m)), min =-10, max = 10), ncol=(m))
# W2 <- matrix(runif((m+1), min = -10, max = 10), nrow=(m+1))
# W0 <- c(W1,W2)
# W0 <- c(-2.0493124, -7.3832490,  0.8006477,  7.2705676, -9.0331851, -2.4714033,  1.7562234 , 1.9431037 ,-6.8893178, -3.5737563 ,-5.1748239, 0.4774065,  6.2061998, -4.3559198, -7.8843287, -2.4843127, -1.3428676,  7.4322973,  7.9670852,  1.4805731 ,-0.1690451)
# W0 <- c(-1.87 , -7.05,  -0.51 ,  3.81, -13.21,  -0.20,  -1.05 ,  3.97, -10.20,  -2.19,  -4.32,   0.73,  8.96,  -5.52,  -8.65,   4.11,  -0.93, 11.45, 12.12,  0.90,-13.30)

eta <- c(seq(0.1, by=0, length.out = length(W0)))
check <- c(seq(0.3, by=0, length.out = length(W0)))
tol <- 0.0001
iter  <- 1
######################################################

##############SIGMOID FUNCTION NN###############################

l.logit <- function(W) 
{
  w1 <- matrix(W[1:(dim(X)[2]*(m))], ncol=(m), byrow=FALSE)
  w2 <- matrix(W[((dim(X)[2]*m)+1):((dim(X)[2]*m)+m+1)], nrow=(m+1))  
  X.w1 <- (sigmoid((X%*%w1), a = 1, b = 0))
  X.w1 <- cbind(x0,X.w1)
  f  <- sigmoid((X.w1%*%w2),a=1,b=0)
  lg.l <- (1/length(y))*((t(y)%*%log(f) + t(1-y)%*%log(1-f)))
  return(-lg.l)
}
################################################################

d.k <- grad(l.logit, W0)     # 15 14  4        # direction of steepest descent
w.cur <- W0
w.next <- w.cur + eta*d.k 
w.mat.iter <- matrix(w.next,ncol = ((dim(X)[2]*m)+m+1), byrow="TRUE")
while(min(check) >= tol)
{
  w.cur <- w.next
  d.k <- grad(l.logit, w.cur)     # 15 14  4        # direction of steepest descent
  w.next <- w.cur - eta*d.k
  w.next.vec <-matrix(w.next, ncol = ((dim(X)[2]*m)+m+1))
  w.mat.iter <- rbind(w.mat.iter,w.next.vec)
  check <- (1 - (l.logit(w.next)/l.logit(w.cur)))
  # w.dum <- w.cur
  iter <- iter + 1
}
###############################################################
####PLOTTING####
w.opt.data2 <- w.next
write.table(w.mat.iter, "w_mat_data1.txt", sep="\t", row.names=NULL)
w1.opt <- matrix(w.next[1:(dim(X)[2]*m)], ncol=m, byrow=FALSE)
w2.opt <- matrix(w.next[((dim(X)[2]*m)+1):((dim(X)[2]*m)+m+1)], nrow=(m+1)) 
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
points(x1[y==1], x2[y==1], pch=1, col = "grey")
points(x1[y==0], x2[y==0], pch=1, col = 4)
legend(x=0.9, y=1, legend = c("+1", "-1"), fill=c("grey",4))
#############################################################
# Stop the clock
proc.time() - ptm

