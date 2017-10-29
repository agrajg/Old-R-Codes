rm(list = ls())
setwd("F:/Agraj Gupta Documents/STAT535/HW3")
library("MASS", lib.loc="~/R/R-3.2.2/library")
library("class", lib.loc="~/R/R-3.2.2/library")
library("stats", lib.loc="~/R/R-3.2.2/library")
library("nnet", lib.loc="~/R/R-3.2.2/library")
library("pracma", lib.loc="~/R/R-3.2.2/library")
library("reshape", lib.loc="~/R/R-3.2.2/library")



################## Parameters #########################
m<-5
# x0<-seq(1,by=0, length.out = length(x1))
# X <- matrix(c(x0,x1,x2), ncol = 3,  byrow = FALSE)
# y <- replace(y, y == -1, 0)
# W1 <- matrix(runif((dim(X)[2]*(m)), min =-10, max = 10), ncol=(m))
# W2 <- matrix(runif((m+1), min = -10, max = 10), nrow=(m+1))
# W0 <- c(W1,W2)
W0 <- c(-2.0493124, -7.3832490,  0.8006477,  7.2705676, -9.0331851, -2.4714033,  1.7562234 , 1.9431037 ,-6.8893178, -3.5737563 ,-5.1748239, 0.4774065,  6.2061998, -4.3559198, -7.8843287, -2.4843127, -1.3428676,  7.4322973,  7.9670852,  1.4805731 ,-0.1690451)

eta <- c(seq(0.1, by=0, length.out = length(W0)))
check <- c(seq(0.3, by=0, length.out = length(W0)))
tol <- 0.0001
iter  <- 1
######################################################



#################### DATA #############################
data <- read.table("hw3-nn-test.dat", quote="\"", comment.char="")
data <- rename(data, c(V3="y.test"))
data <- rename(data, c(V2="x2.test"))
data <- rename(data, c(V1="x1.test"))
attach(data)
class(data)
X<- cbind(x1.test,x2.test)


w.data2 <- read.table("w_mat_data1.txt", quote="\"", comment.char="")
w.mat.iter<- data.matrix(w.data2, rownames.force = NA)

l.01 <- matrix(data=NA, nrow=dim(w.mat.iter))
Loss.logit <- matrix(data=NA, nrow=dim(w.mat.iter))
# cnt<-10
for(cnt in 1:dim(w.mat.iter)[1]){
  
  x0.test <-seq(1,by=0, length.out = length(x1.test))
  x.test <- matrix(cbind(x0.test,x1.test, x2.test), ncol = 3)
  
  
  w1.iter <- matrix(w.mat.iter[cnt,1:(dim(x.test)[2]*m)], ncol=m, byrow=FALSE)
  w2.iter <- matrix(w.mat.iter[cnt,((dim(x.test)[2]*m)+1):((dim(X)[2]*m)+m+1)], nrow=(m+1)) 
  
  
  X.w1.test <- (sigmoid((x.test%*%w1.iter), a = 1, b = 0))
  X.w1.test <- cbind(x0.test,X.w1.test)
  z.test  <- sigmoid((X.w1.test%*%w2.iter),a=1,b=0)-0.5
  y.predict <- matrix(data=NA,nrow=length(x1.test),ncol=1)
  y.predict <- replace(y.predict, z.test < 0, -1)
  y.predict <- replace(y.predict, z.test > 0, 1)
  X<- cbind(x1.test,x2.test)
  ##################L01 LOSS#############################
  l.01[cnt]<-(1/length(y.test))*table(y.predict!=y.test)["TRUE"]
  #######################################################
  ##################################LOGIT LOSS############
  f<-z.test +0.5
  Loss.logit[cnt]<- (1/length(y.test))*((t(y.test)%*%log(f) + t(1-y.test)%*%log(1-f)))
  
}
######################################################
k<-seq(1:(dim(w.mat.iter)[1]))

plot(k,l.01, type = "n", main = "L_01",ylim=c(0.45,0.55), xlab ="K", ylab="loss", xlim=c(1,3300))
lines(k,l.01,  lty=1, col = 3, lwd=1)
# lines(k,l.01, lty=2 , col = 4)
# legend(x=0.9, y=-4, legend = c("+1", "-1"), lty=c(1,2))


#######################################################

w.data2 <- read.table("w_mat_data1_ls.txt", quote="\"", comment.char="")
w.mat.iter<- data.matrix(w.data2, rownames.force = NA)

l.01 <- matrix(data=NA, nrow=dim(w.mat.iter))
Loss.logit <- matrix(data=NA, nrow=dim(w.mat.iter))
# cnt<-10
for(cnt in 1:dim(w.mat.iter)[1]){
  
  x0.test <-seq(1,by=0, length.out = length(x1.test))
  x.test <- matrix(cbind(x0.test,x1.test, x2.test), ncol = 3)
  
  
  w1.iter <- matrix(w.mat.iter[cnt,1:(dim(x.test)[2]*m)], ncol=m, byrow=FALSE)
  w2.iter <- matrix(w.mat.iter[cnt,((dim(x.test)[2]*m)+1):((dim(X)[2]*m)+m+1)], nrow=(m+1)) 
  
  
  X.w1.test <- (sigmoid((x.test%*%w1.iter), a = 1, b = 0))
  X.w1.test <- cbind(x0.test,X.w1.test)
  z.test  <- sigmoid((X.w1.test%*%w2.iter),a=1,b=0)-0.5
  y.predict <- matrix(data=NA,nrow=length(x1.test),ncol=1)
  y.predict <- replace(y.predict, z.test < 0, -1)
  y.predict <- replace(y.predict, z.test > 0, 1)
  X<- cbind(x1.test,x2.test)
  ##################L01 LOSS#############################
  l.01[cnt]<-(1/length(y.test))*table(y.predict!=y.test)["TRUE"]
  #######################################################
  ##################################LOGIT LOSS############
  f<-z.test +0.5
  Loss.logit[cnt]<- (1/length(y.test))*((t(y.test)%*%log(f) + t(1-y.test)%*%log(1-f)))
  
}
######################################################
k<-seq(1:(dim(w.mat.iter)[1]))

#plot(k,Loss.logit, type = "n", main = "Scatter plot of x1 vs x2 for different classes in y",ylim=c(-6,1), xlab ="x1", ylab="x2")
lines(k,l.01, lty=1, col = 1, lwd=2)
# lines(k,l.01, lty=2 , col = 4)
# legend(x=0.9, y=-4, legend = c("+1", "-1"), lty=c(1,2))


#######################################################


w.data2 <- read.table("w_mat_data2.txt", quote="\"", comment.char="")
w.mat.iter<- data.matrix(w.data2, rownames.force = NA)

l.01 <- matrix(data=NA, nrow=dim(w.mat.iter))
Loss.logit <- matrix(data=NA, nrow=dim(w.mat.iter))
# cnt<-10
for(cnt in 1:dim(w.mat.iter)[1]){
  
  x0.test <-seq(1,by=0, length.out = length(x1.test))
  x.test <- matrix(cbind(x0.test,x1.test, x2.test), ncol = 3)
  
  
  w1.iter <- matrix(w.mat.iter[cnt,1:(dim(x.test)[2]*m)], ncol=m, byrow=FALSE)
  w2.iter <- matrix(w.mat.iter[cnt,((dim(x.test)[2]*m)+1):((dim(X)[2]*m)+m+1)], nrow=(m+1)) 
  
  
  X.w1.test <- (sigmoid((x.test%*%w1.iter), a = 1, b = 0))
  X.w1.test <- cbind(x0.test,X.w1.test)
  z.test  <- sigmoid((X.w1.test%*%w2.iter),a=1,b=0)-0.5
  y.predict <- matrix(data=NA,nrow=length(x1.test),ncol=1)
  y.predict <- replace(y.predict, z.test < 0, -1)
  y.predict <- replace(y.predict, z.test > 0, 1)
  X<- cbind(x1.test,x2.test)
  ##################L01 LOSS#############################
  l.01[cnt]<-(1/length(y.test))*table(y.predict!=y.test)["TRUE"]
  #######################################################
  ##################################LOGIT LOSS############
  f<-z.test +0.5
  Loss.logit[cnt]<- (1/length(y.test))*((t(y.test)%*%log(f) + t(1-y.test)%*%log(1-f)))
  
}
######################################################
k<-seq(1:(dim(w.mat.iter)[1]))

#plot(k,Loss.logit, type = "n", main = "Logit Loss",ylim=c(-6,1), xlab ="x1", ylab="x2")
lines(k,l.01, lty=2, col = 3, lwd=1)
# lines(k,l.01, lty=2 , col = 4)
# legend(x=0.9, y=-4, legend = c("+1", "-1"), lty=c(1,2))


#######################################################

w.data2 <- read.table("w_mat_data2_ls.txt", quote="\"", comment.char="")
w.mat.iter<- data.matrix(w.data2, rownames.force = NA)

l.01 <- matrix(data=NA, nrow=dim(w.mat.iter))
Loss.logit <- matrix(data=NA, nrow=dim(w.mat.iter))
# cnt<-10
for(cnt in 1:dim(w.mat.iter)[1]){
  
  x0.test <-seq(1,by=0, length.out = length(x1.test))
  x.test <- matrix(cbind(x0.test,x1.test, x2.test), ncol = 3)
  
  
  w1.iter <- matrix(w.mat.iter[cnt,1:(dim(x.test)[2]*m)], ncol=m, byrow=FALSE)
  w2.iter <- matrix(w.mat.iter[cnt,((dim(x.test)[2]*m)+1):((dim(X)[2]*m)+m+1)], nrow=(m+1)) 
  
  
  X.w1.test <- (sigmoid((x.test%*%w1.iter), a = 1, b = 0))
  X.w1.test <- cbind(x0.test,X.w1.test)
  z.test  <- sigmoid((X.w1.test%*%w2.iter),a=1,b=0)-0.5
  y.predict <- matrix(data=NA,nrow=length(x1.test),ncol=1)
  y.predict <- replace(y.predict, z.test < 0, -1)
  y.predict <- replace(y.predict, z.test > 0, 1)
  X<- cbind(x1.test,x2.test)
  ##################L01 LOSS#############################
  l.01[cnt]<-(1/length(y.test))*table(y.predict!=y.test)["TRUE"]
  #######################################################
  ##################################LOGIT LOSS############
  f<-z.test +0.5
  Loss.logit[cnt]<- (1/length(y.test))*((t(y.test)%*%log(f) + t(1-y.test)%*%log(1-f)))
  
}
######################################################
k<-seq(1:(dim(w.mat.iter)[1]))

#plot(k,Loss.logit, type = "n", main = "Scatter plot of x1 vs x2 for different classes in y",ylim=c(-6,1), xlab ="x1", ylab="x2")
lines(k,l.01, lty=2, col = 1, lwd=2)
# lines(k,l.01, lty=2 , col = 4)


legend(x=2000, y=0.54, legend = c( "n=100 with fixed step size", "n=100 with line search","n=10000 with fixed step size", "n = 10000 with line search"), lty=c(1,1,2,2), col = c(3,1,3,1), lwd=c(1,2,1,2))


