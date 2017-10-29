#rm(list = ls())
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
data <- read.table("hw3-nn-test.dat", quote="\"", comment.char="")
data <- rename(data, c(V3="y.test"))
data <- rename(data, c(V2="x2.test"))
data <- rename(data, c(V1="x1.test"))
attach(data)
class(data)
# plot(x1.test,x2.test, type = "n", main = "Scatter plot of x1 vs x2 for different classes in y", xlab ="x1", ylab="x2")
# points(x1.test[y.test==1], x2.test[y.test==1], pch=16, col = 2)
# points(x1.test[y.test==-1], x2.test[y.test==-1], pch=1, col = 4)
# legend(x=0.9, y=1, legend = c("+1", "-1"), fill=c(2,4))
# #######################################################
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
  
  #######################################################




