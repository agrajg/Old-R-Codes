rm(list = ls())
setwd("F:/Agraj Gupta Documents/STAT535/HW2")
library("MASS", lib.loc="~/R/R-3.2.2/library")
library("class", lib.loc="~/R/R-3.2.2/library")
library("stats", lib.loc="~/R/R-3.2.2/library")
####################################################################################
data <- read.table("hw2-1d-train.dat", quote="\"", comment.char="")
out <- data.frame(V1=100, V2=1)
data <- rbind(data, out)
x<- data$V1
y<- data$V2
color <- y
color <- replace(color, color == -1, 2)
plot(x, col = color)
####################################################################################
#(a)
p_hat <- sum(y[y==1])/length(y)
mu.plus <- mean(x[y==1])
mu.minus <- mean(x[y==-1])
####################################################################################
#(b)  LDA
K<-1
beta <- K*(mu.plus - mu.minus)
beta0 <- log(p_hat/(1-p_hat)) +  mu.minus*mu.minus - mu.plus*mu.plus
####################################################################################
#(c)
s<- 1
N<- 20


N.points <- 2000
theta.l <- -0.4
l.hat.f <-  matrix(data=NA,nrow=N.points,ncol=1)




theta.l.seq <- seq(-N, N, length.out = N.points)
k <- 1
for(theta.l in theta.l.seq)
{
  f.l.x<- sign(s*x - theta.l)  
  l.hat.f[k,1] <- (1/length(y))*sum(y*f.l.x<0)
  k <- k+1
}
plot(theta.l.seq ,l.hat.f, type="l", xlab="theta_L",    ylab="lhat_01" ) 


############################################################################
y.s <- rbinom(length(y), 1, (1/3))
############################################################################
Y.s<-matrix(data=NA,nrow=length(y),ncol=1)
X_givenY.s<-matrix(data=NA,nrow=length(y),ncol=1)
############################################################################
for(m in 1:length(y) )
{
  if(y.s[m] == 1)
  {
    Y.s[m,1] = 1
    X_givenY.s[m,] <- rnorm(1, mean = 2, sd = 1)
  }
  else
  {
    Y.s[m,1] = -1
    X_givenY.s[m,] <- rnorm(1, mean = -2, sd = 1)
  }      
}
newdata <- data.frame(y.s, X_givenY.s)
logit <- glm(y.s~X_givenY.s,data = newdata, family = binomial)
x.data<-data.frame(X_givenY.s)
prediction <- predict.glm(logit, newdata)
y.predict <- (prediction>=0)
y.predict <- replace(y.predict, y.predict == "TRUE", 1)
prediction <- predict.glm(logit, newdata, type = "response")
newdata <- data.frame(y.s, X_givenY.s, y.predict, prediction)
negdata <- y.predict-1
bayes.loss <- (1/length(X_givenY.s))*(length(X_givenY.s) - sum(y.predict*prediction) + sum((1-prediction)*negdata))

