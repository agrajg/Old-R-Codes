#Problem 1 CODE

rm(list=ls (all=TRUE))
#Read the data
data <- read.table('http://www.stat.washington.edu/tsr/s509/examples/caschool.csv', header=TRUE,sep=",")
testscr <- data$testscr
stu <- data$enrl_tot
teachers <- data$teachers
stutchrratio <- stu/teachers
#linear fit
fit <- lm(formula = testscr ~ stutchrratio)

EscrSTfn = matrix(nrow=12, ncol=1)
EscrSTbin = matrix(nrow=12, ncol=1)
p <- 1
for (bin in 14:25) 
  {
  sum <- 0
  k <- 0
  for(i in 1:420)
    {
      if ( stutchrratio[i] >bin & stutchrratio[i]<bin+1 )
      {
        sum <- sum + testscr[i]
        k=k+1 
      }
    }
  EscrSTfn[p] <- sum/k
  EscrSTbin[p] <- bin + 0.5
  p <- p+1
 }
loess.fit <- loess(testscr  ~ stutchrratio)
help(loess)
plot(stutchrratio,testscr,xlab="Student to teacher ratio", ylab="Test Score",main="Relating Student to teacher ratio and Test Score", col="grey")
predict(loess.fit,data.frame(testscr=20)) # fitted value for ed=8
predict(loess.fit,data.frame(testscr))
points(sort(stutchrratio),predict(loess.fit,data.frame(stutchrratio=sort(stutchrratio))),col="black",type="l")
points(EscrSTbin, EscrSTfn, bg = "black")
points(abline(fit,col="red"))

