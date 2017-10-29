rm(list=ls (all=TRUE))
#(a) generating random normal numbers with mean 7 and variance 1
X = rnorm(1000, mean = 7, sd = 1)
#(b) transforming X
V = exp(0.01*X)
EV = mean(V)
#(c)
W = exp(-0.01*X)
EW = mean(W)
#(d)
DEetx = (EV - EW)/(2*0.01)
