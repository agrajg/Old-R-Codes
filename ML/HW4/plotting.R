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

