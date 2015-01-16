x <- 1:20
y <- c(1,6,16,23,27,39,31,30,43,51,63,70,88,97,91,104,110,113,149,159)

# Plots: ###################################################################
par(mfrow=c(1,2),cex.main=.9)

# 1a)
plot(x,y,pch=20,cex=1.5,main="Number of Cases Vs. Time Period",
     xlab="Time Period",ylab="Number of Cases")

# 1b)
plot(log(x),log(y),pch=20,cex=1.5,main="Log Number of Cases Vs. 
     \n Log Time Period", xlab="Log Time Period",ylab="Log Number of Cases")
############################################################################

# 1c)


iwls <- function(X,b=rep(0,ncol(X)),z) {
  X <- cbind(1,log(X))
  W <- exp(X%*%b)
   
}
