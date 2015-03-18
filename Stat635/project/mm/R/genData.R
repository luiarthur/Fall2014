library(lme4)
library(MASS)
set.seed(5)

n <- 3*30
# THIS SET WORKS with sig.r2=.1!!!
#X <- cbind(1,matrix(runif(n,0,5),n))
#b <- c(2,4)
#gam <- c(3,7,30)

#b <- c(2,4)
b <- 4
gam <- c(3,9,15)

#gam.vec <- cbind(gam[1]+rnorm(1000),gam[2]+rnorm(1000),gam[3]+rnorm(1000))
#
#sum.x <- matrix(0,3,3)
#mean.x <- apply(gam.vec,2,mean)
#for (i in 1:(n/3)) {
#  sum.x <- sum.x + (gam.vec[i,]) %*% t(gam.vec[i,])
#}
#S <- 1/(n/3-1) * sum.x
#G <- S
G <- diag(73,3)
#apply(mvrnorm(1000,c(0,0,0),G),2,mean)

#X <- cbind(1,matrix(runif(n,0,5),n))
X <- matrix(runif(n,0,5),n)

Z <- matrix(0,n,3)
Z[1:(n/3),1] <- 1
Z[(n/3+1):(2*n/3),2] <- 1
Z[(2*n/3+1):n,3] <- 1

e <- rnorm(n,0,.1)

y <- X%*%b + Z%*%gam + e

R <- diag(1,n)
V <- Z%*%G%*%t(Z) + R

b.hat <- solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% y
gam.hat <- G %*% t(Z) %*% solve(V) %*% (y-X%*%b.hat)

y.hat <- X%*%b.hat + Z%*%gam.hat 
plot(y-y.hat)

plot(X,y,pch=20)
points(X,y.hat,col="blue",cex=2)
abline(lm(y~X),col="red")

cbind(b,b.hat)
cbind(gam,gam.hat)

#ind(y,X[,2],apply(Z,1,which.max))
#sink("data.txt")
#  cbind(y,X[,2],apply(Z,1,which.max))
#sink()
