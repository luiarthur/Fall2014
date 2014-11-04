# PSA
X <- as.matrix(read.table("PSAData.txt",header=T))
X <- cbind(1,X)

Y <- as.matrix(read.table("PSAContributions.txt",header=T))

#1
n <- nrow(X)
k <- ncol(X)

B <- solve(t(X) %*% X) %*% t(X) %*% Y
XB <- X %*% B
Y.XB <- Y-XB

S <- t(Y.XB) %*% Y.XB / (n-k)

# Q1: Is there a significant relationship between the chemical abundances (X) and 
#     the source of contributions (Y)? What is $\hat(B)$?

library(xtable)
xtab.B <- xtable(B)


