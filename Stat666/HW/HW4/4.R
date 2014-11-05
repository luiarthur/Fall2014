options("width"=150)

# PSA
X <- as.matrix(read.table("PSAData.txt",header=T))
X <- cbind(1,X)

Y <- as.matrix(read.table("PSAContributions.txt",header=T))

#1
n <- nrow(X)
r <- ncol(X)
q <- r-1
p <- ncol(Y)

B <- solve(t(X) %*% X) %*% t(X) %*% Y
XB <- X %*% B
Y.XB <- Y-XB
E <- t(Y.XB) %*% Y.XB

S <-  E / (n-r)

# Q1: Is there a significant relationship between the chemical abundances (X) and 
#     the source of contributions (Y)? What is $\hat(B)$?

# What is $\hat(B)$?
library(xtable)
xtab.B <- xtable(B)

y.bar <- apply(Y,2,mean)
H <- t(Y) %*% Y - n * y.bar %*% t(y.bar)

lam <- det(E) / det(E+H)

# lam can also be calculated this way:
s <- min(p,q)
l <- eigen(solve(E,H))$values[1:s]
lam <- prod(1/(1+l))
V <- sum(l/(1+l)) # Pillai


lam.to.F <- function(lam,p,vh,ve) { # Approximation
  t <- sqrt( (p^2*vh^2-4) / (p^2+vh^2-5) )
  L <- lam^(1/t)
  w <- ve + vh - (p+vh+1)/2

  df <- c(p*vh, w*t-(p*vh-2)/2)

  F.stat <- (1-L) / L * df[2]/df[1]
  out <- c(F.stat,df)
  names(out) <- c("F.stat","df1","df2")

  out
}

V.2.F <- function(V,s,N,m) { # Approximation
  F.stat <- (2*N + s + 1) / (2*m + s + 1) * V/(s-V)
  df <- s * (c(m,N) + s + 1)
  
  out <- c(F.stat,df)
  names(out) <- c("F.stat","df1","df2")
  
  out
}

F.L <- lam.to.F(lam,p=p,vh=q-1,ve=n-r)
p.L <- 1-pf(F.L[1],F.L[2],F.L[3])

F.V <- V.2.F(V,s=min(p,q),N=(n-q-p-2)/2,m=(abs(p-1)-1)/2)
p.V <- 1-pf(F.V[1],F.V[2],F.V[3])

# p.val < .05 => Yes, there is a significant contribution. H_0 B1 = O is rejected.

#2 
var.B.vec <- S %x% solve(t(X)%*%X)
B0 <- B[1,]
B1 <- B[-1,]

# Another.Lambda <- det(cov(cbind(Y,X[,-1]))) / (det(var(X[,-1])) * det(var(Y))) # But why is it different?
# We reject H0, and the first eigen value does not completely dominate the other eigen values. (See: l/sum(l))
# l1+l2 accounts for 94% of the eigen values, which substantially dominates the other eigen values.
# So, the essential dimensionality of the relationship between X and Y is 2.
# This was not directly evident from inspecting B1, because B1 is large and it is difficult to see trends.

sum(l[1:2]) / sum(l)


#3: Canonical Correlation 
# I don't know what to do for this problem YET.
#   OBJECTIVE: Summarize the linear relationship betweem the two groups of variables, Y & X.
Syy <- var(Y)
Sxx <- var(X[,-1])
Syx <- cov(Y,X[,-1])
Sxy <- t(Syx)
A <- solve(Syy) %*% Syx %*% solve(Sxx) %*% Sxy
ri <- eigen(A)$values[1:s]
R2 <- prod(ri) # Should be the same as det(A)
#R2 <- det(A) # Should be the same as product of the eigen values of A
# The first 6 ri's?

L.m <- apply(matrix(1:s),1,function(m) prod(1-ri[m:length(ri)]))
F.m <- t(apply(matrix(1:s),1,function(m) lam.to.F(L.m[m],p=q-m+1,vh=p-m+1,ve=n-m-p)))
p.m <- apply(F.m,1,function(x) 1-pf(x[1],x[2],x[3]))
p.m

#4:
Xr <- X[,-which(colnames(X)=="Pb")]
Br <- solve(t(Xr) %*% Xr) %*% t(Xr) %*% Y
Y.XBr <- Y - Xr%*%Br
Er <- t(Y.XBr) %*% Y.XBr

L.full.red <- det(E) / det(Er)
h <- 1
F.f.r <- lam.to.F(L.full.red,p=p,vh=h,ve=n-r)
p.L.f.r <- 1-pf(F.f.r[1],F.f.r[2],F.f.r[3])

# p.L.f.r < .05 => Pb is important in overall prediction of pollution source emissions.


