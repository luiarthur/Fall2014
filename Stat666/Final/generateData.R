
a1 <- matrix(c(0,1,0,0,0,0,
               1,1,1,0,0,0,
               0,1,0,0,0,0,
               0,0,0,0,0,0,
               0,0,0,0,0,0,
               0,0,0,0,0,0),6,6,byrow=T)

a2 <- matrix(c(0,0,0,1,1,1,
               0,0,0,1,0,1,
               0,0,0,1,1,1,
               0,0,0,0,0,0,
               0,0,0,0,0,0,
               0,0,0,0,0,0),6,6,byrow=T)

a3 <- matrix(c(0,0,0,0,0,0,
               0,0,0,0,0,0,
               0,0,0,0,0,0,
               1,0,0,0,0,0,
               1,1,0,0,0,0,
               1,1,1,0,0,0),6,6,byrow=T)

a4 <- matrix(c(0,0,0,0,0,0,
               0,0,0,0,0,0,
               0,0,0,0,0,0,
               0,0,0,1,1,1,
               0,0,0,0,1,0,
               0,0,0,0,1,0),6,6,byrow=T)

# New Stuff. Generate Data.
N <- 100
K <- 4
Z <- matrix(sample(0:1,N*K,replace=T),N,K)
for (i in 1:N) {
  while (sum(Z[i,])==0) {
    Z[i,] <- sample(0:1,K,replace=T)
  }  
}
A <- matrix(c(a1,a2,a3,a4),K,byrow=T)
ZA <- Z%*%A
sigX <- .5
E <- matrix(rnorm(prod(dim(ZA)),0,sigX),dim(ZA)[1],dim(ZA)[2])
Y <- ZA + E

