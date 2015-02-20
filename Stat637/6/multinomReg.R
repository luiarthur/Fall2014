M <- matrix(c(371,49,74,
              250,45,71,
              64,9,15,
              25,5,13),4,3,byrow=T)

rownames(M) <- c("WF","WM","BF","BM")
colnames(M) <- c("Y","U","N")

n <- sum(M)
y <- numeric(n)
X <- matrix(0,n,3)
X[,1] <- 1

rM <- nrow(M)
cM <- ncol(M)
k <- 0
for (i in 1:rM) {
  gender <- substr(rownames(M)[i],2,2)
  race <- substr(rownames(M)[i],1,1)
  for (j in 1:cM) {
    belief <- colnames(M)[j]

    current.counter <- 0
    while (current.counter < M[i,j]) {
      current.counter <- current.counter + 1
      k <- k + 1
      y[k] <- belief
      X[k,2] <- race
      X[k,3] <- gender
    }  
  }
}

dat <- cbind(y,X)

library(nnet)
mod.race <- multinom(y~X[,2])
mod.gender <- multinom(y~X[,3])
