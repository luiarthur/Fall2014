source("plotPost.R")
source("countDown.R")
library(MASS)

dat <- read.table("sore.txt",header=T)

y <- dat$Y
X <- cbind(1,dat$D,dat$T)


gibb <- function(y,X,n=nrow(X),k=ncol(X),B=1e4,burn=round(B*.1),
                 trim.burn=F,V=diag(k)) {
  
  out <- matrix(0,B,k)
  Xt <- t(X)
  XX <- Xt%*%X
  S <- solve(XX+solve(V))
  SXt <- S%*%Xt
  z <- rep(0,n)

  for (i in 2:B) {
    old.time <- Sys.time()
    b.hat <- SXt %*% z
    out[i,] <- b <- mvrnorm(1,b.hat,S)
    z <- mvrnorm(1,X%*%b,diag(n))
    count.down(old.time,i,B)
  }

  out
}

out <- gibb(y,X,B=1e5)

par(mfrow=c(3,1))
  plot.post(out[,1],main="beta_0",trace=T)
  par(mfg=c(2,1,3,1))
  plot.post(out[,2],main="beta_1",trace=T)
  par(mfg=c(3,1,3,1))
  plot.post(out[,3],main="beta_2",trace=T)
par(mfrow=c(1,1))

