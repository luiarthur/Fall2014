library(truncnorm) # for rtruncnorm
source("plotPost.R")
source("countDown.R")

dat <- read.table("sore.txt",header=T)

y <- dat$Y
X <- cbind(1,dat$D,dat$T)

mvrnorm <- function(M,S,n=nrow(S)) M + t(chol(S)) %*% rnorm(n)

updateZ <- function(x,y,b){
  z <- numeric(length(y))
  z[y==1] <- rtruncnorm(sum(y==1),a=0,b=Inf,mean=x[y==1,] %*% b,sd=1)
  z[y==0] <- rtruncnorm(sum(y==0),a=-Inf,b=0,mean=x[y==0,] %*% b,sd=1)
  z
}


gibb <- function(y,X,n=nrow(X),k=ncol(X),B=1e4,burn=round(B*.1),
                 trim.burn=F,V=diag(1,k)) {
  
  S <- solve(t(X)%*%X + solve(V))
  Xt <- t(X)

  # Initialize Parameters
  z <- 1
  beta <- matrix(0,B,k)
  #######################

  for (i in 2:B){
    #Updates:
    old.time <- Sys.time()
    z <- updateZ(X,y,beta[i-1,])
    beta[i,] <- mvrnorm(S %*% Xt%*%z, S) 
    count.down(old.time,i,B)
  }

  beta
}

out <- gibb(y,X,B=1e3)

plot.posts(out,names=c("b0","b1","b2"))

hpd.95 <- t(apply(out,2,get.hpd))
rownames(hpd.95) <- paste0("beta",0:2)
colnames(hpd.95) <- c("Lower 95% HPD","Upper 95% HPD")
hpd.95

