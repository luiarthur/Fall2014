library(truncnorm) # for rtruncnorm
library(xtable)
library(splines)
source("plotpost.R")

# 12 populations of tulips
#   - 30 measurements made at each of 7 chilling times
#   - Total of 210 = 30*7 measurements per population
#   - none of population 12's germinated (so we exclude)

dat <- read.csv("data/tulip.csv")
dat <- dat[-which(dat$Pop==12),]

y <- ifelse(dat$Germinated=="N",0,1)
N <- length(y)
chill <- dat$ChillingTime
uchill <- unique(chill)
pop <- dat$Population
upop <- unique(pop)
k <- length(unique(pop))
X <- cbind(1,chill)
Z <- matrix(0,N,k*ncol(X))
for (i in 1:N) {
  Z[i,pop[i]] <- 1
  Z[i,pop[i]+k] <- X[i,2]
}

pdf("images/rawData.pdf",width=19,height=13)
  par(mfrow=c(6,2))
    yy <- apply(matrix(uchill),1,function(ct) mean(y[which(chill==ct)]))
    plot(uchill,yy,main="All Populations",xlab="Chill Time (Weeks)",
         ylab="Germination Rate",col="purple",pch=20,cex=2,ylim=c(-.1,1.1))
    for (pn in 1:k) {
        yy <- apply(matrix(uchill),1,function(ct) {
          mean(y[which(pop==pn & chill==ct)])
        })
        #plot(uchill,yy,main=paste("Population",pn),ylab="Germination Rate",xlab="Chill Time (Weeks)")
        plot(uchill,yy,main=paste("Population",pn),ylab="",xlab="",pch=20,
             col="purple",cex=2,ylim=c(-.1,1.1))
    }
  par(mfrow=c(1,1))
dev.off()


mvrnorm <- function(M,S,n=nrow(S)) M + t(chol(S)) %*% rnorm(n)

updateW <- function(b,g){
  w <- numeric(length(y))
  #w[y==1] <- rtruncnorm(sum(y==1),a=0,b=Inf, mean=X[y==1,]%*%b,sd=1)
  #w[y==0] <- rtruncnorm(sum(y==0),a=-Inf,b=0,mean=X[y==0,]%*%b,sd=1)
  w[y==1] <- rtruncnorm(sum(y==1),a=0,b=Inf, mean=X[y==1,]%*%b + Z[y==1,]%*%g,sd=1)
  w[y==0] <- rtruncnorm(sum(y==0),a=-Inf,b=0,mean=X[y==0,]%*%b + Z[y==0,]%*%g,sd=1)
  w
}

gibbs <- function(y,X,Z,n=nrow(X),p=ncol(X),k=ncol(Z),B=1e4,burn=round(B*.1),
                 trim.burn=F, U=10*solve(t(X)%*%X), V=10*solve(t(Z)%*%Z)) {

  Xt <- t(X)
  Zt <- t(Z)
  XX <- Xt%*%X
  ZZ <- Zt%*%Z
  T1 <- solve(XX + solve(U))
  T2 <- solve(ZZ + solve(V))

  # Initialize Parameters
  b <- matrix(0,B,p)
  g <- matrix(0,B,k)
  colnames(b) <- paste0("b",0:(p-1))
  colnames(g) <- c(paste0("g0.",1:(k/2)),paste0("g1.",1:(k/2)))
  #######################

  for (i in 2:B){
    #Updates:
    old.time <- Sys.time()
    w <- updateW(b[i-1,],g[i-1,])
    #b[i,] <- mvrnorm(T1 %*% Xt %*% w, T1) 
    b[i,] <- mvrnorm(T1 %*% Xt %*% (w-Z%*%g[i-1,]), T1) 

    w <- updateW(b[i,],g[i-1,])
    g[i,] <- mvrnorm(T2 %*% Zt %*% (w-X%*%b[i,]),   T2) 
    count.down(old.time,i,B)
  }

  list("b"=b,"g"=g)
}

out <- gibbs(y,X,Z,B=1e4)
plot.posts(out$b,names=c("beta0","beta1"))
b <- apply(out$b,2,mean)
g <- apply(out$g,2,mean)


compare.chill.effect <- function(G) {
  compare.one.pair <- function(i,j) {
    diff <- G[,i]-G[,j]
    ci <- mean(diff) + quantile(diff,c(.05,.95))
    ci[1] <= 0 && 0 <= ci[2]
  }

  k <- ncol(G)
  out <- matrix(0,k,k)
  J <- matrix(1:k)
  for (j in 1:k) {
    #out[i,] <- apply(J,1,function(j) compare.one.pair(i,j))
    for (i in 1:j) {
      out[i,j] <- compare.one.pair(i,j)
    }
  }

  out
}

compare.chill.effect(cbind(out$g[,12:22],out$b[,2]))
