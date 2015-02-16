source("plotPost.R")
source("countDown.R")
dat <- read.table("sore.txt",header=T)

y <- dat$Y
X <- cbind(1,dat$D,dat$T)

m.probit <- function(y,X,n=nrow(X),k=ncol(X),prior.b=cbind(rep(0,k),rep(5,k)),
                      cand.s=rep(.01,k),B=1e4,burn=round(B*.1),
                      trim.burn=F) {
  
  out <- matrix(0,B,k)
  log.g <- function(b,m,s,p) {
    sum(y*log(p)+(1-y)*log(1-p)) - ((b-m)/s)^2/2
  }
  acc <- rep(0,k)

  for (i in 2:B) {
    old.time <- Sys.time()
    out[i,] <- out[i-1,]
    for (j in 1:k) {
      cand <- rnorm(1,out[i,j],cand.s[j])
      cand.b <- out[i,]
      cand.b[j] <- cand
      p.cand <- pnorm(c(X%*%cand.b))
      p.prev <- pnorm(c(X%*%out[i,]))
      m.ratio <- log.g(cand,prior.b[j,1],prior.b[j,2],p.cand) - 
                 log.g(out[i,j],prior.b[j,1],prior.b[j,2],p.prev)
      
      if (!(any(p.cand==0) || any(p.cand==1))) {
        if (m.ratio > log(runif(1))) {
          out[i,j] <- cand
          if (!(trim.burn)) {
            acc[j] <- acc[j] + 1
          } else if (i>burn){
            acc[j] <- acc[j] + 1
          }
        }
      }

    }
    count.down(old.time,i,B)
  }

  acc.rate <- acc/B
  if (trim.burn) acc.rate <- acc/(B-burn)
  list("post"=out,"acc"=acc.rate)
}

#X1 <- X[,2] - mean(X[,2])
#c.X <- cbind(X[,1],X1,X[,3])
#out <- m.probit(y,c.X,cand.s=c(1,.1,1))
out <- m.probit(y,X,cand.s=c(1,.02,1),trim.burn=T,B=1e5)
out$acc

source("plotPost.R")
plot.post(out$post[,1],main="beta_0",trace=T)
plot.post(out$post[,2],main="beta_1",trace=T)
plot.post(out$post[,3],main="beta_2",trace=T)


hpd.95 <- t(apply(out$post,2,get.hpd))
rownames(hpd.95) <- paste0("beta",0:2)
colnames(hpd.95) <- c("Lower 95% HPD","Upper 95% HPD")
hpd.95

# Need to compute deviance!!!
# Need to interpret values!!!

