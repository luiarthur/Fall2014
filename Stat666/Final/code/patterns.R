source("rfunctions.R")
source("ibp.R")
source("gibbs.R")

source("data/combine.R",chdir=T)
elapsed.time <- system.time(out <- gibbs.post(Y,a=1,B=1000,burn=0,showProgress=T,
                                              plotProgress=T,a.a=3,a.b=2,
                                              siga=1,sigx=.5))

# What Next?

M <- out$Zs
alpha <- out$alpha
burn <- round(length(M) * .1)

n.col <- unlist(lapply(M,ncol))

pdf("draw.post.out/traceplot.pdf")
  plot(n.col,type="l",main="Trace Plot: Number of Columns in Z",lwd=1,cex=.1,
       col="blue",pch=20)
  abline(h=mean(n.col[-(1:burn)]),lwd=2,col="red")
dev.off()


EAXZ <- function(X,Z,siga=1,sigx=sigX) {
  k <- ncol(Z)
  Ik <- diag(k)
  ZT <- t(Z)
  out <- solve(ZT%*%Z +(sigx/siga)^2 * Ik,ZT%*%X)
  #out <- solve(ZT%*%Z, ZT%*%X)
  out
}

Z.post <- M[-(1:burn)] # Burn in about 100
Z.post.mean <- sum.matrices(Z.post) / length(Z.post)
#Z.post.mean <- ifelse(Z.post.mean>runif(length(Z.post.mean)),1,0)
Z.post.mean <- ifelse(Z.post.mean>.9,1,0)
col0.ind <- which(apply(Z.post.mean,2,function(x) sum(x)==0))
Z.post.mean <- Z.post.mean[,-col0.ind]
a.image(Z.post.mean)


one.A <- EAXZ(Y,Z.post.mean,siga=1,sigx=.5)
d2 <- 2
d1 <- ceiling(nrow(one.A)/d2)


pdf("draw.post.out/postA.pdf")
  a.image(one.A,main="Posterior Mean for A")
dev.off()

plot.post.As <- function(one.A) {
  par(mfrow=c(d1,d2))
  for (i in 1:nrow(one.A)) {
    one.Ai <- matrix(one.A[i,],6,6) # matrix(Y[n,],6,6) = X[[n]]
    a.image(one.Ai,main=paste0("Posterior Mean A",i))
    #a.image(one.Ai,main=paste0("Posterior Mean A",i),col=BLUE)
  }
  par(mfrow=c(1,1))
}

pdf("draw.post.out/postA66.pdf")
  plot.post.As(one.A)
dev.off()

pdf("draw.post.out/Y.pdf")
  a.image(Y,main="Y")
dev.off()

pdf("draw.post.out/postZ.pdf")
  a.image(Z.post.mean,main="Posterior Estimate for Z")
dev.off()


post.ZA <- Z.post.mean %*% one.A
plot.post.ZA <- function(n) {
  par(mfrow=c(1,2))
    a.image(matrix(Y[n,],6,6),main=paste0("n=",n,": ",label[n]))
    a.image(matrix(post.ZA[n,],6,6),
            main=paste0("n=",n,":  ",toString(Z.post.mean[n,])))
  par(mfrow=c(1,1))
}

plot.post.ZA(90)

#a.image(matrix(apply(y9,2,mean),6,6))
a.image(Z.post.mean)
plot.post.As(one.A)