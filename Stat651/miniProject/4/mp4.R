source("countdown.R")
options("width"=100)
y <- as.vector(as.matrix(read.table("faculty.dat")))
k <- length(y)

dig <- function(x,a,b,log=F) { # b is rate. 1/b is scale.
  out <- NULL
  if (!log) {
    out <- 1/(gamma(a)*b^a) * x^(-a-1) * exp(-1/(b*x))
  } else {
    out <- -lgamma(a)-a*log(b) -(a+1)*log(x) -1/(b*x)
  }

  out
}

rig <- function(n,a,b) 1/rgamma(n,a,scale=b)

# Prior Speicifications
  n <- 1e5
  #Priors:
  m <- 4.5 # => E[mu] = 4.5
  s2 <- 2  # => V[mu] = 2
  as <- 5/2 # => E[sig2] = 1
  bs <- 3/2 # => V[sig2] = 2
  at <- 13/2 # => E[tau2] = 3
  bt <- 2/33 # => V[tau2] = 2

  prior.tau2 <- rig(n,at,sqrt(bt))
  prior.sig2 <- rig(n,as,sqrt(bs))
  prior.mu <- rnorm(n,m,sqrt(s2))
  prior.theta <- rnorm(n,prior.mu,sqrt(prior.tau2))
  prior.y <- rnorm(n,prior.theta,sqrt(prior.sig2))

  plot(density(prior.y),lwd=3,col="red",main="Prior Predictive")
  abline(v=mean(prior.y)) # mean about 4.5

gibbs <- function(B=1e5) {
  M <- matrix(0,nrow=B,ncol=k+3) # theta[1:k],mu,sig2,tau2
  M[1,] <- 2
  
  update.theta <- function(i,mu,sig2,tau2) { # theta ~ N(mu,sig2)
    mu.new <- (y[i]*tau2+mu*sig2) / (tau2+sig2)
    sig2.new <- (tau2*sig2) / (tau2+sig2)
    out <- c(mu.new,sig2.new)
    names(out) <- c("mu","sig2")
    out
  }

  update.mu <- function(theta,tau2) { # mu ~ N(m,s2)
    theta.new <- (mean(theta)*k*s2+m*tau2)/(k*s2+tau2)
    s2.new <- s2*tau2/(k*s2+tau2)
    out <- c(theta.new,s2.new)
    names(out) <- c("m","s2")
    out
  }

  update.sig2 <- function(theta) { # sig2 ~ IG(as,bs)
    as.new <- as+k/2 # Does not change
    bs.new <- 1/(1/bs+sum((y-theta)^2)/2)
    out <- c(as.new,bs.new)
    names(out) <- c("as","bs")
    out
  }
  
  update.tau2 <- function(theta,mu) { # tau2 ~ IG(at,bt)
    at.new <- at+k/2 # Does not change
    bt.new <- 1/(1/bt+sum((theta-mu)^2)/2)
    out <- c(at.new,bt.new)
    names(out) <- c("at","bt")
    out
  }

  for (i in 2:B) {
    old.time <- Sys.time()
    M[i,] <- M[i-1,]
    
    # theta[1:k],mu,sig2,tau2
    for (j in 1:k) {
      new.theta <- update.theta(j,M[i,k+1],M[i,k+2],M[i,k+3])
      M[i,j] <- rnorm(1,new.theta[1],new.theta[2])
    }

    new.mu <- update.mu(M[i,1:k],M[i,k+3])
    M[i,k+1] <- rnorm(1,new.mu[1],new.mu[2])

    new.sig2 <- update.sig2(M[i,1:k])
    M[i,k+2] <- rig(1,new.sig2[1],new.sig2[2])

    new.tau2 <- update.tau2(M[i,1:k],M[i,k+1])
    M[i,k+3] <- rig(1,new.tau2[1],new.tau2[2])
    
    #print(M[i,(k+1):(k+3)])
    #print(new.tau2)
    #Sys.sleep(1)
    #cat(paste0("\r",round(i/B*100),"%"))
    count.down(old.time,i,B)
  }
  
  M
}

#1: Posterior
M <- gibbs(B=1e5)
M <- M[-(1:200),]
N <- nrow(M)

#for (i in 1:ncol(M)){
#  plot(M[,i],main=i,type="l")
#  scan()
#}


#2: E[theta|Y]
M.mean <- apply(M,2,mean)
M.upper <- apply(M,2,function(x) quantile(x,.975))
M.lower <- apply(M,2,function(x) quantile(x,.025))

#3: V[theta|Y]
M.cov <- var(M)
#sum(M.cov > .05) / prod(dim(M.cov))

# Plots for mean and variance
plot(M.mean[1:23],col="blue",pch=20,ylim=c(4.7,6.5),type="o",main="Posterior Mean Theta's")
lines(M.upper[1:23],col="red",type="b",pch=20)
lines(M.lower[1:23],col="red",type="b",pch=20)

plot.hyper <- function(m,names=NULL) {
  library(MASS)
  k <- ncol(m)
  par(mfrow=c(k,k))
  for (i in 1:k) {
    for (j in 1:k) {
      if (i<j) {
        name <- ifelse(is.null(names),"",paste("Bivariate Trace Plot for",names[i],"&",names[j]))
        K <- kde2d(m[,i],m[,j])
        contour(K,col="red",main=name,cex.main=.9,xlab=names[i],ylab=names[j])
      } else if (i>j) {
        name <- ifelse(is.null(names),"",paste("Bivariate Contour Plot for",names[i],"&",names[j]))
        plot(m[,i],m[,j],type="l",col="pink",main=name,cex.main=.9,xlab=names[i],ylab=names[j])
      } else {
        name <- ifelse(is.null(names),"",paste("Posterior Density for",names[i]))
        plot(density(m[,i]),col="blue",lwd=3,main=name)
      }
    }
  }
  par(mfrow=c(1,1))
}
plot.hyper(m<-M[80000:nrow(M),24:26],c("mu","sigma2","tau2"))

#4: Posterior Predictive
source("../3/color.R")
#                     mu           tau2
theta.pred <- rnorm(N,M[,k+1],sqrt(M[,k+3]))
#                                    sig2
post.pred <- rnorm(N,theta.pred,sqrt(M[,k+2]))
#post.pred <- ifelse(post.pred>7,7,post.pred)
#post.pred <- ifelse(post.pred<0,0,post.pred)
#post.pred.den <- density(post.pred,from=0,to=7)
post.pred.den <- density(post.pred)
p.gt.5 <- mean(post.pred>5)
mx <- max(post.pred.den$x)
plot(post.pred.den,lwd=3,col="blue",main="Posterior Predictive for Next Average Faculty Evaluation")
color.den(post.pred.den,5,mx,col="blue")
text(5.8,.2,round(p.gt.5,4),cex=2)


