options("width"=70)
y <- as.vector(as.matrix(read.table("faculty.dat")))

dig <- function(x,a,b,log=F) {
  out <- NULL
  if (!log) {
    out <- 1/(gamma(a)*b^a) * x^(-a-1) * exp{-1/(b*x)}
  } else {
    out <- -lgamma(a)-a*log(b) -(a+1)*log(x) -1/(b*x)
  }

  out
}

rig <- function(n,a,b) 1/rgamma(n,a,scale=b)

gibbs <- function(B=1e5,cand=c(1,1,1,1)) {
  #Priors:
  m <- 1
  s2 <- 1
  as <- 1
  bs <- 1
  at <- 1
  bt <- 1

  k <- length(y)
  M <- matrix(0,nrow=B,ncol=length(y)+3) # theta[1:k],mu,sig2,tau2
  M[1,] <- 1
  
  update.theta <- function(i,mu,sig2,tau2) {
    mu.new <- (y[i]*tau2+mu*sig2) / (tau2+sig2)
    sig2.new <- (tau2*sig2) / (tau2+sig2)
    out <- c(mu.new,sig2.new)
    names(out) <- c("mu","sig2")
    out
  }

  d.theta <- function(theta,i,mu,sig2,tau2,log=T) {
    dnorm(theta,(y[i]*tau2+mu*sig2)/(tau2+sig2),(tau2*sig2)/(tau2+sig2),log=log)
  }
  
  d.mu <- function(mu,theta,s2,log=T) {
    dnorm(mu,(mean(theta)*k*s2+m*tau2)/(k*s2+tau2),s2*tau2/(k*s2+tau2,log=log))
  }

  d.sig2 <- function(sig2,theta,log=T) {
    dig(sig2,as+k/2,1/(1/bs+sum((y-theta)^2)/2),log=log)
  }

  d.tau2 <- function(tau2,theta,mu,log=T) {
    dig(tau2,at+k/2,1/(1/bt+sum((theta-mu)^2)/2))
  }

  # Now write the draw functions.
}
