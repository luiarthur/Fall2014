# Paper:
#http://www.stat.columbia.edu/~gelman/research/published/multi2.pdf
# Data taken from:
#http://www.unc.edu/courses/2007spring/enst/562/001/docs/assignments/assign10.htm
source("countdown.R")
source("plotpost.R")

radon <- read.csv("radon.txt",header=T)
city <- read.csv("cty.txt",header=T)

rad <- radon[which(radon$state=="MN"),c("activity","basement","county")]
rad <- rad[which(sapply(rad$basement,as.character) > ""),]
rad <- rad[which(rad$activity > 0),]
ctyInfo <- city[which(city$st=="MN"),c("cty","Uppm")]
dat <- merge(rad,ctyInfo,by.x="county",by.y="cty")

N <- nrow(dat)
J <- length(unique(dat$county))
y <- log(dat$activity)
x <- ifelse(dat$basement=="Y",1,0)
counties <- unique(dat$county)

G <- matrix(0,N,J)
u <- unique(dat$Uppm)
Y <- as.list(1:J)
X <- as.list(1:J)
for (j in 1:J) {
  Y[[j]] <- y[which(dat$county==counties[j])]
  X[[j]] <- x[which(dat$county==counties[j])]
}


mh <- function(B=1e3,csa=rep(3,J),csb=3,csg0=3,csg1=3,cssy=2,cssa=2,
               sigb2=100,ubg0=100,ubg1=100,ubsy2=100,ubsa2=100) {
  # Likelihoods:
  # yij ~ N(aj+b*xij, sy2)  ......(1)
    ll1 <- function(j,aj,b,sy2) sum(dnorm(Y[[j]],aj+b*X[[j]],sqrt(sy2),log=T))
  #  aj ~ N(g0+g1*uj, sa2) ......(2)
    ll2 <- function(aj,g0,g1,uj,sa2) dnorm(aj,g0+g1*uj,sqrt(sa2),log=T)

  #Priors:
  # aj ~ N(g0+g1*uj,sa2)
    laj <- ll2
  # b ~ N(0,100)
    lb <- function(b) dnorm(b,0,sqrt(sigb2),log=T)
  # g0 ~ U(-100,100)
    lg0 <- function(g0) dnorm(g0,0,10,log=T) #0
  # g1 ~ U(-100,100)
    lg1 <- function(g1) dnorm(g1,0,10,log=T) #0
  # sy2 ~ U(0,100)
    lsy2 <- function(sy2) dnorm(sy2,2,log=T)#0
  # sa2 ~ U(0,100)
    lsa2 <- function(sa2) dnorm(sa2,2,log=T)#0

  a <- matrix(0,B,J)
  b <- rep(0,B)
  g0 <- rep(1,B)
  g1 <- rep(1,B)
  sy2 <- rep(1,B)
  sa2 <- rep(1,B)

  acc.a <- rep(0,J)
  acc.b <- 0
  acc.g0 <- 0
  acc.g1 <- 0
  acc.sy2 <- 0
  acc.sa2 <- 0
  
  for (z in 2:B) {
    ot <- Sys.time()

    a[z,] <- a[z-1,]
    b[z] <- b[z-1]
    g0[z] <- g0[z-1]
    g1[z] <- g1[z-1]
    sy2[z] <- sy2[z-1]
    sa2[z] <- sa2[z-1]

    # Update a:
    for (j in 1:J) {
      cand <- rnorm(1,a[z,j],sqrt(csa[j]))
      lr <- ll1(j,cand,  b[z],sy2[z]) + laj(cand,  g0[z],g1[z],u[j],sa2[z]) -
           (ll1(j,a[z,j],b[z],sy2[z]) + laj(a[z,j],g0[z],g1[z],u[j],sa2[z]))
      if (lr > log(runif(1))) {
        a[z,j] <- cand
        acc.a[j] <- acc.a[j]+1
      }

      # Update b:
      cand <- rnorm(1,b[z],csb)
      #j <- sample(1:J,1)
      lr <- ll1(j,a[z,j],cand,sy2[z]) + lb(cand) - 
           (ll1(j,a[z,j],b[z],sy2[z]) + lb(b[z]))
      if (lr > log(runif(1))) {
        b[z] <- cand
        acc.b <- acc.b+1
      }

      # Update sy2:
      cand <- rnorm(1,sy2[z],cssy)
      if (cand>0){
        #j <- sample(1:J,1)
        lr <- ll1(j,a[z,j],b[z],cand) + lsy2(cand) - 
             (ll1(j,a[z,j],b[z],sy2[z]) + lsy2(sy2[z]))
        if (lr > log(runif(1))) {
          sy2[z] <- cand
          acc.sy2 <- acc.sy2+1
        }
      }

      # Update sa2:
      cand <- rnorm(1,sa2[z],cssa)
      if (cand>0){
        lr <- ll2(a[z,j],g0[z],g1[z],u[j],cand) + lsa2(cand) - 
             (ll2(a[z,j],g0[z],g1[z],u[j],sa2[z]) + lsa2(sa2[z]))
        if (lr > log(runif(1))) {
          sa2[z] <- cand
          acc.sa2 <- acc.sa2+1
        }
      }

      # Update g0:
      cand <- rnorm(1,g0[z],csg0)
      lr <- ll2(a[z,j],cand,g1[z],u[j],sa2[z]) + lg0(cand) - 
           (ll2(a[z,j],g0[z],g1[z],u[j],sa2[z]) + lg0(g0[z]))
      if (lr > log(runif(1))) {
        g0[z] <- cand
        acc.g0 <- acc.g0+1
      }

      # Update g1:
      cand <- rnorm(1,g1[z],csg1)
      lr <- ll2(a[z,j],g0[z],cand,u[j],sa2[z]) + lg1(cand) - 
           (ll2(a[z,j],g0[z],g1[z],u[j],sa2[z]) + lg1(g1[z]))
      if (lr > log(runif(1))) {
        g1[z] <- cand
        acc.g1 <- acc.g1+1
      }
    }# End of J Loops
    count.down(ot,z,B)
  }# End of B Loops
  
  out <- list("a"=a,"b"=b,"g0"=g0,"g1"=g1,"sy2"=sy2,"sa2"=sa2,
              "acc.a"=acc.a/B,"acc.b"=acc.b/B,"acc.g0"=acc.g0/B,"acc.g1"=acc.g1/B,
              "acc.sy2"=acc.sy2/B,"acc.sa2"=acc.sa2/B)
  out
}

out <- mh(B=1e5)
plot.posts(out$a[,c(1,50,85)],names=c("a1","a50","a85"))
plot.posts(out$b)
plot.posts(out$g0)
plot.posts(out$g1)
plot.posts(out$sy2)
plot.posts(out$sa2) # With uniform prior, trace plot is terrible; and acceptance rate is high (.8)

out$acc.a
out$acc.b
out$acc.g0
out$acc.g1
out$acc.sy2
out$acc.sa2
