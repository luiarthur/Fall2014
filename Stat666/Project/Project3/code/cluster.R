# SectVC.R
source("multT2.R")
X <- read.table("collins.txt",header=T)
write.table(X,quote=F,col.names=F,row.names=F,file="cleanData.txt")

rownames(X) <- X[,1]
Y <- X[,-1]

Z <- scale(Y[,1:18])
Z <- data.matrix(Z)
D <- dist(Z)
wardlink <- hclust(D,method="ward.D")
plot(wardlink,labels=rownames(Y),cex=.0001)

grps <- as.list(1:7)
grps <- lapply(grps,function(x) cutree(wardlink,k=x))


z <- as.list(1:7)
for (i in 1:7) {
  z[[i]] <- as.list(1:i)
  for (j in 1:i){
    #zbar[[i]][[j]] <- apply(Z[which(grps[[i]]==j),],2,mean)
    # e.g. zbar[[3]][[2]] <- for k=3, get means of the 2nd cluster
    z[[i]][[j]] <- Z[which(grps[[i]]==j),]
  }
}

lrbind <- function(L) { #rbinds a list of matrices
  k <- length(L)
  p <- ncol(L[[1]])
  out <- matrix(0,0,p)
  for (i in 1:k) {
    out <- rbind(out,L[[i]])
  }
  out
}

#ybars <- zbar[[3]]
ys <- z[[3]]
my.manova <- function(ys) {# a list of matrices
  k <- length(ys)
  n <- as.numeric(lapply(ys,nrow))
  p <- ncol(ys[[1]])
  N <- sum(n)
  ybar. <- lapply(ys,function(y) as.matrix(apply(y,2,mean)))
  y <- lrbind(ys)
  ybar.. <- apply(y,2,mean)

  H <- NULL
  for (i in 1:k) {
    A <- ybar.[[i]]-ybar..
    H <- n[i] * A%*%t(A)
  }
  
 E <- NULL
 for (i in 1:k){
   for (j in 1:(n[[i]])){
     A <- ys[[i]][j,] - ybar.[[i]]
     E <- A%*%t(A)
   }
 }
 
 lam <- det(E) / det(E+H)
 vH <- k-1
 vE <- N-k
 
 #see p.185 of 666 (373)
}
