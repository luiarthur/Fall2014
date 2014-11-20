source("multT2.R")
source("a.manova.R")
source("lrbind.R")
source("rapply.R")
source("countdown.R")

X <- read.table("collins.txt",header=T)
write.table(X,quote=F,col.names=F,row.names=F,file="cleanData.txt")

rownames(X) <- X[,1]
Y <- X[,-1]
Z <- scale(Y[,1:18])

get.clusters <- function(Z,k=3) { #z=standardized data, k=num of clusters
  D <- dist(Z)
  link <- hclust(D,method="ward.D")
  grp <- cutree(link,k=k)

  z <- as.list(1:k)
  center <- matrix(0,k,ncol(Z))
  for (j in 1:k) {
    z[[j]] <- Z[which(grp==j),]
    center[j,] <- apply(z[[j]],2,mean)
  }
  
  km <- kmeans(Z,center)
  km.grp <- km$cluster

  z.new <- as.list(1:k)
  for (j in 1:k) {
    z.new[[j]] <- Z[which(km.grp==j),]
  }

  out <- list("km"=km,"z"=z.new) # km = kmeans object, z = new data list
  out
}

ks <- 3:7 # number of clusters
clusters <- lapply(as.list(ks),function(x) get.clusters(Z,x))
clus.z <- lapply(clusters,function(x) x$z)
clus.km <- lapply(clusters,function(x) x$km)

manova.result <- rapply(clus.z,a.manova)
colnames(manova.result) <- c("F.stat","df1","df2","p.val")
rownames(manova.result) <- paste0("k=",3:7)
manova.result

library(clue) # for cl_predict
clus.cv <- function(Z,k=3) { # Z is your standardized data, k is # of clusters
  all.km <- get.clusters(Z,k)$km
  n <- nrow(Z)
  out <- NULL

  one.it <- function(i) {
    Z.i <- Z[-i,]
    one.out.km <- get.clusters(Z.i,k)$km
    cl_predict(one.out.km,matrix(Z[i,],1)) == all.km$cluster[i]
  }

  #V1:
  #apply(matrix(1:n),1,one.it)

  #V2:
  out <- NULL
  for (i in 1:n) {
    old.time <- Sys.time()
      out[i] <- one.it(i)
    count.down(old.time,i,n)
  }
  out
}

error.rate <- NULL
for (i in 1:length(ks)) {
  print(paste0("Iteration ",i,":"))
  error.rate[i] <- 1-mean(clus.cv(Z,ks[i]))
}

names(error.rate) <- paste0("k=",ks)
error.rate


