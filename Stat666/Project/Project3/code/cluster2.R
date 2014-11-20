source("multT2.R")
source("a.manova.R")
source("lrbind.R")
source("rapply.R")

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
dat.clus <- lapply(as.list(ks),function(x) get.clusters(Z,x)$z)

rapply(dat.clus,a.manova)
