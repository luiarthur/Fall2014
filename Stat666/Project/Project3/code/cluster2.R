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
clusters <- lapply(as.list(ks),function(x) get.clusters(Z[,1:15],x))
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

library(doMC)
registerDoMC(strtoi(system("nproc",intern=T))/2)
error.rate <- foreach(i=1:length(ks),.combine=cbind) %dopar% (1-mean(clus.cv(Z,ks[i])) )
colnames(error.rate) <- paste0("k=",ks)
#  error.rate
#  k=3   k=4   k=5   k=6   k=7 
#  0.008 0.234 0.245 0.307 0.330 

#1: Press
#2: Non-press
#3: Biography
#4: Scholarship
#5: Fiction

supGen.k3.cluster <- table(supGen,clus.km[[1]]$cluster)
supGen.prop <- t(apply(supGen.k3.cluster,1,function(x) x/sum(x)))
rownames(supGen.prop) <- c("Press","Non-press","Biography","Scholarship","Fiction")
colnames(supGen.prop) <- paste0("Cluster",1:3)
supGen.prop


gen.k3.cluster <- table(gen,clus.km[[1]]$cluster)
gen.prop <- t(apply(gen.k3.cluster,1,function(x) x/sum(x)))
colnames(gen.prop) <- colnames(supGen.prop)
rownames(gen.prop) <- c("Press: Reporting","Press: Editorial","Press: Reviews",
                        "Religion","Skills & Hobbies","Popular Lore","Biography",
                        "Official Communications","Learned","General Fiction",
                        "Mystery","Science Fiction","Adventure","Romance","Humor")
gen.prop


gen <- Y$Genre
supGen <- ifelse(gen %in% 1:3,1, ifelse(gen %in% 4:6,2, ifelse(gen == 7,3, ifelse(gen %in% 8:9,4,5))))

cv.supGen <- function(Z) {
  euclid.dist <- function(x,y) sum((x-y)^2)

  one.out.centroid <- function(i) {
    z <- as.list(unique(supGen))
    z <- lapply(z,function(x) Z[which(supGen[-i]==x),])
    centroid <- rapply(z,function(x) apply(x,2,mean))
    centroid
  }

  n <- nrow(Z) 
  clust <- NULL
  for (i in 1:n) {
    centroid <- one.out.centroid(i)
    clust[i] <- which.min(apply(centroid,1,function(x) euclid.dist(x,Z[i,])))
  }
  1-mean(clust==supGen)
}

error.rate.supGen <- cv.supGen(Z)
err.natural.v.supGen <- c(error.rate.supGen,error.rate[3])
names(err.natural.v.supGen) <- c("Super.Genres","Natural,k=5")
#  err.natural.v.supGen
#  Super.Genres  Natural,k=5 
#         0.363        0.245


