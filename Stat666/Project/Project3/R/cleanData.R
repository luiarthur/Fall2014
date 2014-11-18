X <- read.table("collins.txt",header=T)
rownames(X) <- X[,1]
Y <- X[,-1]
Y <- X[,2:19]

write.table(Y,quote=F,col.names=T,row.names=T,file="cleanData.txt")

Z <- read.table("cleanData.txt",header=T)
