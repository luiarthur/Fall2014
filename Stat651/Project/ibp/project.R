dat <- read.table("BX-Book-Ratings.csv",header=T)

p <- 10 # Num. of Books
x <- table(dat[,2])
top.10.books <- as.vector(dat[rev(order(x)[1:p]),2])
cust <- dat[dat[,2] %in% top.10.books,1]
n <- length(cust)

do.it <- function(cust.id) {
  ind <- which(dat[,1] == cust.id)
  y <- top.10.books %in% dat[ind,2]

  z <- NULL
  for (i in 1:p) {
    z[i] <- ifelse(y[i],dat[ind[which(dat[ind,2]==top.10.books[i])],3],-1)
  }

  t(z)
}

M <- t(apply(matrix(cust),1,do.it))
rownames(M) <- cust
colnames(M) <- top.10.books

M

