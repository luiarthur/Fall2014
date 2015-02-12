dat <- read.table("sore.txt",header=T)

y <- dat$Y
X <- cbind(1,dat$D,dat$T)

