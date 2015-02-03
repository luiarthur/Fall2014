#8
dat <- read.table("../2/case.txt",header=T)
y <- dat$Rem
m <- dat$Cases
x <- dat$L1

#a)
mod <- glm(y/m~x,weights=m,family=binomial(link="logit"))
pred <- predict(mod,type="response")

p0 <- .5
p.i <- ifelse(y/m>p0,1,0)
p.i.hat <- ifelse(pred>p0,1,0)

true.ind <- which(p.i==1)
mean(p.i[true.ind]==p.i.hat[true.ind])
(p.i[true.ind]==p.i.hat[true.ind])

