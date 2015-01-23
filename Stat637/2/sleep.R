y <- ifelse(sleep$extra>0,1,0)
x <- ifelse(sleep$group==1,0,1)
plot(x,y)

glm(y~x)
