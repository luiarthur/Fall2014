source("colorUnderCurve.R")
bound <- function(x, dens, return.x=TRUE){
  # Mickey Warner: 
  # https://github.com/mickwar/r-sandbox/blob/master/mcmc/bayes_functions.R
  # returns the x-value in dens that is closest
  # to the given x
  if (return.x)
      return(dens$x[which.min(abs(dens$x-x))])

  # returns the y-value in dens at the closest x
  return(dens$y[which.min(abs(dens$x-x))])
}

col.mult = function(col1 = 0x000000, col2 = "gray50"){
  # Mickey Warner: 
  # https://github.com/mickwar/r-sandbox/blob/master/mcmc/bayes_functions.R
  # returns the x-value in dens that is closest
  # to the given x
  if (is.character(col1))
      val1 = t(col2rgb(col1) / 255)
  if (is.numeric(col1))
      val1 = t(int2rgb(col1) / 255)
  if (is.character(col2))
      val2 = t(col2rgb(col2) / 255)
  if (is.numeric(col2))
      val2 = t(int2rgb(col2) / 255)
  rgb(val1 * val2)
}

int2rgb = function(x){
# int2rgb()
# convert an integer between 0 and 16777215 = 256^3 - 1,
# or between 0 and 0xFFFFFF
# this function is depended upon by col.mult
  # Mickey Warner: 
  # https://github.com/mickwar/r-sandbox/blob/master/mcmc/bayes_functions.R
  # returns the x-value in dens that is closest
  # to the given x
  hex = as.character(as.hexmode(x))
  hex = paste0("#", paste0(rep("0", 6-nchar(hex)), collapse=""), hex)
  col2rgb(hex)
}

plot.post <- function(x,main=NULL,hpd=T,color="cornflowerblue",trace=T) {
  mn.x <- round(mean(x),5)
  v.x <- round(sd(x),3)
  den <- density(x)
  rng <- c(min(den$y),max(den$y))

  diff <- rng[2]-rng[1]
  main <- ifelse(is.null(main),"Posterior Distribution",
                         paste("Posterior Distribution for",main))
  plot(density(x),col=color,ylim=c(rng[1],rng[2]+diff*.3),lwd=3,
       main=main)
  legend("topleft",legend=c(paste("Mean =",mn.x),
                            paste("Std. Dev. = ",v.x)),bty="n")
  rng.x <- range(den$x)
  x.diff <- rng.x[2] - rng.x[1]

  if (trace) {
    opts <- par(no.readonly=T)
      left <- rng.x[1] + x.diff*2/3
      right <- rng.x[2]
      par(fig = c(grconvertX(c(left,right),from="user",to="ndc"),
                  grconvertY(c(rng[2],rng[2]+diff*.3),from="user",to="ndc")),
          mar = c(.1,.1,1,.1), new = TRUE)
      plot(x,type="l",col="gray30",cex.main=.5,axes=F,main="Trace Plot")
      axis(1,cex.axis=.5)
      axis(2,cex.axis=.5)
    par(opts)
  }

  color.den(den,rng.x[1],rng.x[2],col.den=color,col.area=color,add=T)
  if (hpd) {
    hpd <- get.hpd(x)
    color.den(den,hpd[1],hpd[2],col.den=col.mult(color),
              col.area=col.mult(color),add=T)
  }

  lines(c(mn.x,mn.x),c(0,bound(mn.x,den,ret=F)),lwd=2,col="red")
  #abline(v=mn.x,col="red",lwd=2)
}


get.hpd <- function(x,a=.05,len=1e3) {
  V <- matrix(seq(0,a,length=len))
  quants <- t(apply(V,1,function(v) quantile(x,c(v,v+1-a))))
  diff <- quants[,2]-quants[,1]
  min.d <- V[which.min(diff)]
  hpd <- quantile(x,c(min.d,min.d+1-a))
  hpd
}



