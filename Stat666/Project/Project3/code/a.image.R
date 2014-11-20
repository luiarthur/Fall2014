# install.packages("fields")
a.image <- function(Q,color=paste0("gray",0:100),...) {
  library(fields)
  pr <- par("mar")
  par(mar=c(5,4.5,4,7))
  image(t(apply(Q,2,rev)),yaxt="n",xaxt="n",col=color,...)
  axis(2,at=seq(0,1,length.out=ncol(Q)),labels=rev(colnames(Q)),las=2)
  axis(3,at=seq(0,1,length.out=nrow(Q)),labels=rownames(Q),las=2)
  image.plot(Q,legend.only=T,col=color)
  par(mar=pr)
}
