a.image <- function(Q,color=paste0("gray",0:100),...) {
  image(t(apply(Q,2,rev)),yaxt="n",xaxt="n",col=color,...)
}


