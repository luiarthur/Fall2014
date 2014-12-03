a.image <- function(Q,color=paste0("gray",0:100),...) {
  image(t(apply(Q,2,rev)),yaxt="n",xaxt="n",col=color,...)
}

count.down <- function(old.time,i,B) {
  prog <- round(100*i/B,4)
  new.time <- Sys.time()
  time.diff <- as.numeric(new.time-old.time)
  time.remain <- time.diff * (B-i)
  if (time.remain < 60) {
    secs <- round(time.remain)
    time.remain <- paste0(secs,"s     ")
  } else if (time.remain<3600) {
    mins <- round(time.remain%/%60)
    secs <- round(time.remain%%60)
    time.remain <- paste0(mins,"m ",secs,"s        ")
  } else {
    hrs <- round(time.remain%/%3600)
    mins <- round((time.remain%%3600) %/% 60)
    time.remain <- paste0(hrs,"h ",mins,"m         ")
  }
  cat(paste0("\rProgress: ",prog,"%. Time Remaining: ",time.remain," "))
  if (i==B) cat("100%\n")
}

# Example
#B <- 5000
#for(i in 1:B) {
#  old.time <- Sys.time()
#  Sys.sleep(1) # your function here
#  count.down(old.time)
#}


sum.matrices <- function(Ms,return.matrices=F) { 
# Ms is a list of matrices of different lengths
# return.matrices is a boolean. If FALSE, function returns the sum of the matrices.
# If TRUE, function returns a list of the matrices also.

  l <- length(Ms)
  max.c <- max(unlist(lapply(Ms,ncol)))
  max.r <- max(unlist(lapply(Ms,nrow)))
  
  for (i in 1:l) {
    M <- Ms[[i]]
    
    ncol0 <- max.c - ncol(M)
    nrow0 <- max.r - nrow(M)

    if (ncol0>0) {
      col0 <- matrix(0,nrow(M),ncol0)
      M <- Ms[[i]] <- cbind(Ms[[i]],col0)
    }

    if (nrow0>0) {
      row0 <- matrix(0,nrow0,ncol(M))
      M <- Ms[[i]] <- rbind(Ms[[i]],row0)
    }
  }

  out <- Reduce("+",Ms)

  if (return.matrices) out <- list("sum"=out,"matrices"=Ms)

  out
}


# EXAMPLE: #########################################################3
#A <- matrix(1:10,nrow=2)
#B <- matrix(1:6,nrow=3)
#C <- matrix(1:6,nrow=1)
#D <- matrix(1:4)
#
#Ms <- list(A,B,C,D)
#
#
#sum.matrices(Ms)
#sum.matrices(Ms,T)
