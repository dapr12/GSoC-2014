svdinv <- function(X, lim=0.01, plot=FALSE) { 
  s <- svd(X)
  if (plot == (TRUE || T)) {
    tmp <- s$d/max(s$d)
    plot(tmp, ylim=range(c(lim, tmp)), type="o", 
       xlab="Component number", ylab="Amplitude", 
       main="SVD component amplitudes")
    text(1, lim, sprintf("lim = %.3f \n",lim), adj=c(0,0.4))  
    abline(h=lim, lty=2)
  }

  lim <- max(s$d) * lim
  idx1 <- which(s$d >= lim)
  idx2 <- which(s$d < lim)
  s$d[idx1] <- 1/s$d[idx1]
  s$d[idx2] <- 0
  zeronum <- length(idx2)

  cat("svdinv zeroed ", zeronum, " of ", length(s$d), " elements \n")
  return (s$v %*% diag(s$d) %*% t(s$u))
}

