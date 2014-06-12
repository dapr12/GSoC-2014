
SpMed<-function( t, x )
{
  
  time<-t
  xmat<-x
  n<- dim(xmat)[1]
  m<- dim(xmat)[2]
  A <- 0.5 * ( xmat[, 1:m-1] %*% diag(time[2:m]-time[1:m-1]) %*% t( xmat[,1:m-1] )  
               + xmat[, 2:m] %*% diag(t[2:m]-t[1:m-1]) %*% t(xmat[,2:m]) )
  w<-rep(1, n)/n
  norms<- sqrt( diag(A) + t(w) %*% A %*% w - 2 * A %*% w)
  f<- sum(norms)
  err<-1
  iter<- 0 
  while( err > 1e-5 && iter < 50 )
    {
      iter <- iter +1
      f0 <- f 
      if ( any(norms< .Machine$double.eps  )) 
      {i0<- which(norms<eps)
       w<-rep(0, length(n))
       w[i0]<- 1 /length(i0)}
      else {
          w<- 1/norms
          w<- w/sum(w) }
      norms<-sqrt(diag(A) + t(w) %*% A %*% w - 2 * A %*% w)
      f<-sum(norms)
      err<-abs(f/f0-1)
  }
  
  med<-t(w) %*% xmat
    
  list(median = med, 
       
       weights = w, 
       
       norms = norms
       
  ) 
}
