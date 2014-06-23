#function y = bspl(x,k,t,r)
#
#B-spline basis functions and their derivatives
#
#INPUT:
#   x   (m x 1 or 1 x m)    Input grid.
#   k   (scalar)            Spline order.
#   t   (n x 1 or 1 x n)    Knots, must be a strictly increasing sequence
#                             and must INCLUDE interval endpoints.
#   r  (scalar)            Order of derivative.
#
#OUTPUT:
#   y   (m x n+k-2)         Basis function (or derivative) values at X
#
bspl<-function( x, k, ti, r)
{
  tKnots <-as.array(ti)
  tt <-as.array(t)
  xmat<-as.matrix(x)
  K<-k
  R<-r
  M<-length(x)
  N<-length(tknots)
  y<- rep(0, N+K-2)
  
  if ( r == 0)
    {
    if ( nrow(tknots)> 1)
      {
      
      tknots<-t(tknots)
      
    }
    tKnots<- c(repmat(tKnots[1],1,K-1), tKnots, repmat(tKnots[N], 1, K-1))
    N<-length(tKnots)
    b<-rep(0,K)
    dr<-rep(0,K-1)
    dl<-rep(0,K-1)
    y<-matrix(0, M, N+K-2)
    for( l in 1:M)
      { b[1]<-1
        i<- max( which(t <= x[l]))
        if( i == N){ i <- (N-K) }
        for(j in 1:K-1)
        { dr[j]<-tKnots[i+j]-xmat[l]
          dl[j]<-xmat[l] - tKnots[i+1-j]
          saved<-0
          term<-as.numeric(0)
        for(r in 1:j)
          { term<-b[r]/(dr[r]+dl[j+1-r])
            b[r]<- saved+ dr[r]*term
            saved<- dl[j+1-r]*term}
          b[j+1]<-saved
        }
          y[l,(i-K+1):i]<-b
      }
    }
  else {
      tt<-c(repmat(tKnots[1],1,K-2), tKnots, repmat(tKnots[N], 1, K-2))
      msp<- matrix(0, M, N+K-2)
      y<- matrix(0, M, N+K-2)
      On<-t(rep(1,M))
      msp<- (K-1)/((t(On) %*% (tt[K:N+2*(K-2)]-tt[1:N+K-3])) %*% bspl(xmat, K-1, tKnots, R-1))
      y[,1]<- -msp[,1]
      y[,2:(N+K-3)]<-msp[,1:N+K-4] - msp[, 2:N+K-3]
      y[,(N+K-2)]<-msp[, N+K-3]
  }
  
}
