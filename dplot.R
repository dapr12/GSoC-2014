dpout<-function(lamb)
{
  lambda<-lamb
  
  m<-length(lambda)
  
  d<-rep(0,m-1)
  
  fhat<-rep(0,m)
  
  for( i in 1:m)
    {
      fhat[i]<-sum(log(lambda[i:m])) - (m-i+1)*log(mean(lambda[i:m]))
  }

  imaj<-chull(d, fhat)

  thmin<-rep(0,m)
  
  for( i in 1:m-1)
    {
      thmin[i]<-max((fhat[i+1:m]- fhat[i])/(d[i+1:m]-d[i])))
  }

  thmin(m)<-0

  thmax<-rep(0,m)

  thmax[1]<-Inf

  for( i in 2:m)
    {
    
    thmax[i]<-min((fhat[i]-fhat[1:i-1])/(d[i]-d[1:i-1]))
    
  }
  
  ijump<-which(thmin<=thmax)
  
  bstck<-rep(0,m)

  for(i in 1:m)
    {
    bstck[i]<-(1/m)*sum(1/i:m)
          
  }

  bstck<-sum(lambda)*bstck

  list(dimension = d, 
     
     fhat = fhat, 
     
     imaj = imaj,
     
     ijump =
    
     bstck = bstck  ) 
}

