

mhat_nw<-function(x, y, h){
  
    y<-y[order(x)]
    
    x<-x[order(x)]
    
    n<-length(x)
    
    m<-50
    
    t<-numeric(m)
    
    for(j in 1:m) t[j]<-min(x)+j*(max(x)-min(x))/m
    
    #t<-seq(from=0, to=1, length=m)
    
    est<-numeric(m)
    
    num<-numeric(m)
    
    denom<-numeric(m)
    
    for(j in 1:m){
    
      num[j]<-0
      
      denom[j]<-0
      
      est[j]<-0
      
      for(i in 1:n){
      
          ui<-(t[j]-x[i])/h
          
          #kui<-dnorm(ui)
          
          kui<-bwkern(ui)
          
          num[j]<-num[j]+kui*y[i]
          
          denom[j]<-denom[j]+kui
          }
          
    
    num[j]<-num[j]/h
    
    denom[j]<-denom[j]/h
    
      if (denom[j] != 0) {est[j]<-num[j]/denom[j]}
    
      else {est[j]<-0}}
      
nwest<-data.frame(t, est, h)

return(nwest)

  }



