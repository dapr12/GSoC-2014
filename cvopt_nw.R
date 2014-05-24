
cvopt_nw<-function(x, y, int=c(0.0001, 0.5)){
  
  y<-y[order(x)]
  
  x<-x[order(x)]
  
  n<-length(x)
  
  est<-numeric(n)
  
  x1<-numeric(n-1)
  
  y1<-numeric(n-1)
  
  m<-25
  
  h<-numeric(m)
  
  cv<-numeric(m)
  
  #h<-seq(from=int[1], to =int[2], length=m)
  
  for(l in 1:m) h[l]<-int[1]+(l-0.5)*(int[2]-int[1])/m
  
  for(k in 1:m){
    
    cv[k]<-0
    
    for(j in 1:n){
      
      x1<-x[-j]
      
      y1<-y[-j]
      
      num<-0
      
      denom<-0
      
      est[j]<-0
      
      for(i in 1:(n-1)){
        
        ui<-(x[j]-x1[i])/h[k]
        
        #kui<-dnorm(ui)
        
        kui<-bwkern(ui)
        
        num<-num+kui*y1[i]
        
        denom<-denom+kui}
      
      num<-num/h[k]
      
      denom<-denom/h[k]
      
      if (denom != 0) {est[j]<-num/denom}
      
      else {est[j]<-0}
      
      cv[k]<-cv[k]+(y[j]-est[j])^2}
    
    cv[k]<-cv[k]/(n)}
  
  cvres<-data.frame(h, cv)
  
  return(cvres)

}
