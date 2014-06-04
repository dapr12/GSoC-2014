

denfestimator<-function (dist, nk, h, logdensity=FALSE){
  
   
  denest<- 0 
  
   
  for(i in 1:nk){      
    
    
    denest<- denest + kern(dist[i]/h)
    
  }
  
  
  denest<- denest/nk
  
  
  return(denest)
  
  if (logdensity== TRUE) 

	{
	
  for(i in 1:nk){      
    
    
    denest<- denest + kern(dist[i]/h)
    
  }
  
  
  logdenest<- log(denest/nk)
  

	return(logdenest) 


	}
  
}


