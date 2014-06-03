

denest <- function(x, xk, m, nk, h){
	
	denest <- 0
	
	temp<-0
	
	for(i in 1:nk){
	
		ssqd<-0
	
	for(j in 1:m){
	
		ssqd<-ssqd+(x[j]-xk[i,j])^2
	
		}
	
	dist<-sqrt(ssqd)
	
	print(dist)
	
	denest<- denest + kern(dist/h)
	
	}
	
	denest<- denest/nk
  
   return(denest)
}

	
