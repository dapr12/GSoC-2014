bwkern<-function(u){

	if((u>-1) && (u<1))

	{
		bwkern<-(15/16)*(1-u^2)^2
	}
	else  { bwkern<-0 }
	
	return(bwkern)
	}


epkern<-function(u){
	
	if((u>-1) && (u<1))
	
		(epkern<-(3/4)*(1-u^2))

	else  (epkern<-0)
	
	return(epkern)
	
	}

epk4<-function(u){

	if((u>-1) && (u<1))

	(epk4<-(525/32)*((3/35)-(1/5)*u^2)*(1-u^2))

	else  (epk4<-0)

	return(epk4)
}


bwk4<-function(u){

	if((u>-1) && (u<1))
	
	(bwk4<-(2205/64)*((1/21)-(1/7)*u^2)*(1-u^2)^2)

	else  (bwk4<-0)

	return(bwk4)
}
