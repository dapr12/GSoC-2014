
cvopt<-function(x, y, int=c(0.0001, 0.5)){
	
	n<-length(x)

	est<-numeric(n)

	x1<-length(n-1)

	y1<-length(n-1)

	m<-25

	h<-numeric(m)

	cv<-numeric(m)

	#h<-seq(from=int[1], to =int[2], length=m)
	
	PSE<-matrix(0, length(x) ,m)
	
	for(l in 1:m)
		{

			 h[l]<-int[1]+(l-0.5)*(int[2]-int[1])/m

		}
	

	for(k in 1:m){
		
		cv[k]<-0
	
	for(j in 1:n){

		x1<-x[-j]

		y1<-y[-j]

		est[j]<-0

	for(i in 2:(n-1))
			{
		
			ui<-(x[j]-x1[i])/h[k]

			kui<-bwkern(ui)	#Modify Select Kernell Options

			est[j]<-est[j]+(x1[i]-x1[i-1])*kui*y1[i]

			}

		est[j]<-est[j]/h[k]

		PSE[j,k] <- (y[j]-est[j])^2

		cv[k]<-cv[k]+(y[j]-est[j])^2

		}

		cv[k]<-cv[k]/(n)
		
		}

		#print(PSE)

		meanpse<- apply(PSE, 2, mean )

		meanpse<-as.data.frame(meanpse)

		medianpse<- apply(PSE, 2, median )

		medianpse<-as.data.frame(medianpse)

		cvres<-data.frame(h, meanpse, medianpse)
	
	return(cvres)
}

}
