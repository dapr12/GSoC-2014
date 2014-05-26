
criterian<-function( h, meanpse, medianpse, type.criteria= "MedianMean") 
{ 

  tab = list("MedianMean", "MedianMedian", "MaxMean", "MaxMedian") 
  
  type.i = pmatch(type.criteria, tab)
  
  if (is.na(type.i)) # Choose by default the first criteria
    { 
	mop <- median(meanpse)
	hopt<-which( h == mop) 
	
	}

     else {
	
	if (type.i == '1') {

	mop <- median(meanpse)
	hopt<-which( meanpse == mop) 
	hopt<-h[hopt]

      }

	if (type.i == '2') {

	mop <- median(medianpse)
	hopt<-which( medianpse == mop) 
	hopt<-h[hopt]


      }

	if (type.i == '3') {

	mop <- max(meanpse)
	hopt<-which( meanpse == mop) 
	hopt<-h[hopt]

      }

	if (type.i == '4') {

	mop <- max(medianpse)
	hopt<-which( medianpse == mop) 
	hopt<-h[hopt]


      }

   
	}

	return(hopt) 
	
