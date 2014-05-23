
rm(list=ls(all=TRUE))

distance <- function (x, y = NULL, method = "euclidean", p=2) 
{
  
    if (is.vector(x)) 
    
      x <- matrix(x, nrow = 1)
  
    if (!is.null(y)) {
    
      if (is.vector(y)) 
      
        y <- matrix(y, nrow = 1)
    
      n <- nrow(y)
    
      nn <- nrow(x)
    
      mdist <- as.matrix(dist(rbind(x, y), method = method, diag = TRUE, upper = TRUE, p = p))[1:nn, (nn + 1):(nn +n)]
  }
    
  else mdist <- as.matrix(dist(x, method = method, diag = TRUE, upper = TRUE, p = p))
    
  if (is.vector(mdist)) 
    
    mdist <- matrix(mdist, nrow = 1)
    
  return(mdist)
    
}


#a1<-seq(0,1,by=.01)
#a2=rnorm(length(a1),sd=0.2)
#f1<-(sin(2*pi*a1))+rnorm(length(a1),sd=0.2)
#nc<-10
#np<-length(f1)
#tt=seq(0,1,len=101)
#mdata<-matrix(NA,ncol=np,nrow=nc)
#for (i in 1:nc) mdata[i,]<- (sin(2*pi*a1))+rnorm(length(a1),sd=0.2)

#d<-distance(mdata)
#d





