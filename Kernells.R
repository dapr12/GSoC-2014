#"gaussian", "epanechnikov", "Uniform", "triangular", "biweight", "cosine", "optcosine"
kernells <- function (u, type.Kernell = "Kernell.Gaussian") 
{
  
  tab = list("Kernell.Gaussian", "Kernell.Epanechnikov", "Kernell.Uniform", "Kernell.Triangular", "Kernell.Biweight",
             "Kernell.Triweight", "Kernell.Tricube","Kernell.Cosine", "Kernell.Logistic")
  
  type.i = pmatch(type.Kernell, tab)
  
  if (is.na(type.i)) # Choose by default the Gaussian Kernel
    
   { Kernell <-  (1/sqrt(2*pi))*exp((-1/2)*(u^2)) }
  
  else {
    
    if (type.i == '1') {
      
      if((u>-1) && (u<1)) ( Kernell <- (1/sqrt(2*pi))*exp((-1/2)*(u^2)) ) 
     
      else Kernell <- 0 }
    
    if (type.i == '2') {
      
      if ((u>-1) && (u<1)) ( Kernell<- (3/4)*(1-u^2) )
    
      else Kernell <- 0}
    
    if (type.i == '3') {
      
      if((u>-1) && (u<1)) ( Kernell <- 1/2 )
      
      else Kernell <- 0}    
    
    if (type.i == '4') {
      
      if((u>-1) && (u<1)) ( Kernell <- ( 1 - abs(u)) )
      
      else Kernell <- 0}  
    
    if (type.i == '5') {
      
      if((u>-1) && (u<1)) ( Kernell <- (15/16) * ( 1- u^2 )^2 )
      
      else Kernell <- 0}  
    
    if (type.i == '6') {
      
      if((u>-1) && (u<1)) ( Kernell <-  (35/32) * (1 - u^2)^3 )

      else Kernell <- 0}  
    
    if (type.i == '7'){         
      
      if((u>-1) && (u<1)) ( Kernell <- (70/81) * (1 - abs(u)^3)^3 )
      
      else Kernell <- 0}  
                              
    if (type.i == '8') {        
            
      if((u>-1) && (u<1)) ( Kernell <- (pi/4)*cos((pi/2)*u) )
      
      else Kernell <- 0}  
                                                        
   if (type.i == '9') {        
                                                          
     if((u>-1) && (u<1)) ( Kernell <- 1/(exp(u) + 2 + exp(-u)) )
     
     else Kernell <- 0}                                                                              
                        
   else Kernell <- 0 
    
  }
  
  return(Kernell)

}


#y=qnorm(seq(.1,.9,len=100))
#kernells(y, "Gaussian")
#kernells(y, "Tricube")



