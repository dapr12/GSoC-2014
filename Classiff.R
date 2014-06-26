rm(list=ls(all=TRUE))
library('KernSmooth')
library('ks')
library('sm')
library('sampling')
library('mda')
library('fda')
library('caTools')
library('MASS')
library('fda.usc')
source("Functions.R")
source("FerratyCode.R")

Protein <- read.csv("protein.asc" ,header = FALSE, sep = "")
Protein <- as.data.frame(Protein)
plot(Protein$V1, type="b")

whtspec <- read.csv("whtspec.asc" ,header = FALSE, sep = "")
whtspec <- as.data.frame(whtspec)
matplot(whtspec, type="l" )


wave.length <- seq(1, 701, by = 1)
y <- as.numeric(Protein > median(Protein[ , 1]))

whtspec.fdata <- fdata(mdata = whtspec, argvals = wave.length, names
                       = list(main = "Wheat data set", xlab = "Wave length (nm)", ylab =
                                "NIR spectra ( log(1/R) )"))

First.Deriv<- fdata.deriv(whtspec.fdata, nderiv = 1)

par(mfrow=c(1,2))
plot(whtspec.fdata, col = y+1)
plot(fdata.deriv(whtspec.fdata, nderiv = 1), col = y+1, main =
       "Wheat data set. 1st derivative")

#Second Derivative
plot(fdata.deriv(whtspec.fdata, nderiv = 2), col = y+1, main=
       "Wheat data set. 2nd derivative")


First.Deriv<- fdata.deriv(whtspec.fdata, nderiv = 1)
names(First.Deriv)
First.Deriv$names
First.Deriv$data
Group1<-which(y==1)
Group2<-which(y==0)

GroupData1<-matrix(0,50,701)
GroupData2<-matrix(0,50,701)


for ( i in 1:50)
{
  GroupData1[i,]<- First.Deriv$data[Group1[i],]
  lines(First.Deriv$data[Group1[i],])
}



plot(First.Deriv$data[2,], type="n", xlim=c(), ylim=c(-0.005,0.015))
abline(h=0)

for ( i in 1:50)
{
  
  GroupData2[i,]<-First.Deriv$data[Group2[i],]
  lines(First.Deriv$data[Group2[i],])
}


par(mfrow=c(1,2))
matplot(t(GroupData1), type="l")
matplot(t(GroupData2), type="l")

GroupData<-cbind(t(GroupData1),t(GroupData2))

tt=1:701
Deltat=tt[2]-tt[1]
longt=length(tt)

#Size of full data set (number of curves available)
nall=100

#Data matrix : put your data in this matrix
XXdata=matrix(0,nrow=nall,ncol=longt)
XXdata= GroupData

#Class lables (0 or 1): put your class lables here
Y1=rep(0,50)
Y2=rep(1,50)
Y=c(Y1,Y2)

#Compute indices of curves with clas lable 0 or 1, and put the curves from class lable 0 or 1  in two separate metrices
indClass1=which(Y==0)
indClass0=which(Y==1)

indClass1All=indClass1
indClass0All=indClass0

XXdata1=XXdata[indClass1,]
XXdata2=XXdata[indClass0,]

#Create a matrix that puts first all the data from group 0 and then all the data from group 1
XX=rbind(XXdata1,XXdata2)
nall=dim(XX)[1]

#indices of class label 0 or 1
indClass1All=1:nrow(XXdata1)
indClass0All=1:nrow(XXdata2)+nrow(XXdata1)

#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Choose test and training samples
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

#choose randomly wich observations will be the test data. ntest=number of test data we choose
ntest=10
s=srswor(ntest,nall)
indtest=(1:nall)[s==1]
indClass1=setdiff(indClass1All,indtest)
indClass0=setdiff(indClass0All,indtest)

indtest=sort(indtest)

#Training sample size
ntrain=nall-length(indtest)

as.array(indtest)
XXtest=XX[indtest,]
XX=XX[-indtest,]

#Rescale the data in some way to avoid too many numerical problems. For example, do it this way:
varXX=var(as.vector(XX))
muX=mean(as.vector(XX))
XX=(XX-muX)/sqrt(varXX)
XXtest=(XXtest-muX)/sqrt(varXX)

#relabel the indices

indClass1=1:length(indClass1)
indClass0=1:length(indClass0)+length(indClass1)

#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Start procedure
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

# Calculate means in each class
barXClass1=XX[indClass1[1],]
for(i in indClass1[2:length(indClass1)])
  barXClass1=barXClass1+XX[i,]

barXClass0=XX[indClass0[1],]
for(i in indClass0[2:length(indClass0)])
  barXClass0=barXClass0+XX[i,]

barXClass1=barXClass1/length(indClass1)
barXClass0=barXClass0/length(indClass0)

#Construct PC basis estimators based on the data from the two groups centered to their mean
ZZ=XX[indClass0,]-outer(rep(1, length(indClass0)), barXClass0)
ZZ=rbind(ZZ,XX[indClass1,]-outer(rep(1, length(indClass1)), barXClass1))
compo=prcomp(ZZ)
phi=(compo$rotation)
phi=phi/sqrt(Deltat)
lambda=(compo$sdev)
lambda=lambda^2*Deltat

# npsir is the maximum number of terms considered in our sums
cumul=cumsum(lambda)/sum(lambda)
npsir=min(which(cumul>1-1/ntrain^2))
npsir=min(npsir,ntrain/2)

#Difference between means + projections of that difference on the basis fucntions. Plays the role of the mu_j's in the paper
DiffbarX=barXClass1-barXClass0
hatmuj=DiffbarX%*%phi[,]*Deltat

#B5fold = Number of data partitions created to compute the cross-valdiation criterion
B=50
K=5
KKCV=round(ntrain/K)

#ind5foldCV: indices of the samples Xstarb1,...,Xstarbm for b=1..B
ind5foldCV=matrix(0,nrow=B,ncol=KKCV)
for(b in 1:B)
{
  s=runif(KKCV,0,ntrain)
  s=ceiling(s)
  ind5foldCV[b,]=s
}

#To save time later, compute now the eigen functions and eigen values for each of the CV samples and save them in matirces and vectors
for(b in 1:B)
{
  
  i=ind5foldCV[b,]
  XXCV=ZZ[-i,]
  compoCV=prcomp(XXCV)
  phiCV=(compo$rotation)
  eval(parse(text=paste("phiCV",b,"=phiCV/sqrt(Deltat)", sep = "")))
  lambdaCV=(compoCV$sdev)
  eval(parse(text=paste("lambdaCV",b,"=lambdaCV^2*Deltat", sep = "")))
  
}

#------------------------------------------------------------
#------------------------------------------------------------
# METHOD 1: Estimate psir computed through the PC components
#------------------------------------------------------------
#------------------------------------------------------------

#Compute the numbers of terms in the sum using CV. 
#Take the global minimum among thr first two local minima

CV=rep(0,npsir)

#nbcv is the number of local minima found so far
nbCV=0

#npsirA is the numebr of components we keep
npsirA=1

for(j in 1:npsir)
{
  if(nbCV<2)
  {
    CV[j]=CVpsi(XX,j)
    
    if(j>2)
    {
      #check your CV function to see if CV is flat in some parts. If it is flat you can only find the mins if you use a non strict inequality
      if((CV[j-1]<CV[j-2])&(CV[j-1]<=CV[j]))
      {
        nbCV=nbCV+1
        if(nbCV<2)
        {
          npsirA=j-1
          minCV=CV[j-1]
        }
        
        if((nbCV==2)&(CV[j-1]<minCV))
        {  npsirA=j-1
           minCV=CV[j-1]
        }
        
        
      }#end if CV[j-1]
    }#end if j>1
  }#end while j
  
}#end for j


#Compute the function hatpsir
hatpsir=0*phi[,1]
for(j in 1:npsirA)
  hatpsir=hatpsir+hatmuj[j]/lambda[j]*phi[,j]


#---------------------------------------------------
#---------------------------------------------------
# METHOD 2: Estimate psir  computed through MPLSR
#---------------------------------------------------
#---------------------------------------------------


#Compute number of components to keep by CV
Y=matrix(0,nrow=ntrain,ncol=1)
Y[indClass1]=rep(1,length(indClass1))

CV=rep(0,npsir)

#m is the numebr of components we keep
m=1

for(j in 1:npsir)
{
  if(nbCV<2)
  {
    CV[j]=CVmplsr(XX,Y,j)
    
    if(j>2)
    {
      #check your CV function to see if CV is flat in some parts. If it is flat you can only find the mins if you use a non strict inequality
      if((CV[j-1]<CV[j-2])&(CV[j-1]<=CV[j]))
      {
        nbCV=nbCV+1
        if(nbCV<2)
        {
          m=j-1
          minCV=CV[j-1]
        }
        
        if((nbCV==2)&(CV[j-1]<minCV))
        {	m=j-1
          minCV=CV[j-1]
        }
        
        
      }#end if CV[j-1]
    }#end if j>1
  }#end while j
  
}#end for j

#Compute the resulting beta_r
hatpsipls=mplsr(XX,Y,m)$COEF


#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Apply the centroid classifier to the test data using each of the functions psir and see compute the number of errors made by the classifier
#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------


BBB=length(indtest)
CV1=0
CVPLS=0


for(b in 1:BBB)
{
  
  #Test curve
  X=XXtest[b,]
  
  
  
  #project the data on the estimated psir function
  Xproj=X%*%hatpsir
  XXproj=XX%*%hatpsir
  
  
  TofX=(Xproj-mean(XXproj[indClass1]))^2-(Xproj-mean(XXproj[indClass0]))^2
  
  #misclassification rate
  if((TofX>=0)&(is.element(indtest[b], indClass1All)))
    CV1=CV1+1
  if((TofX<=0)&(is.element(indtest[b], indClass0All)))
    CV1=CV1+1
  
  
  
  #project the data on the  PLS
  Xproj=X%*%hatpsipls*Deltat
  XXproj=XX%*%hatpsipls*Deltat
  
  TofX=(Xproj-mean(XXproj[indClass1]))^2-(Xproj-mean(XXproj[indClass0]))^2
  
  
  #misclassification rate
  if((TofX>=0)&(is.element(indtest[b], indClass1All)))
    CVPLS=CVPLS+1
  if((TofX<=0)&(is.element(indtest[b], indClass0All)))
    CVPLS=CVPLS+1
  
}
