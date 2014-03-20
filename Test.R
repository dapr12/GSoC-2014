# Remove all the variables 
# and load the fda package

rm(list=ls(all=TRUE))
library(fda)
library(ks)
library(MASS)

# The data set we consider is the data set avaliable in the fda package
# for the Canadian Weather. It contains data for 35 Canadian weather stations
# We do a first analysis and we consider four of them: Montreal, Edmonton, Pr. Rupert and Resolute. 
# We plot the mean temperature for the four weather stations. 

figStns <- c('Montreal', 'Edmonton', 'Pr. Rupert', 'Resolute')
figTemp <- CanadianWeather$dailyAv[, figStns, 'Temperature.C']

#Create a Fourier Basis for each day of the year. 
Temp.fourier  <- create.fourier.basis(c(0, 365), 13)
figTemp.fd <- Data2fd(day.5, figTemp, Temp.fourier)

plot(figTemp.fd, day.5, axes=FALSE, col=1, lwd=2,   xlab='', ylab='Mean Temperature (deg C)')
axis(2, las=1)
axisIntervals(labels=monthLetters)

monthIndex <- rep(1:12, daysPerMonth)
monthAvTemp <- matrix(NA, 12, 4, dimnames=list(month.abb, figStns))
for(i in 1:4)
  monthAvTemp[, i] <- tapply(figTemp[, i], monthIndex, mean)
StnLtrs <- substring(figStns, 1, 1)

matpoints(monthMid, monthAvTemp, pch=StnLtrs, lwd=2, col=1)

legend('topleft', paste(figStns, ' (', StnLtrs, ')', sep=''),
       lty=1:4, col=1, lwd=2)

# The mean temperatures are plotted as smooth curves. From the graph we can 
# see that Montreal, has the warmest summer temperature and it has a temperature
# patter that appears to be sinusoidal. 

 

# From the same graph we can see that Prince Rupert has the smalles amount 
# of annual variation in temperature. 

# Now we will focus in the precipitation for Prince Rupert (in mm), so we get this data
# from the same dataset  

P.RupertPrecip <- CanadianWeather$dailyAv[ ,'Pr. Rupert', 'Precipitation.mm']

plot(day.5, P.RupertPrecip, axes=FALSE, pch='.', cex=2, xlab="", ylab="Preciipitation (mm)")
axis(2, las=1)
axisIntervals(labels=monthLetters)

# Now we smooth the data with the value of lambda that minimizes the GCV 
# We create a constant basis all the year along. 

Lbasis       <- create.constant.basis(c(0,365), axes=list("axesIntervals"))
Lcoef        <- matrix(c(0,(2*pi/365)^2,0),1,3)
bfdobj       <- fd(Lcoef,Lbasis)
bwtlist      <- fd2list(bfdobj)
harmaccelLfd <- Lfd(3, bwtlist)

TempBasis <- create.fourier.basis(c(0, 365), 13)
P.Rupert.Prec.fd <- smooth.basisPar(day.5, P.RupertPrecip,
                 TempBasis, harmaccelLfd, lambda=10^7)$fd

# And we plot the smoothed curve. 

plot(day.5, P.RupertPrecip, axes=FALSE, pch='.', cex=2, xlab="", ylab="Preciipitation (mm)")
axis(2, las=1)
axisIntervals(labels=monthLetters)
lines(P.Rupert.Prec.fd, lwd=2)

 

# At this point we smooth functional data (precipitation for the Princes Ruper t
# station using a fourier basis. I decided to smooth the data using the Fourier
# basis because Fourier Basis is the usual choice for periodic
# functions, and the spline basis system (and bsplines in particular) tends to serve
# well for nonperiodic functions.

# Now we will compute the functional principal components.

logprecav <- CanadianWeather$dailyAv[dayOfYearShifted, , 'log10precip']
dayrange  <- c(0,365)
daybasis  <- create.fourier.basis(dayrange, 365)

Lcoef        <- c(0,(2*pi/diff(dayrange))^2,0)
harmaccelLfd <- vec2Lfd(Lcoef, dayrange)

lambda      <- 1e6
fdParobj    <- fdPar(daybasis, harmaccelLfd, lambda)
logprec.fit <- smooth.basis(day.5, logprecav, fdParobj)
logprec.fd  <- logprec.fit$fd

#  Functional Principal Component Analysis with 2 components

nharm <- 2
logprec.pcalist <- pca.fd(logprec.fd, nharm)
plot.pca.fd(logprec.pcalist)

 	 

# The plot shows the two principal component functions by displaying the mean
# curve along +’s and -’s indicating the consequences of adding and subtracting a
# small amount of each principal component.

# We do this because a principal component represents variation around the mean.
# The first principal component account for 87.4% of the variation.

# Finally we plot the scores for the two rotated principal component functions
# We can identify two central clusters 

logprec.rotpcalist <- varmx.pca.fd(logprec.pcalist)
rotpcascores <- logprec.rotpcalist$scores

plot(rotpcascores[,1], rotpcascores[,2], type="p", pch="o",
     xlab="Rotated Harmonic I", ylab="Rotated Harmonic II")
 

ii.	Write an R Function to simulate a set of functional data using a self-selected procedure.


# To simulate functional Data, we want to project some random pairs {x_i,y_i}  
# into a spline basis. Then, we are assured to get a draw from a (smooth) GP. 

# So we write a function to compute sigma

Sigma<-function( x,y,l=1){
    Sigma<-matrix(rep(0,length(x)*length(y)),nrow=length(x))
    for(i in 1:nrow(Sigma)){
        for (j in 1:ncol(Sigma)) Sigma[i,j]<-exp(-1/2*(abs(x[i]-y[j])/l)^2)
    }
    return(Sigma)
}

# The standard deviation of the noise

samples	<-50
draws		<-100
x.star	<-seq(-5,5,len=draws)
nval		<-4
f		<-data.frame(x=seq(-5,5,l=nval),y=rnorm(nval,0,10))
sigma.n	<-0.2

# Recalculate the mean and covariance functions

k.xx		<- Sigma(f$x,f$x)
k.xxs		<- Sigma(f$x,x.star)
k.xsx		<- Sigma(x.star,f$x)
k.xsxs	<- Sigma(x.star,x.star)
f.bar.star	<-k.xsx%*%solve(k.xx+sigma.n^2*diag(1,ncol(k.xx)))%*%f$y
cov.f.star	<-k.xsxs-k.xsx%*%solve(k.xx+sigma.n^2*diag(1,ncol(k.xx)))%*%k.xxs

values<-matrix(rep(0,length(x.star)*samples),ncol=samples)

for (i in 1:samples) 
	 values[,i]<-mvrnorm(1,f.bar.star,cov.f.star)
values<-cbind(x=x.star,as.data.frame(values))

matplot(x=values[,1],y=values[,-1],type="l", main='Simulated Functional Data', xlab='Interval', ylab='Value')

