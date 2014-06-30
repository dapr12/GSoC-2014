rm(list=ls(all=TRUE))
library('fda')
data(growth)
a <- growth$age
b <- growth$hgtm[,1:39]
c <- growth$hgtf[,1:54]
Ma<-matrix(0, 31,39)
Fm<-matrix(0, 31,54)
for( i in 1:39) Ma[,i] <-growth$hgtm[,i]
for( j in 1:54) Fm[,j] <-growth$hgtf[,j]
as.data.frame(Ma)
as.data.frame(Fm)
colnames(Ma)<-paste("Male",1:39, sep= " ")
colnames(Fm)<-paste("Female",1:54, sep= " ")
mydata <- list(Age= a, Male=Ma, Female=Fm)
dput(mydata, file = "growth.dta") 
Gwd<-dget(file = "growth.dta") 
matplot(Gwd$Age, Gwd$Male, type="l")
matplot(Gwd$Age, Gwd$Female, type="l")
