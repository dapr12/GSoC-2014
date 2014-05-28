
rm(list=ls(all=TRUE))

#Set Working Directory

setwd("C:/Work")
getwd() 

#Load the Data 
Data <- read.csv("my_file.txt" ,header = FALSE, sep = "")
#names(Data)
Data<-as.data.frame(Data)

NewData<-Data[ ( Data$V4 == "1") | ( Data$V4 == "2")  ,]
write.matrix(NewData, file = "NewData.dat", sep = " ")

Data2 <- read.table("NewData.dat", header=TRUE)
dim(Data2)
head(Data2)
Data2<-as.data.frame(Data2)

DatanoNA <-na.omit(Data2)
head(DatanoNA)

months<-seq(1,12,1)
years<-seq(840,940,1)

AustralianRainFall<-array(0, dim=c(length(stations) , length(years), length(months) , 31)) 

for ( i in (stations) )
{
  for( j in (years) )
  {
    if ( j == DatanoNA[ (DatanoNA$V1==i) & (DatanoNA$V2 == j ),] )
    {  
      for ( k in (months)  ) 
      {
        if ( k == DatanoNA[ (DatanoNA$V1==i) & (DatanoNA$V2 == j ) & (DatanoNA$V3 == k),] )
        {  
           Stationi<-DatanoNA[(DatanoNA$V1==i) & (DatanoNA$V2 == j ) & (DatanoNA$V3 == k),]
           MeanS<- apply(Stationi, 2, mean )
           AustralianRainFall[i,j,k,] <- as.data.frame(MeanS35[6:36])[,1]
        }       
      }
    }    
  }
}
