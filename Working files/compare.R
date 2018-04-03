compare<- function(choice){
  # corvalues = read.table(paste("stocks/",choice,".csv",sep = ""),sep = ",",header = TRUE)
  #corvalues<-read.csv("corvalues.csv",header=T)
  names<-corvalues[,1]
  choice=as.character(choice)
  
  index<-which(corvalues[,1]==choice)
  #index<-which(corvalues[,1]=="20MICRONS")
  
  index2<-which(corvalues[,1]=="NIFTY_50")
  index2
  index
  #subvalues<-as.numeric(corvalues[index,2:2141])
  value<-corvalues[index,1+index2]
  value
  a<-c()
  if(abs(value)>.80)
    a[1]<-"high"
  else if(abs(value)>.60)
    a[1]<-"medium"
  else
    a<-"low"
  if(value<0)
    a[2]<-"negative"
  else
    a[2]<-"positive"

  return (a) # top 5 correlated stocks
}
#compare(20MICRONS)
