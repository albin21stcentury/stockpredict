library(Quandl)
Quandl.api_key('TWj5VZn6WpjXBstiWUxB')
setwd("C:/Users/user/Desktop") # change as needed
temp<-read.csv("nseraw.csv")
a<-temp$Codes
b<-temp$Names
name<-as.character(a[1])
name
#name
#head(temp[1])
cl=Quandl(name, start_date='2014-10-19', end_date='2018-01-19')
plot(x=c[,1],y=cl[,2])
# This is how we can update csvs use a loop instead for all csvs
?strsplit
name=as.character(name)
name=paste(name,".csv")
name=strsplit(name,'/')
name=name[[1]][2]

write.csv(
  x = c,
  file = name,
  row.names = FALSE)
name

