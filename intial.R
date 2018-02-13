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
c=Quandl(name, start_date='2017-10-19', end_date='2018-01-19')
c