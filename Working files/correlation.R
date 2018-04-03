
##
##Trying to store data of close 2141 variables
#removed stocks with less than 100 rows for data that will result in inaccuracy in correlation
setwd("C:/Users/user/Documents/Working - Copy/stocks - Copy")
vect=list()
files <- Sys.glob("*.csv")
#files[2141]
len=length(files)

for (i in 1:len) {
  data=read.csv(files[i],header=TRUE)
#vect[[i]]=data$Close
  vect[[i]]=na.omit(data$Close)
}
##head(vect)
##vect[1]

#length((vect["AARTIDRUGS.csv"][[1]]))

##trying writing to csv file
setwd("C:/Users/user/Documents/Working - Copy")
#a=length(vect[[1]])
#a
#b=length(vect[[2]])
#b
#min
#min=min(a,b)
#cor(vect[[1]][1:min],vect[[2]][1:min])
allvalues=list()
for(j in 1:len){
a=vect[[j]]
l1=length(vect[[j]])
values=c()
for(i in 1:len){
  b=vect[[i]]
  l2=length(vect[[i]])
  min=min(l1,l2)
  values[i]=cor(a[1:min],b[1:min])
}
allvalues[[j]]=values
##write.table(values,file="hello.csv",append=T,sep=',',row.names=T,col.names=F)
}

head(allvalues)
df=data.frame(files)
#head(df)
for(i in 1:len){
df=data.frame(df,allvalues[[i]])
}
head(df)
write.csv(df,file="corvalues.csv")
