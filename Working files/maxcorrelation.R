corvalues<-read.csv("corvalues.csv",header=T)
correlation<- function(choice){
 # corvalues = read.table(paste("stocks/",choice,".csv",sep = ""),sep = ",",header = TRUE)
#corvalues<-read.csv("corvalues.csv",header=T)
names<-corvalues[,1]
choice=as.character(choice)

index<-which(corvalues[,1]==choice)
subvalues<-as.numeric(corvalues[index,2:2141])
#head(subvalues)
#max(subvalues)
#simstocks<-which((subvalues)>.90)
#??exclude
#for(i in simstocks)
#{
#  if(names[i]!="RADAAN")
 #   print(names[i])
#}
#length(simstocks)
##FInding Top 5
max<-which(subvalues==max(subvalues))
subvalues[max]=0
vect=c()
#vect=corvalues[max,1]
for(i in 1:5){
max<-which(subvalues==max(subvalues))
vect[i]=as.character(corvalues[max[1],1])
subvalues[max]=0
}

return (vect) # top 5 correlated stocks
}
#x[order(x)[1:5]]
#tail(sort(a),5)
correlation("INV20")
