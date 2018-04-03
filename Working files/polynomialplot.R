library(forecast)
library(fpp)


polynomialplot <- function(choice){
  
  
     
  setwd("C:/Users/user/Documents/Working - Copy") 
     Stock = read.table(paste("stocks/",choice,".csv",sep = ""),sep = ",",header = TRUE)

    # Stock = read.csv("stocks/AARTIIND.csv",header = TRUE)
      ##albin added
      ##Stock$Close=rev(Stock$Close)
      #converting to month is not needed when you download monthly data
      #Stock$Date = as.Date(Stock$Date)
      #Stock_monthly = agregate(Stock$close,by = list(Date=format(Stock$Date,"%Y%m")),mean)
      
      #converting to ts
       subdata<-Stock$Close[18:2532]#take till 2008 jan 1 from 2018 feb last
       subdata=rev(subdata)
       temp<-c()
       j=1
       i=1
       c=1
       #head(subdata)
       for(j in 1:122)#122 months from 2008 1 to 2018 2
       {
         if(c%%2==1)
         {
           temp[j]<-mean(subdata[i:i+19])#taking average to convert to months
           i=i+20
         }  
         else
         {
           temp[j]<-mean(subdata[i:i+20])
           i=i+21
         }
         c=c+1
       }
       temp=na.omit(temp)
      tryCatch({tsStock= ts(temp,start=c(2008,1),end=c(2018,2),frequency=12)})
    #  tryCatch({ tsStock = ts(Stock$Close,start=c(2008,1),end=c(2018,2),frequency=12)})
      ?ts
     # which(is.nan(log(tsStock)))
       #tsStock
 #     tsStock1
      max_value = max(tsStock)
      min_value = min(tsStock)
      
      #Generalize function as Polynomial "trend" (TREND = toStocktrend1)
      t1 = seq(2008,2018,length=length(tsStock))
      t12 = t1^7
      polyStock = lm(tsStock ~ t1 + t12)
      tsStocktrend1 = ts(polyStock$fit,start=c(2008,1),frequency=12)
  #    tsStocktrend1
      
      #Decompose a time series into seasonal, trend and irregular components based on loess method
      # get second generalized "trend" function (TREND = tsStocktrend2)
      stlStock = stl(tsStock,s.window = "periodic")
   #   plot(stlStock,col="blue",lw=2)
      tsStocktrend2 = stlStock$time.series[,2]
   #   plot(forecast(stlStock))
  #    abline(v=2018.2,lty = 3)
      
     # ?abline
      #albin added
    #  tsStocktrend1=abs(tsStocktrend1)
      tsStocktrend1=abs(tsStocktrend1)
      #start predicting #
      #based on polynomial function#
      HWStock1_ng = HoltWinters(tsStocktrend1,gamma=FALSE)
      HWStock1 = HoltWinters(tsStocktrend1)
      NETfit1 <- nnetar(tsStocktrend1)
      autofit1 = auto.arima(tsStocktrend1)
      #fit12 <- arima(tsStocktrend1,order=c(1,0,0),list(order=c(2,1,0),period=12),optim.method="Nelder-Mead")
      fit11 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)
 
   
    #    tsStocktrend1
         which(is.nan(log(tsStocktrend1)))
       stlStock1 = stl(tsStocktrend1,s.window="periodic")
      
      tobereturned <- plot(forecast(autofit1,h=24),xlim=c(2008,2020.10),ylim=c(min_value-30,max_value+50),lw=2,col="red",xlab="Time",ylab="Stock Price",main="Predictions of the polynomial trend")
      
      lines(forecast(stlStock1,h=24)$mean,col="red",lw=2)
      lines(tsStock,lw=3)
      lines(forecast(fit11,h=24)$mean,col="orange")
      lines(forecast(NETfit1,h=24)$mean,lw=3,lty="longdash",col="brown")
      lines(predict(HWStock1_ng,n.ahead=24),lw=2,col="green")
   #added down
      lines(predict(autofit1,h=24)$mean,lw=2,col="blue")
      #lines(forecast(fit12,h=24)$mean, lw=2,col="purple")
      lines(predict(HWStock1,n.ahead=24,prediction.interval = T, level = 0.95)[,1],lw=2,col="green")
      
      lines(predict(HWStock1,n.ahead = 24,prediction.interval=T,level = 0.95)[,2],col="green")
      
      lines(predict(HWStock1,n.ahead = 24,prediction.interval = T, level = 0.95)[,3],col="green")
      
      legend("bottomleft",legend=c("Actual Function","Polynomial Trend","Prediction - Holt Winters","Prediction - Arima (auto)","Prediction - Neural Nets",
                                   "Tslm"),col=c("black","red","green","blue","brown","orange"),lw=1,cex=0.75)
      
  #    ?legend
      abline(v = 2018.2,lty=3)
      
      return(tobereturned)
}
