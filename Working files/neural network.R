library(shiny)
library (tseries)
library (xts)
library (zoo)
library (quantmod)
library (neuralnet)

#set.seed(1234);
start_date = "2013-01-01"
end_date = as.character(Sys.Date())
setwd("C:/Users/user/Documents/Working - Copy")
#GSPC_data <- as.data.frame(get.hist.quote(instrument = code_stock, start = start_date, end = end_date,
   #                           quote = c("Open","High", "Low","Close","Volume","AdjClose")))
#write.table(GSPC_data,file = "STOCKDATA.csv",sep="," ,row.names =TRUE)
GSPC_data<-as.data.frame(read.csv("stocks/ICICIBANK.csv"))
date_range <- as.data.frame(GSPC_data$Date)
normal <- function(data){
  (data - min(data,na.rm = TRUE))/(max(data,na.rm=TRUE) - min(data,na.rm=TRUE))
}
#GSPC_data=GSPC_data[1:2000,]
Open_data <- (GSPC_data$Open)
Close_data <- (GSPC_data$Close)
High_data <- (GSPC_data$High)
Low_data <- (GSPC_data$Low)
Volume_data <- (GSPC_data$Total.Trade.Quantity)
AdjClose_data <- (GSPC_data$Last)

GSPC_df <- cbind(Open_data,High_data,Low_data,Close_data,Volume_data,AdjClose_data)


GSPC_Actual <- data.frame(GSPC_df[,-6])
train_stock <- (GSPC_Actual[-(nrow(GSPC_Actual)),])


trainingoutput_stock <- as.data.frame(GSPC_Actual[-1,4])


#Column bind the data into one variable
trainingdata_stock <- as.data.frame(cbind(train_stock,trainingoutput_stock))

net_stock <- neuralnet(GSPC_Actual[-1,4]~(GSPC_Actual$Open[-(nrow(GSPC_Actual))] + GSPC_Actual$High[-(nrow(GSPC_Actual))] + GSPC_Actual$Low[-(nrow(GSPC_Actual))] + GSPC_Actual$Close[-(nrow(GSPC_Actual))] + GSPC_Actual$Volume[-(nrow(GSPC_Actual))]),
                       trainingdata_stock,err.fct = "sse", hidden=13, threshold=0.001)

testdata <- as.data.frame(GSPC_Actual[nrow(GSPC_Actual),])
net.results <- compute(net_stock, testdata)
Predicted_Close_Rate<- net.results$net.result
Close_rate<- Predicted_Close_Rate[1,1]
Close_rate

