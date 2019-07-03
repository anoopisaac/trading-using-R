library(jsonlite)
macd_data <- fromJSON("https://www.alphavantage.co/query?function=MACD&symbol=ASIANPAINT.NS&interval=15min&series_type=close&outputsize=full&apikey=THY47BYOS2TUCLUX")
macd_tech_list<-macd_data[["Technical Analysis: MACD"]]
macd_tech_data <- data.frame(matrix(unlist(macd_tech_list), nrow=length(macd_tech_list), byrow=T))
colnames(macd_tech_data)<-c("signal",'history','macd')

intraday_data <- fromJSON("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=ASIANPAINT.NS&interval=15min&outputsize=full&apikey=THY47BYOS2TUCLUX")
intr_tech_list<-intraday_data[["Time Series (15min)"]]
intr_tech_data <- data.frame(matrix(unlist(intr_tech_list), nrow=length(intr_tech_list), byrow=T))
colnames(intr_tech_data)<-c("open",'high','low','close','volume')

#adding time to macd, even though its est
tick_times<-names(intr_tech_list)
for(i in 1:length(tick_times)){
  print(tick_times[i])
  intr_tech_data[i,'time']=tick_times[i]
}

#making  both sizes same to be copied
intr_tech_data_699<-intr_tech_data[1:699,]

macd_tech_data$close<-intr_tech_data_699$close
macd_tech_data$time<-intr_tech_data_699$time
#reversing the data
macd_tech_data<- macd_tech_data[seq(dim(macd_tech_data)[1],1),]
macd_tech_data$time<-as_datetime(macd_tech_data$time)+ minutes(+570)


