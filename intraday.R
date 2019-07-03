library(jsonlite)
macd_data <- fromJSON("https://www.alphavantage.co/query?function=MACD&symbol=ASIANPAINT.NS&interval=15min&series_type=close&outputsize=full&apikey=THY47BYOS2TUCLUX")
macd_tech_data<-macd_data[["Technical Analysis: MACD"]]
macd_tech_data <- data.frame(matrix(unlist(macd_tech_data), nrow=length(macd_tech_data), byrow=T))
colnames(macd_tech_data)<-c("signal",'history','macd')

intraday_data <- fromJSON("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=ASIANPAINT.NS&interval=15min&outputsize=full&apikey=THY47BYOS2TUCLUX")
intr_tech_data<-intraday_data[["Time Series (15min)"]]
intr_tech_data <- data.frame(matrix(unlist(intr_tech_data), nrow=length(intr_tech_data), byrow=T))
colnames(intr_tech_data)<-c("open",'high','low','close','volume')


mydate = strptime('16/Oct/2005:07:51:00',format='%d/%b/%Y:%H:%M:%S')
mydate + minutes(15)
