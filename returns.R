library(quantmod)
setwd('C:/Users/anoop/dream/running/r-stock-trading')
getwd()

#finding macd
findMacd<-function(tickData){
  closeData<-tickData[,4]
  #print(closeData)
  tmp <- to.weekly(closeData)
  print(tmp)
  #print(tmp)
  macd  <- MACD( tmp[,4], 12, 26, 9, maType="EMA" )
  #print(macd)
  return(macd)
  #print(macd)
}
tickerData=get(sprintf('%s%s','ASIANPAINT','.NS'))
tickerData<-na.omit(tickerData)
closeData<-tickerData[,4]
tmp <- to.weekly(closeData)
ema.12<-EMA(tmp[,4],12)
ema.26<-EMA(tmp[,4],26)
sma.12<-SMA(tmp[,4],12)
sma.26<-SMA(tmp[,4],26)
macd  <- MACD( tmp[,4], 12, 26, 9, maType="EMA",percent = F )
#print(tickerData)
check<-findMacd(tickerData)
plot(check)

#function to calculate return counts
profitMonths <- function(tickData) 
{
  monthly.return.data<-monthlyReturn(tickData[,4])
  greater.than.zero=which(monthly.return.data$monthly.returns>0)
  return(length(greater.than.zero))
}

profitQuarterly <- function(tickData) 
{
  quarterly.return.data<-quarterlyReturn(tickData[,4])
  print(quarterly.return.data)
  greater.than.zero=which(quarterly.return.data$quarterly.returns>0)
  return(length(greater.than.zero))
}




#init ticker symbols
tickers <- read.csv(file=file.path("nifty", "200"), header=T)

#init monthly return data
returns.data<-data.frame(matrix(ncol = 2, nrow = 0))
colnames(returns.data) <- c("symbol", "profits")



#load all returns data
count=0
for(symbol in tickers$Symbol){
#for(symbol in c('ASIANPAINT')){
  count=count+1
  tickerData=get(sprintf('%s%s',symbol,'.NS'))
  na.omit(tickerData)
  u<-profitQuarterly(tickerData)
  print('wererere')
  returns.data[count, ] <- c(symbol, as.integer(u))
}
as.numeric(returns.data[,2])->returns.data[,2]



#get all ticker data using symbols
count<-0
for(symbol in tickers$Symbol){
  getSymbols(sprintf('%s%s',symbol,'.NS'),from="2015-01-01")
}
