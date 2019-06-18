library(quantmod)
library(lubridate)

setwd('C:/Users/anoop/dream/running/r-stock-trading')
getwd()



#init ticker symbols
tickers <- read.csv(file=file.path("nifty", "200"), header=T)

#finding macd line that goes below zero when plotted in weeks
getMacdDataByTicker<-function(symbolName){
  weekData <- to.weekly(getTickerData(symbolName))
  weekMacd  <- MACD( weekData[,4], 12, 26, 9, maType="EMA",percent = F )
  return(weekMacd)
}

getLastYearReturn<-function(symbolName){
  last_day_prev_year <- floor_date(Sys.Date(), "year") - days(1)
  first_day_prev_year <- floor_date(last_day_prev_year, "year")
  print(sprintf('%s dates %s',last_day_prev_year,first_day_prev_year))
  tickerData<-getTickerData(symbolName)
  startValue=as.numeric(tickerData[first_day_prev_year,1])
  endValue=as.numeric(tickerData[last_day_prev_year,1])
  print(sprintf('%s  %s',endValue,startValue))
  stockReturn<-(endValue-startValue)/startValue
  return(stockReturn)
}
getLastQuarterReturn<-function(symbolName){
  quarterly.return.data<-quarterlyReturn(getTickerData(symbolName)[,1])
  print(quarterly.return.data[-1])
  #print(sprintf('return:::: %s',quarterly.return.data[,1]))
  return(tail(quarterly.return.data,1))
}

getTickerData<-function(symbolName){
  tickerData<-get(sprintf('%s%s',symbolName,'.NS'))
  na.omit(tickerData)
  return(tickerData[,4])
}

getMacdStats<-function(symbol){
  macdData<-getMacdDataByTicker(symbol)
  macdValidCount=length(which(!is.na(macdData[,'macd'])))
  macdValueGTZero=length(week.macd.data[week.macd.data$macd>0,'macd'])
  macdSuccessPerc=macdValueGTZero/macdValidCount
  print(sprintf("%s %s",macdValueGTZero,macdSuccessPerc))
  #print(length(which(is.na(macdData[,'macd']))))
  
  
  #print(length(which(is.na(macdData[,'macd']))))
}

week.macd.data<-getMacdDataByTicker('ASIANPAINT')
summary(week.macd.data)
length(week.macd.data[!is.na(week.macd.data$macd),'macd'])
length(week.macd.data[week.macd.data$macd<0,'macd'])
findPercMacdGoingBelow(week.macd.data)
as.numeric(week.macd.data[,'macd'])->week.macd.data[,'macd']
lapply(week.macd.data,is.numeric)

#function to calculate return counts
profitMonths <- function(symbolName) 
{
  tickerData<-getTickerData(symbolName)
  monthly.return.data<-monthlyReturn(tickerData[,1])
  greater.than.zero=which(monthly.return.data$monthly.returns>0)
  return(length(greater.than.zero))
}


profitQuarterly <- function(symbolName) 
{
  tickerData<-getTickerData(symbolName)
  quarterly.return.data<-quarterlyReturn(tickerData[,1])
  print(quarterly.return.data)
  greater.than.zero=which(quarterly.return.data$quarterly.returns>0)
  return(length(greater.than.zero))
}

populateReturnData<-function(){
  #init quarterly/monthly return data
  returns.data<-data.frame(matrix(ncol = 2, nrow = 0))
  colnames(returns.data) <- c("symbol", "success-quarters","success-macd-by-week")
  
  #load all returns data
  count=0
  for(symbol in tickers$Symbol){
    #for(symbol in c('ASIANPAINT')){
    count=count+1
    u<-profitQuarterly(symbol)
    returns.data[count, ] <- c(symbol, as.integer(u))
  }
  as.numeric(returns.data[,2])->returns.data[,2]
  return(returns.data)
}



return.data<-populateReturnData()






#get all ticker data using symbols
for(symbol in tickers$Symbol){
  getSymbols(sprintf('%s%s',symbol,'.NS'),from="2015-01-01")
}
