library(quantmod)
library(lubridate)

setwd('C:/Users/anoop/dream/running/r-stock-trading')
getwd()


#init quarterly/monthly return data
return.stats<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(return.stats) <- c("symbol", "success-quarters","success-macd-by-week")

#init ticker symbols
tickers <- read.csv(file=file.path("nifty", "200"), header=T)

getLastYearReturn<-function(symbolName){
  last_day_prev_year <- floor_date(Sys.Date(), "year") - days(1)
  first_day_prev_year <- floor_date(last_day_prev_year, "year")
  #print(sprintf('%s dates %s',last_day_prev_year,first_day_prev_year))
  tickerData<-getTickerData(symbolName)
  #if any of the start or end tick data is not presetn return -1
  if(length(tickerData[first_day_prev_year,1])==0 | length(tickerData[last_day_prev_year,1])==0){
    return(-1111)
  }
  startValue=as.numeric(tickerData[first_day_prev_year,1])
  endValue=as.numeric(tickerData[last_day_prev_year,1])
  cat(startValue,tickerData[first_day_prev_year,1], endValue,length(endValue))
  #print(sprintf('%s  %s',endValue,startValue))
  stockReturn<-(endValue-startValue)/startValue
  return(stockReturn)
}
print(getLastYearReturn('BANDHANBNK'))
print(getLastYearReturn('ASIANPAINT'))
getLastQuarterReturn<-function(symbolName){
  quarterly.return.data<-quarterlyReturn(getTickerData(symbolName)[,1])
  #print(quarterly.return.data[-1])
  #print(sprintf('return:::: %s',quarterly.return.data[,1]))
  return(tail(quarterly.return.data,1))
}

getTickerData<-function(symbolName){
  tickerData<-get(sprintf('%s%s',symbolName,'.NS'))
  na.omit(tickerData)
  return(tickerData[,4])
}

#finding macd line that goes below zero when plotted in weeks
getMacdDataByTicker<-function(symbolName){
  weekData <- to.weekly(getTickerData(symbolName))
  #print(sprintf("%s %s",symbolName,length(weekData)))
  weekMacd  <- MACD( weekData[,4], 12, 26, 9, maType="EMA",percent = F )
  return(weekMacd)
}
for(symbol in tickers$Symbol){
  getMacdDataByTicker(symbol)
}


getMacdStats<-function(symbol){
  macdData<-getMacdDataByTicker(symbol)
  macdValidCount=length(which(!is.na(macdData[,'macd'])))
  macdValueGTZero=length(macdData[macdData$macd>0,'macd'])
  macdSuccessPerc=macdValueGTZero/macdValidCount
  #print(sprintf("%s %s",macdValueGTZero,macdSuccessPerc))
  return(macdSuccessPerc)
}

week.macd.data.asian<-getMacdDataByTicker('ASIANPAINT')
week.macd.data.abb<-getMacdDataByTicker('ABB')
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
  #print(quarterly.return.data)
  greater.than.zero=which(quarterly.return.data$quarterly.returns>0)
  return(length(greater.than.zero))
}

populateReturnData<-function(){
  
  return.stats <- data.frame(Symbol=numeric(), SuccessQtrs=numeric(), SuccessMacd=numeric(),LastYearReturn=numeric(), stringsAsFactors=FALSE) 
  count=0
  for(symbol in tickers$Symbol){
    #for(symbol in c('ASIANPAINT')){
    count=count+1
    successQurters<-profitQuarterly(symbol)
    #list of macd where the value is above zero, which i assume, would mean its 12 weeks average is above 26 week average
    successMacdsPercByWeek<-getMacdStats(symbol)
    lastYearReturn<-getLastYearReturn(symbol)
    cat(symbol,is.na(lastYearReturn))
    return.stats[count, ] <- c(symbol, successQurters,successMacdsPercByWeek,lastYearReturn)
    #return.stats[count, ] <- c(1,1,2)
    #print(count)
    #print(c(symbol, successQurters,successMacdsPercByWeek))
  }
  as.numeric(return.stats[,2])->return.stats[,2]
  as.numeric(return.stats[,3])->return.stats[,3]
  as.numeric(return.stats[,4])->return.stats[,4]
  return(return.stats)
}

getSumOfAllEarnings<-function(){
  attach(return.stats)
  sumOfAllEarnings<-sum(head(return.stats[order(-SuccessMacd),],10)[,4])
  return(sumOfAllEarnings)
}

return.stats<-populateReturnData()

#get all ticker data using symbols
for(symbol in tickers$Symbol){
  getSymbols(sprintf('%s%s',symbol,'.NS'),from="2015-01-01")
}
