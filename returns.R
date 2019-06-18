library(quantmod)
setwd('C:/Users/anoop/dream/running/r-stock-trading')
getwd()


#init ticker symbols
tickers <- read.csv(file=file.path("nifty", "200"), header=T)

#finding macd line that goes below zero when plotted in weeks
getMacdDataByTicker<-function(symbolName){
  #get data from global dataframe using dynamic access
  tickerData=get(sprintf('%s%s',symbolName,'.NS'))
  #omit null
  tickerData<-na.omit(tickerData)
  
  closeData<-tickerData[,4]
  weekData <- to.weekly(closeData)
  weekMacd  <- MACD( weekData[,4], 12, 26, 9, maType="EMA",percent = F )
  return(weekMacd)
  #print(macd)
}

findPercMacdGoingBelow<-function(){
  
  #print(tickerData)
  weekMacd<-getMacdDataByTicker(tickerData)
  
  length(which(!is.na(weekMacd[,'macd'])))
  length(which(is.na(weekMacd[,'macd'])))
  check<-which(is.numeric(weekMacd[,'macd']))
}

macd.data<-getMacdDataByTicker('ASIANPAINT')


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

populateReturnData<-function(tickData){
  #init quarterly/monthly return data
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
