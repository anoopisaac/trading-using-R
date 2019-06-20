source("returns.R")
isFalling<-function(prevRowsToCheck,data,rowIndex,colName){
  list=as.vector(data[c(rowIndex:(rowIndex-prevRowsToCheck)),colName])
  print(list)
  return(is.unsorted(rev(list))==FALSE & tail(list,1)!=head(list,1))
}
isRising<-function(prevRowsToCheck,data,rowIndex,colName){
  list=as.vector(data[c(rowIndex:(rowIndex-prevRowsToCheck)),colName])
  print(list)
  return(is.unsorted(list)==FALSE & tail(list,1)!=head(list,1))
}

rowIndex=which(index(dailyMacdData) == "2014-02-07")
print(isFalling(2,dailyMacdData,rowIndex,'macd'))
print(isRising(2,dailyMacdData,rowIndex,'macd'))
dailyMacdData[c((rowIndex-1):(rowIndex-2)),'macd']
list=as.vector(dailyMacdData[c((rowIndex):(rowIndex-2)),'macd'])
print(is.unsorted(list)==FALSE & tail(1)!=head(1))
is.unsorted(c(1,2,3))==FALSE
is.unsorted(list)==TRUE & tail(1)!=head(1)
as.vector(list)

check=c(4,4,4,4)
is.unsorted(list)
is.unsorted(rev(check))
#
tickerData<-getOrgTickerData('ASIANPAINT')
#tickerData<-na.omit(tickerData)
weekMacdData<-getMacdDataByTicker(tickerData)
dailyMacdData  <- MACD( tickerData[,4], 12, 26, 9, maType="EMA",percent = F )
plot(dailyMacdData)
purchase.positions <- data.frame(Symbol=numeric(), type=character(), date=date(),profit=numeric()) 

count=0
for (row in 1:nrow(dailyMacdData)) {
  #check<-
  macd <- dailyMacdData[row, "macd"]
  signal  <- dailyMacdData[row, "signal"]
  date<-index(dailyMacdData[row])
  print(as.Date(date))
  print(dailyMacdData[date])
  #cat(macd,signal,index(dailyMacdData[row]),'\n')
  #return.stats[count, ] <- c(symbol, quarters$total,quarters$success,successMacdsPercByWeek,lastYearReturn,failureMonthsPercentile)
  
}

dailyMacdData<-dailyMacdData[index(dailyMacdData)>='2015-01-01']
