#step.1
library(quantmod)
library(lubridate)

#step.1.01 - execute all methods
populateReturnData<-function(){
  
  return.stats <- data.frame(Symbol=numeric(), total=numeric(), SuccessQtrs=numeric(), SuccessMacd=numeric(),LastYearReturn=numeric(), FailureMonthsPercentile=numeric(),stringsAsFactors=FALSE) 
  count=0
  for(symbol in tickers$Symbol){
    print(symbol)
    #for(symbol in c('ASIANPAINT')){
    count=count+1
    orgTickerData<-getOrgTickerData(symbol)
    quarters<-profitQuarterly(orgTickerData[,4])
    
    macdData<-getMacdDataByTicker(orgTickerData)
    #this is done this way because EMA depends on previous EMAs, so it needs to have data since 2014 for 2015 onwards data to work
    macdData<-macdData[index(macdData)>='2015-01-01']
    #list of macd where the value is above zero, which i assume, would mean its 12 weeks average is above 26 week average
    successMacdsPercByWeek<-getMacdStats(macdData)
    lastYearReturn<-getLastYearReturn(orgTickerData[,4])
    failureMonthsPercentile<-getFailureMonthsPercentile(macdData)
    #cat(symbol,is.na(lastYearReturn))
    return.stats[count, ] <- c(symbol, quarters$total,quarters$success,successMacdsPercByWeek,lastYearReturn,failureMonthsPercentile)
    #return.stats.from.2015=return.stats[index(return.stats)>'2015-01-01']
    #return.stats[count, ] <- c(1,1,2)
    #print(count)
    #print(c(symbol, successQurters,successMacdsPercByWeek))
  }
  #converting to numeric value
  as.numeric(return.stats[,2])->return.stats[,2]
  as.numeric(return.stats[,3])->return.stats[,3]
  as.numeric(return.stats[,4])->return.stats[,4]
  as.numeric(return.stats[,5])->return.stats[,5]
  return(return.stats)
}

#getting returns only for the last year
getLastYearReturn<-function(closeTickerData){
  last_day_prev_year <- floor_date(Sys.Date(), "year") - days(1)
  first_day_prev_year <- floor_date(last_day_prev_year, "year")
  #for some tickerdata for last year could be null, if they started recently
  if(length(closeTickerData[first_day_prev_year,1])==0 | length(closeTickerData[last_day_prev_year,1])==0){
    return(-1111)
  }
  startValue=as.numeric(closeTickerData[first_day_prev_year,1])
  endValue=as.numeric(closeTickerData[last_day_prev_year,1])
  #cat(startValue,tickerData[first_day_prev_year,1], endValue,length(endValue)))
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
  tickerData<-na.omit(tickerData)
  return(tickerData[,4])
}

getOrgTickerData<-function(symbolName,date){
  tickerData<-get(sprintf('%s%s',symbolName,'.NS'))
  tickerData<-na.omit(tickerData)
  #tickerData<-tickerData[index(tickerData)>='2015-01-01']
  na.omit(tickerData)
  return(tickerData)
}

#finding macd line that goes below zero when plotted in weeks
getMacdDailyDataByTicker<-function(orgTickerData){
  dailyMacd  <- MACD( orgTickerData[,4], 12, 26, 9, maType="EMA",percent = F )
  return(dailyMacd)
}

#finding macd line that goes below zero when plotted in weeks
getMacdDataByTicker<-function(orgTickerData){
  weekData <- to.weekly(orgTickerData)
  #print(sprintf("%s %s",symbolName,length(weekData)))
  weekMacd  <- MACD( weekData[,4], 12, 26, 9, maType="EMA",percent = F )
  #print(EMA(weekData[,1],12))
  #print(EMA(weekData[,1],26))
  return(weekMacd)
}


for(symbol in tickers$Symbol){
  getMacdDataByTicker(symbol)
}
library(dplyr) 

asianpaint.macd.data %>% filter(macd>0)
starwars %>% filter(mass > mean(mass, na.rm = TRUE))
x <- 1:100
filter(x, rep(1, 3))

#this values should small which would denote that success macds are evenly distributed and there is concentration
getFailureMonthsPercentile<-function(macdData){
  cat('reached',length(macdData[!is.na(macdData$macd),'macd']))
  success.data<-macdData[macdData$macd>0,'macd']
  cat('reached.',length(success.data))
  if(length(success.data)==0){
    return(0)
  }
  # this is done so that I can get the count after grouping
  success.data$count=c(1:1)
  print('reached..')
  groupedByMonth <- apply.monthly(xts(success.data), colSums)
  print('reached....')
  #way to find percentile given value
  percentile<-ecdf(as.vector(groupedByMonth$count))
  print('reached.....')
  # as there are 4 to 5 weeks in a month and assuming median of 4.2 , 3.8 should be somthing below median. this value 
  #should be as low as possible which would say there not lot many months which success macds below median value
  return(percentile(3.8))
}
print(getFailureMonthsPercentile(adanipower.macd.data))
length(adanipower.macd.data[!is.na(adanipower.macd.data$macd),'macd'])
asianpaint.macd.data<-getMacdDataByTicker('ASIANPAINT')
adanipower.macd.data<-getMacdDataByTicker('ADANIPOWER')
length(adanipower.macd.data[!is.na(adanipower.macd.data$macd),'macd'])
length(adanipower.macd.data[adanipower.macd.data$macd>0,'macd'])
plot(adanipower.macd.data)

length(success.data)
length(asianpaint.macd.data$macd)
lmonthly.success
plot(asianpaint.macd.data$macd)
plot(asianpaint.week.data$`getOrgTickerData("ASIANPAINT").Close`)
asianpaint.week.data <- to.weekly(getOrgTickerData('ASIANPAINT'))
asia.ema12<-(EMA(asianpaint.week.data$`getOrgTickerData("ASIANPAINT").Close`,12))
asia.ema26<-(EMA(asianpaint.week.data$`getOrgTickerData("ASIANPAINT").Close`,26))
filter(week.macd.data,signal=='ewrer')
(asianpaint.macd.data%>%filter(any(signal>0)))
length(asianpaint.macd.data$macd[asianpaint.macd.data$macd>0])
asianpaint.macd.data$macd %>% filter('macd' > 0)

getMacdStats<-function(macdData){
  #macdData<-getMacdDataByTicker(orgTickerData)
  macdValidCount=length(which(!is.na(macdData[,'macd'])))
  macdValueGTZero=length(macdData[macdData$macd>0,'macd'])
  macdSuccessPerc=macdValueGTZero/macdValidCount
  #print(sprintf("%s %s",macdValueGTZero,macdSuccessPerc))
  return(macdSuccessPerc)
}


summary(week.macd.data)
length(week.macd.data[!is.na(week.macd.data$macd),'macd'])
length(week.macd.data[week.macd.data$macd<0,'macd'])
findPercMacdGoingBelow(week.macd.data)
as.numeric(week.macd.data[,'macd'])->week.macd.data[,'macd']
lapply(week.macd.data,is.numeric)

#dpont refer this, this is wrong code
profitMonths <- function(symbolName) 
{
  tickerData<-getTickerData(symbolName)
  monthly.return.data<-monthlyReturn(tickerData[,1])
  greater.than.zero=which(monthly.return.data$monthly.returns>0)
  return(length(greater.than.zero))
}

profitQuarterly <- function(closeTickerData) 
{
  #tickerData<-getTickerData(symbolName)
  quarterly.return.data<-quarterlyReturn(closeTickerData[,1])
  #print(quarterly.return.data)
  greater.than.zero=which(quarterly.return.data$quarterly.returns>0)
  return (list(success=length(greater.than.zero),total=length(quarterly.return.data)))
}


#Step1.1
setwd('/Users/anoopisaac/projects/trading-using-R')
getwd()


#Step.1.2 initiating dataframes
#init quarterly/monthly return data
return.stats<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(return.stats) <- c("symbol", "success-quarters","success-macd-by-week")

#step.2
#init ticker symbols
tickers <- read.csv(file=file.path("nifty", "200"), header=T)



#Step.3
#get all ticker data using symbols
for(symbol in tickers$Symbol){
  getSymbols(sprintf('%s%s',symbol,'.NS'),from="2014-01-01")
}


#Step.4 get return data
return.stats<-populateReturnData()

getSumOfAllEarnings<-function(){
  #attach(return.stats)
  sumOfAllEarnings<-sum(head(return.stats[order(-SuccessMacd),],10)[,4])
  #return(sumOfAllEarnings)
}



orgTickerData<-getOrgTickerData('ASIANPAINT')
quarters<-profitQuarterly(orgTickerData[,4])

macdData<-getMacdDataByTicker(orgTickerData)
plot(macdData)
#this is done this way because EMA depends on previous EMAs, so it needs to have data since 2014 for 2015 onwards data to work
macdData<-macdData[index(macdData)>='2015-01-01']
plot(macdData)


getSymbols(sprintf('%s%s','ASIANPAINT','.NS'),from="2014-01-01")

MUTHOOTFIN.NS['2018-01-01']
MUTHOOTFIN.NS['2018-12-31']
