
#you could blindly run all the below code by doing select all

library(quantmod)
library(lubridate)
library(dplyr) 

populateReturnData<-function(tickers){
  
  return.stats <- data.frame(Symbol=numeric(), total=numeric(), SuccessQtrs=numeric(), SuccessMacd=numeric(), SuccessMacdLastYear=numeric(),LastYearReturn=numeric(), FailureMonthsPercentile=numeric(),stringsAsFactors=FALSE) 
  count=0
  for(symbol in tickers$Symbol){
    # if exception happens this will be turned to true
    print(symbol)
    #for(symbol in c('ASIANPAINT')){
    count=count+1
    
    result = tryCatch({
      orgTickerData<-getOrgTickerData(symbol)
      quarters<-profitQuarterly(orgTickerData[,4])
      cat(orgTickerData[1,],'\n')
      macdData<-getMacdDataByTicker(orgTickerData,12,26,9)
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
    }, finally = {
    })
    if(is.null(result)){
      cat("failed....................",symbol,'\n')
      next
    }
    #this is done this way because EMA depends on previous EMAs, so it needs to have data since 2014 for 2015 onwards data to work
    macdData<-macdData[index(macdData)>='2015-01-01']
    #list of macd where the value is above zero, which i assume, would mean its 12 weeks average is above 26 week average
    successMacdsPercByWeek<-getMacdStats(macdData)
    successMacdPercLastYear<-getMacdStatsLasYear(macdData)
    lastYearReturn<-getLastYearReturn(orgTickerData[,4])
    #failureMonthsPercentile<-getFailureMonthsPercentile(macdData)
    #cat(symbol,is.na(lastYearReturn))
    return.stats[count, ] <- c(symbol, quarters$total,quarters$success,successMacdsPercByWeek,successMacdPercLastYear,lastYearReturn,-11)
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
#print(getLastYearReturn('BANDHANBNK'))
#print(getLastYearReturn('ASIANPAINT'))
getLastQuarterReturn<-function(symbolName){
  quarterly.return.data<-quarterlyReturn(getOrgTickerData(symbolName)[,1])
  return(tail(quarterly.return.data,1))
}


getOrgTickerData<-function(symbolName){
  tickerData<-get(symbolName)
  tickerData<-na.omit(tickerData)
  #tickerData<-tickerData[index(tickerData)>='2015-01-01']
  return(tickerData)
}

#get daily macd data
getMacdDailyDataByTicker<-function(orgTickerData,fast,slow,signal){
  dailyMacd  <- MACD( orgTickerData[,4], fast, slow, signal, maType="EMA",percent = F )
  return(dailyMacd)
}

#get weekly macd datas
getMacdDataByTicker<-function(orgTickerData,fast,slow,signal){
  weekData <- to.weekly(orgTickerData)
  weekMacd  <- MACD( weekData[,4], fast, slow, signal, maType="EMA",percent = F )
  return(weekMacd)
}


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

getMacdStats<-function(macdData){
  #macdData<-getMacdDataByTicker(orgTickerData)
  macdValidCount=length(which(!is.na(macdData[,'macd'])))
  macdValueGTZero=length(macdData[macdData$macd>0,'macd'])
  macdSuccessPerc=macdValueGTZero/macdValidCount
  #print(sprintf("%s %s",macdValueGTZero,macdSuccessPerc))
  return(macdSuccessPerc)
}

getMacdStatsLasYear<-function(macdData){
  endDate=Sys.Date()
  startDate=Sys.Date() - years(1)
  macdData<-macdData[index(macdData)>startDate & index(macdData)<endDate,]
  return(getMacdStats(macdData))
}


profitQuarterly <- function(closeTickerData) 
{
  #tickerData<-getTickerData(symbolName)
  quarterly.return.data<-quarterlyReturn(closeTickerData[,1])
  #print(quarterly.return.data)
  greater.than.zero=which(quarterly.return.data$quarterly.returns>0)
  return (list(success=length(greater.than.zero),total=length(quarterly.return.data)))
}

#Step.1.2 initiating dataframes
#init quarterly/monthly return data
return.stats<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(return.stats) <- c("symbol", "success-quarters","success-macd-by-week")


#getting all tickers
combinedTickers <- read.csv(file=file.path("both", "200nb"), header=T)

#Step.4 get return data
return.stats<-populateReturnData(combinedTickers)

