source("returns.R")
isFalling<-function(prevRowsToCheck,data,rowIndex,colName){
  if(rowIndex-prevRowsToCheck<=0){
    return(FALSE)
  }
  list=as.vector(data[c(rowIndex:(rowIndex-prevRowsToCheck)),colName])
  #print(list)
  status=(is.unsorted(rev(list))==FALSE & tail(list,1)!=head(list,1))
  return(if(is.na(status)) FALSE else status)
}
isRising<-function(prevRowsToCheck,data,rowIndex,colName){
  if(rowIndex-prevRowsToCheck<=0){
    return(FALSE)
  }
  list=as.vector(data[c(rowIndex:(rowIndex-prevRowsToCheck)),colName])
  #print(list)
  status=(is.unsorted(list)==FALSE & tail(list,1)!=head(list,1))
  return(if(is.na(status)) FALSE else status)
}

getClosingPrice<-function(symbol,date){
  print(symbol)
  tickerData<-getOrgTickerData(symbol)
  return(as.numeric(tickerData[date,4]))
}

#get profit in perc by creating seris of buy/sell positions
#macdData - is fetched from global data populated elsewhere
getProfitPerc<-function(symbol,macdData){
  #manually find histogram by subtracting macd value from signal line
  macdData$histogram=as.numeric(macdData[,'macd'])-as.numeric(macdData[,'signal'])
  #creating dataframe to store buy sell positions
  purchase.positions <- data.frame(Symbol=numeric(), Type=character(), Date = numeric(),Profit=numeric(),ProfitPerc=numeric(),stringsAsFactors = FALSE) 
  isPurchaseOn=FALSE
  hasMacdCrossed=FALSE
  #for (row in startIndex:endIndex) {
  for (row in 1:nrow(macdData)) {
    macd <- as.numeric(macdData[row, "macd"])
    signal  <- as.numeric(macdData[row, "signal"])
    date<-index(macdData[row])
    isMacdLess=macd<signal
    isMacdGreater=macd>signal
    
    if(isPurchaseOn ){
      #this needs to be done only once in purchase cycle
      if(hasMacdCrossed==FALSE){
        hasMacdCrossed=!is.na(isMacdGreater)&&isMacdGreater
      }
      #buying row will be the last one.
      buyingRow=purchase.positions[nrow(purchase.positions),]
      buyDate=(as.Date(as.numeric(buyingRow$Date)))
      #for now iam only checking this condition to decide to sell. should be falling for consequtive 3 weeks incluing current week
      sellFlag=isFalling(2,macdData,row,'histogram')
      #cat('inside purchase',buyDate,sellFlag,hasMacdCrossed,"\n")
      if(hasMacdCrossed && !is.na(isMacdLess) && isMacdLess){
        #cat('inside double cross*********\n')
      }
      #sellFlag=sellFlag||(hasMacdCrossed && !is.na(isMacdLess) && isMacdLess)
      if(sellFlag){
        isPurchaseOn=FALSE
        buyingRow=purchase.positions[nrow(purchase.positions),]
        buyDate=(as.Date(as.numeric(buyingRow$Date)))
        #is fetched from global data, if macd is weekly data, this will be the price when then week ends
        buyingPrice=getClosingPrice(symbol,buyDate)
        cat('sell...........',buyingPrice,'\n')
        sellingPrice=getClosingPrice(symbol,date)
        profit=(sellingPrice-buyingPrice)
        profitPerc=profit/buyingPrice
        purchase.positions[nrow(purchase.positions)+1, ] <- c(symbol, 'S',date,(sellingPrice-buyingPrice),profitPerc)
      }
      
    }
    #if(!isPurchaseOn && !is.na(isMacdLess) && isMacdLess && isRising(2,macdData,row,'macd')){
    if(!isPurchaseOn && isRising(2,macdData,row,'histogram')){
      #print(isRising(2,dailyMacdData,row,'macd'))
      cat("buy.................",macd,signal,as.character(date),"\n")
      isPurchaseOn=TRUE
      purchase.positions[nrow(purchase.positions)+1, ] <- c(symbol, 'B',date,0,0)
      hasMacdCrossed=FALSE
      
    }
    
  }
  
  purchase.positions$DateString=as.Date(as.numeric(purchase.positions[,'Date']))
  tradeCounts=nrow(subset(purchase.positions,Type=='S'))
  successTrades=nrow(subset(purchase.positions,Profit>0&Type=='S'))
  successTradesPerc=successTrades/tradeCounts
  purchase.positions$ClosingPrice=getClosingPrice(symbol,as.Date(as.numeric(purchase.positions[,'Date'])))
  if(nrow(purchase.positions)==0){
    isBuyOn=FALSE
  } else{
    isBuyOn=purchase.positions[nrow(purchase.positions),]$Type=='B'
  }
  
  #purchase.positions<-purchase.positions[as.Date(as.numeric(purchase.positions$Date))>as.Date(startDate),]
  return(list(profitPerc=sum(as.numeric(purchase.positions[,'ProfitPerc'])),purchaseData=purchase.positions,successTradesPerc=successTradesPerc,tradeCounts=tradeCounts,isBuyOn=isBuyOn))
  
}

#swing.trading.data <- data.frame(Symbol=numeric(), ProfitPerc=numeric(),successMacd=numeric(),successTradesPerc=numeric(),tradeCounts=numeric(),stringsAsFactors = FALSE) 

backTest<-function(symbolList,startDate,endDate,type){
  #backtest data for passed symbol for the spceified timefram
  dateRangeBacktestData <- data.frame(Symbol=numeric(), ProfitPerc=numeric(),successMacd=numeric(),successTradesPerc=numeric(),tradeCounts=numeric(),isBuyOn=numeric(),stringsAsFactors = FALSE) 
  
  #backtesting for all the filtered symbols- the one with high macd success ratio for the last 5 years
  for(symbol in symbolList){
    # symbol='BAJFINANCE'
    # startDate="2018-05-01"
    # endDate="2019-05-01"
    #dateRangeBacktestData <- data.frame(Symbol=numeric(), ProfitPerc=numeric(),successMacd=numeric(),successTradesPerc=numeric(),tradeCounts=numeric(),isBuyOn=numeric(),stringsAsFactors = FALSE) 
    
    #get ticcker data pulled and kept in global dataframe
    tickerData<-getOrgTickerData(symbol)
    #weekly macd data
    #macdData=getMacdDataByTicker(tickerData)
    #dailydata
    if(type=='W'){
      macdData=getMacdDataByTicker(tickerData)
    } else{
      macdData=getMacdDailyDataByTicker(tickerData)
    }
    
    #pulling the data for desired time frame
    macdData<-macdData[index(macdData)>startDate & index(macdData)<=endDate,]
    cat('weeklydata............##########',symbol,nrow(macdData),startDate,'\n')
    result=getProfitPerc(symbol,macdData)
    #this return.stats holds the percentage of the time macd was greater that 0. which mean 26 weeks ema > 12 weeks ema
    successMacd=return.stats[return.stats$Symbol==symbol,]$SuccessMacd
    print(result)
    dateRangeBacktestData[nrow(dateRangeBacktestData)+1, ] <- c(symbol, result$profitPerc,successMacd,result$successTradesPerc,result$tradeCounts,result$isBuyOn)
    #getting macd success percentage from return.stats
    
    purchase.postions<-result$purchaseData
    #print(result$purchaseData)
    #cat('symobl',symbol,'perc',result$profitPerc,'success trades',result$successTradesPerc,'trade counts',result$tradeCounts,'sumacd',successMacdl,'\n')
  }
  return(dateRangeBacktestData)
}
#symbolList=c('BAJFINANCE','HDFCBANK','HAVELLS','BAJAJFINSV','BIOCON','BRITANNIA','DABUR')
weeklySymbolList=c('BAJFINANCE','BAJAJFINSV','HDFCBANK','HAVELLS','BERGEPAINT','PIDILITIND','ASIANPAINT','MARICO','SRF','KOTAKBANK','RELIANCE')
dailySymbolList=c('BAJFINANCE','BAJAJFINSV','HDFCBANK','HAVELLS','BERGEPAINT','PIDILITIND','ASIANPAINT','SRF','KOTAKBANK','RELIANCE')

#initializing dataframe
backTestData <- data.frame(Symbol=numeric(), ProfitPerc=numeric(),successMacd=numeric(),successTradesPerc=numeric(),tradeCounts=numeric(),isBuyOn=numeric(),year=character(),stringsAsFactors = FALSE) 
dates=c("2018-05-01","2017-05-01","2016-05-01","2015-05-01")


for(year in dates){
  startDate=as.Date(year)
  endDate=as.Date(startDate) + years(1);
  tempData<-backTest(dailySymbolList,startDate,endDate,'D')
  # tempData<-backTest(weeklySymbolList,startDate,endDate,'W')
  # tempData<-backTest(symbolList,"2019-01-01","2019-06-28")
  print(tempData)
  for(i in 1:nrow(tempData)){
    rowIndex=nrow(backTestData)+1
    backTestData[rowIndex, ] <- tempData[i,]
    backTestData[rowIndex,'year']=year
    #swing.trading.data[nrow(swing.trading.data)+1, ] <- c(symbol, result$profitPerc,successMacd,result$successTradesPerc,result$tradeCounts,,result$isBuyOn)
  }
}

symbolList=c('BAJFINANCE','BAJAJFINSV','HDFCBANK','HAVELLS','BERGEPAINT','PIDILITIND','ASIANPAINT','MARICO','SRF','KOTAKBANK','RELIANCE')
#for checking whether buy is on
isBuyOnData<-backTest(symbolList,"2019-01-01","2019-06-28")


sum(as.numeric(subset(backTestData,year=='2018-05-01')$tradeCounts))
sum(as.numeric(subset(backTestData,year=='2018-05-01')$ProfitPerc))
#sum(as.numeric(subset(backTestData,Symbol=='BAJAJFINSV')$ProfitPerc))


subset(backTestData,year=='2018-05-01') %>% group_by(Symbol) %>% summarise(B = sum(as.numeric(ProfitPerc)))%>% arrange(desc(B))



prev.swing.trad<-swing.trading.data

swing.trading.data.macd<-subset(swing.trading.data,(successMacd>.75&ProfitPerc>.15) )
nrow(subset(swing.trading.data,ProfitPerc<=0))

getProfitPerc('ASIANPAINT',"2018-01-01")

dailyMacdData[index(dailyMacdData)>as.Date("2018-01-01"),]

purchase.positions[as.Date(as.numeric(purchase.positions$Date))>as.Date("2018-01-01"),]
