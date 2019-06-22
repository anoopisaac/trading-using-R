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
  return(as.numeric(tickerData[date,4]))
}

getProfitPerc<-function(symbol,macdData){
  
  
  macdData$histogram=as.numeric(macdData[,'macd'])-as.numeric(macdData[,'signal'])
  purchase.positions <- data.frame(Symbol=numeric(), Type=character(), Date = numeric(),Profit=numeric(),ProfitPerc=numeric(),stringsAsFactors = FALSE) 
  isPurchaseOn=FALSE
  hasMacdCrossed=FALSE
  #for (row in startIndex:endIndex) {
  for (row in 1:nrow(macdData)) {
    #check<-
    macd <- as.numeric(macdData[row, "macd"])
    signal  <- as.numeric(macdData[row, "signal"])
    date<-index(macdData[row])
    isMacdLess=macd<signal
    isMacdGreater=macd>signal
    
    #hasMacdCrossed=hasCrossed(dailyMacdData,row)
    #cat('ispurchaseon...',isPurchaseOn,'crossed',hasMacdCrossed,as.character(date),"\n")
    # will only sell if it crosses the signal line
    if(isPurchaseOn ){
      #this needs to be done only once in purchase cycle
      if(hasMacdCrossed==FALSE){
        hasMacdCrossed=!is.na(isMacdGreater)&&isMacdGreater
      }
      
      buyingRow=purchase.positions[nrow(purchase.positions),]
      buyDate=(as.Date(as.numeric(buyingRow$Date)))
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
  purchase.positions$ClosingPrice=getClosingPrice('w',as.Date(as.numeric(purchase.positions[,'Date'])))
  #purchase.positions<-purchase.positions[as.Date(as.numeric(purchase.positions$Date))>as.Date(startDate),]
  return(list(profitPerc=sum(as.numeric(purchase.positions[,'ProfitPerc'])),purchaseData=purchase.positions,successTradesPerc=successTradesPerc,tradeCounts=tradeCounts))
  
}

swing.trading.data <- data.frame(Symbol=numeric(), ProfitPerc=numeric(),successMacd=numeric(),successTradesPerc=numeric(),tradeCounts=numeric(),stringsAsFactors = FALSE) 

backTest<-function(symbolList,startDate){
  backTestData <- data.frame(Symbol=numeric(), ProfitPerc=numeric(),successMacd=numeric(),successTradesPerc=numeric(),tradeCounts=numeric(),stringsAsFactors = FALSE) 
  #get all ticker data using symbols
  #for(symbol in tickers$Symbol){
  for(symbol in symbolList){
    #symbol='BAJFINANCE'
    tickerData<-getOrgTickerData(symbol)
    #daily daa
    #dailyMacdData  <- MACD( tickerData[,4], 12, 26, 9, maType="EMA",percent = F )
    #dailyMacdData<-dailyMacdData[index(dailyMacdData)>as.Date("2018-05-01"),]
    #weeklydata
    weeklyMacData=getMacdDataByTicker(tickerData)
    #weeklyMacData<-weeklyMacData[index(weeklyMacData)>as.Date("2015-05-01")& index(weeklyMacData)<as.Date("2016-05-01"),]
    weeklyMacData<-weeklyMacData[index(weeklyMacData)>as.Date(startDate) & index(weeklyMacData)<as.Date(startDate) + years(1),]
    result=getProfitPerc(symbol,weeklyMacData)
    successMacd=return.stats[return.stats$Symbol==symbol,]$SuccessMacd
    backTestData[nrow(backTestData)+1, ] <- c(symbol, result$profitPerc,successMacd,result$successTradesPerc,result$tradeCounts)
    #getting macd success percentage from return.stats
    
    purchase.postions<-result$purchaseData
    #print(result$purchaseData)
    cat('symobl',symbol,'perc',result$profitPerc,'\n','success trades',result$successTradesPerc,'trade counts',result$tradeCounts,'sumacd',successMacd)
  }
  return(backTestData)
}
symbolList=c('BAJFINANCE','HDFCBANK','HAVELLS','BAJAJFINSV','BIOCON','BRITANNIA','DABUR')
backTestData <- data.frame(Symbol=numeric(), ProfitPerc=numeric(),successMacd=numeric(),successTradesPerc=numeric(),tradeCounts=numeric(),year=character(),stringsAsFactors = FALSE) 
dates=c("2018-05-01","2017-05-01","2016-05-01","2015-05-01")
for(year in dates){
  tempData<-backTest(symbolList,year)
  for(i in 1:nrow(tempData)){
    rowIndex=nrow(backTestData)+1
    backTestData[rowIndex, ] <- tempData[i,]
    backTestData[rowIndex,'year']=year
    swing.trading.data[nrow(swing.trading.data)+1, ] <- c(symbol, result$profitPerc,successMacd,result$successTradesPerc,result$tradeCounts)
  }
}
sum(as.numeric(subset(backTestData,year=='2018-05-01')$tradeCounts))
sum(as.numeric(subset(backTestData,year=='2018-05-01')$ProfitPerc))





prev.swing.trad<-swing.trading.data

swing.trading.data.macd<-subset(swing.trading.data,(successMacd>.75&ProfitPerc>.15) )
nrow(subset(swing.trading.data,ProfitPerc<=0))

getProfitPerc('ASIANPAINT',"2018-01-01")

dailyMacdData[index(dailyMacdData)>as.Date("2018-01-01"),]

purchase.positions[as.Date(as.numeric(purchase.positions$Date))>as.Date("2018-01-01"),]
