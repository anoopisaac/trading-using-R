source("returns.R")
isFalling<-function(prevRowsToCheck,data,rowIndex,colName){
  list=as.vector(data[c(rowIndex:(rowIndex-prevRowsToCheck)),colName])
  #print(list)
  status=(is.unsorted(rev(list))==FALSE & tail(list,1)!=head(list,1))
  return(if(is.na(status)) FALSE else status)
}
isRising<-function(prevRowsToCheck,data,rowIndex,colName){
  list=as.vector(data[c(rowIndex:(rowIndex-prevRowsToCheck)),colName])
  #print(list)
  status=(is.unsorted(list)==FALSE & tail(list,1)!=head(list,1))
  return(if(is.na(status)) FALSE else status)
}

hasCrossed<-function(macdData,currRow){
  prevMacd <- as.numeric(macdData[currRow-1, "macd"])
  prevSignal  <- as.numeric(macdData[currRow-1, "signal"])
  
  isPrevLess=prevMacd<prevSignal
  #cat('has crossed fn',prevMacd,prevSignal,currRow,isPrevLess,"\n")
  
  currMacd <- as.numeric(macdData[currRow, "macd"])
  currSignal  <- as.numeric(macdData[currRow, "signal"])
  isCurrGreater=currMacd>currSignal
  #cat(currMacd,currSignal,currRow,isCurrGreater,"\n")
  if(!is.na(isPrevLess) && isPrevLess && !is.na(isCurrGreater) && isCurrGreater){
    return(TRUE)
  }
  return (FALSE)
}
rowIndex=which(index(dailyMacdData) == "2014-05-14")
hasCrossed(rowIndex,dailyMacdData)

getClosingPrice<-function(symbol,date){
  return(as.numeric(tickerData[date,4]))
}


rowIndex=which(index(dailyMacdData) == "2014-01-17")
print(isFalling(2,dailyMacdData,rowIndex,'histogram'))
print(isRising(2,dailyMacdData,rowIndex,'histogram'))
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
tickerData<-getOrgTickerData(symbol)
#tickerData<-na.omit(tickerData)
weekMacdData<-getMacdDataByTicker(tickerData)
dailyMacdData  <- MACD( tickerData[,4], 12, 26, 9, maType="EMA",percent = F,histo )
plot(dailyMacdData)

startIndex=which(index(dailyMacdData) == "2014-02-18")
endIndex=which(index(dailyMacdData) == "2014-05-30")
symbol='ASIANPAINT'
purchase.positions <- data.frame(Symbol=numeric(), Type=character(), Date = numeric(),Profit=numeric(),ProfitPerc=numeric(),stringsAsFactors = FALSE) 
isPurchaseOn=FALSE
hasMacdCrossed=FALSE
#for (row in startIndex:endIndex) {
for (row in 1:nrow(dailyMacdData)) {
  #check<-
  macd <- as.numeric(dailyMacdData[row, "macd"])
  signal  <- as.numeric(dailyMacdData[row, "signal"])
  date<-index(dailyMacdData[row])
  isMacdLess=macd<signal
  isMacdGreater=macd>signal
  
  #hasMacdCrossed=hasCrossed(dailyMacdData,row)
  cat('crossed',hasMacdCrossed,as.character(date),"\n")
  # will only sell if it crosses the signal line
  if(isPurchaseOn ){
    #this needs to be done only once in purchase cycle
    if(hasMacdCrossed==FALSE){
      hasMacdCrossed=!is.na(isMacdGreater)&&isMacdGreater
    }
    
    buyingRow=purchase.positions[nrow(purchase.positions),]
    buyDate=(as.Date(as.numeric(buyingRow$Date)))
    sellFlag=isFalling(3,dailyMacdData,row,'histogram')
    cat('inside purchase',buyDate,sellFlag,hasMacdCrossed,"\n")
    if(hasMacdCrossed && !is.na(isMacdLess) && isMacdLess){
      cat('inside double cross*********\n')
    }
    sellFlag=sellFlag||(hasMacdCrossed && !is.na(isMacdLess) && isMacdLess)
    if(sellFlag){
      isPurchaseOn=FALSE
      buyingRow=purchase.positions[nrow(purchase.positions),]
      buyDate=(as.Date(as.numeric(buyingRow$Date)))
      buyingPrice=getClosingPrice(symbol,buyDate)
      #cat('buyingPrice',buyingPrice,'\n')
      sellingPrice=getClosingPrice(symbol,date)
      profit=(sellingPrice-buyingPrice)
      profitPerc=profit/buyingPrice
      purchase.positions[nrow(purchase.positions)+1, ] <- c(symbol, 'S',date,(sellingPrice-buyingPrice),profitPerc)
    }
    
  }
  if(!isPurchaseOn && !is.na(isMacdLess) && isMacdLess && isRising(2,dailyMacdData,row,'histogram')){
    #print(isRising(2,dailyMacdData,row,'macd'))
    cat("is less",macd,signal,as.character(date),"\n")
    isPurchaseOn=TRUE
    purchase.positions[nrow(purchase.positions)+1, ] <- c(symbol, 'B',date,0,0)
    hasMacdCrossed=FALSE
    
  }
  
}

purchase.positions$DateString=as.Date(as.numeric(purchase.positions[,'Date']))
purchase.positions$ClosingPrice=getClosingPrice('w',as.Date(as.numeric(purchase.positions[,'Date'])))
sum(as.numeric(purchase.positions[,'ProfitPerc']))

buyDate=(as.Date(as.numeric(purchase.positions[1,'Date'])))

getClosingPrice('w',buyDate)
getClosingPrice('w','2015-01-01')

as.numeric (dailyMacdData[100, "signal"])
dailyMacdData<-dailyMacdData[index(dailyMacdData)>='2015-01-01']
dailyMacdData$histogram=as.numeric(dailyMacdData[,'macd'])-as.numeric(dailyMacdData[,'signal'])
purchase.positions[nrow(purchase.positions)+1, ]<-c(symbol, 'Buy',date,0)


startIndex=which(index(dailyMacdData) == "2018-01-01")
endIndex=which(index(dailyMacdData) == "2019-05-30")

check.data<-dailyMacdData[startIndex:endIndex,]
dailyMacdData[as.Date("2018-01-01"):as.Date("2019-05-31"),]

