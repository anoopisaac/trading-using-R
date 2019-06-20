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

getClosingPrice<-function(symbol,date){
  return(as.numeric(tickerData[date,4]))
}

getProfitPerc<-function(symbol){
  
  tickerData<-getOrgTickerData(symbol)
  dailyMacdData  <- MACD( tickerData[,4], 12, 26, 9, maType="EMA",percent = F )
  dailyMacdData$histogram=as.numeric(dailyMacdData[,'macd'])-as.numeric(dailyMacdData[,'signal'])
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
  return(sum(as.numeric(purchase.positions[,'ProfitPerc'])))
  
}

getProfitPerc('ASIANPAINT')
