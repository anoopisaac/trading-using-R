olderLogicToFindPurchasePoint<-function(){
  
  
  lastNDaysDataAsList<-split(lastNDaysData, seq(nrow(lastNDaysData)))
  inZeroAndOnes<-Map({function (ticker) ifelse(ticker[[4]]-ticker[[1]]>0,1,0)}, lastNDaysDataAsList)
  inZeroAndOnes<-unlist(inZeroAndOnes);
  sumOfGreenCandles<-Reduce(function (x, y) x+y, inZeroAndOnes)
  isRecentPositive<-Reduce(function (x, y) x&y, inZeroAndOnes[(length(inZeroAndOnes)-nRecentPostivies):length(inZeroAndOnes)])
  isAnyBelowEma<-Reduce(function (sofar,data) sofar|data[[4]]<emaValue, lastNDaysDataAsList,FALSE)
  print(paste(emaValue,closingPrice))
  isLastAboveEma<-(closingPrice>as.numeric(emaValue))
  print(paste('ema:',emaValue,'|cl price',closingPrice, '|recent gre candles:',sum,'|threshold:',successCount,'|is recent candles green:', isRecentPositive,'|any below ema:',
              isAnyBelowEma,'|islast above ema:',isLastAboveEma,'|date:',index(rowData)))
}