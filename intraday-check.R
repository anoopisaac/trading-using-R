isFalling<-function(prevRowsToCheck,data,rowIndex,colName){
  # check whether this was first row
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

#going through macd data and decidign when to sell
checkTrading<-function(macd_tech_data){
  intra.purchase.positions <- data.frame(Type=character(), SD = numeric(),BD=numeric(),Profit=numeric(),ProfitPerc=numeric(),BP=numeric(),SP=numeric(), stringsAsFactors = FALSE) 
  isPurchaseOn=FALSE
  buyingPrice=-1
  for(i in 1:nrow(macd_tech_data)){
    macd <- as.numeric(macd_tech_data[row, "macd"])
    signal  <- as.numeric(macd_tech_data[row, "signal"])
    currRow=macd_tech_data[i,]
    currPrice=as.numeric(as.character(currRow$close))
    currDate=as.character(currRow$time)
    
    if(isPurchaseOn ){
      sellFlag=FALSE;
      #buying row will be the last one.
      buyingRow=intra.purchase.positions[nrow(intra.purchase.positions),]
      buyDate=(as.character(buyingRow$BD))
      #is fetched from global data, if macd is weekly data, this will be the price when then week ends
      buyingPrice=as.numeric(buyingRow$BP)
      
      
      
      #checking whether price has increased.
      difference=currPrice-buyingPrice
      ratio=difference/buyingPrice
      cat(buyDate,buyingPrice,currPrice,difference,ratio,i,'\n')
      if(ratio>.02 || difference>1){
        sellFlag=TRUE
      }
      #sellFlag=sellFlag||(hasMacdCrossed && !is.na(isMacdLess) && isMacdLess)
      if(sellFlag){
        print('sell================>')
        isPurchaseOn=FALSE
        profit=(currPrice-buyingPrice)
        profitPerc=profit/buyingPrice
        intra.purchase.positions[nrow(intra.purchase.positions)+1, ] <- c('S',currDate,buyDate,(currPrice-buyingPrice),profitPerc,buyingPrice,currPrice)
      }
    }
    if(!isPurchaseOn && isRising(2,macd_tech_data,i,'history')){
      cat('buy==>',currDate,currPrice,i,'\n')
      isPurchaseOn=TRUE
      intra.purchase.positions[nrow(intra.purchase.positions)+1, ] <- c('B',0,currDate,0,0,currPrice,-1)
    }
  }
  return(intra.purchase.positions)
}

grouped_data<-macd_tech_data%>%  mutate(day = format(as.Date(as.character(time)), "%Y-%m-%d")) %>% group_by(day) %>% summarise(Day = min(day))
intra.purchase.positions<-data.frame()
for(dayIndex in 1:nrow(grouped_data)){
  day=grouped_data[dayIndex,]$Day
  # print(day)
  day2<-as.Date(day) + days(1)
  # print(day2)
  # cat('day....',day,day2,'\n')
  dayData<-subset(macd_tech_data,time>day&time<day2)
  
  cat('new-day===>',day,nrow(dayData),'\n')
  purchasePosition<-checkTrading(dayData)
  intra.purchase.positions<-rbind(intra.purchase.positions,purchasePosition)
}

intra.purchase.positions$day=format(as.Date(as.character(intra.purchase.positions$BD)))
nrow(subset(intra.purchase.positions,Type=='B'))
nrow(subset(intra.purchase.positions,Type=='S'))
grouped_by_day<-intra.purchase.positions%>%group_by(day) %>% summarise(Count = length(Type),B = length(Type[Type=='B']),S = length(Type[Type=='S']))
# length(Symbol[ProfitPerc>0])
