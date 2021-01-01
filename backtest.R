f_add<- function(x,y){ x + y }
f_subtract<- function(x,y){ x - y }
f_multi<- function(x,y){ x * y }

operation<- function(FUN, x, y){ FUN(x , y)}
operation(f_add, 9,2)


#nRecentDays : denotes the previous days considered to check the patterns
#nRecentPositives : how many recent positives
isStockImprovingAfterDip<-function(fqnTickerName,tickerData,emaData,dataIndex,configData){
  #dataIndex,nRecentDays,countOfCandles
  emaValue<-as.numeric(emaData[dataIndex,1])
  if(is.na(emaValue)){
    return(FALSE)
  }
  closeColumnName<-sprintf('%s%s',fqnTickerName,".Close")
  openColumnName<-sprintf('%s%s',fqnTickerName,".Open")
  rowData=tickerData[dataIndex,]
  rowDate=index(rowData)
  closingPrice=as.numeric(rowData[1,closeColumnName])
  
  
  #this is needed as to account for the index returned by the below range
  indexJustBeforeTheRange=(dataIndex-configData$nRecentDays-1)
  
  lastNDaysData<-tickerData[(dataIndex-configData$nRecentDays):dataIndex,]
  lastNDaysMinIndex<-(indexJustBeforeTheRange+which.min(tickerData[(dataIndex-configData$nRecentDays):dataIndex,closeColumnName]));
  lastNDaysMaxIndex<-(indexJustBeforeTheRange+which.max(tickerData[(dataIndex-configData$nRecentDays):dataIndex,closeColumnName]));
  #print(paste(lastNDaysMinIndex,lastNDaysMaxIndex,'min Date',index(lastNDaysMinIndex)))
  
  lastNDaysMin<-tickerData[lastNDaysMinIndex,];
  lastNDaysMax<-tickerData[lastNDaysMaxIndex,];
  
  lastNDaysMinClose<-as.numeric(tickerData[lastNDaysMinIndex,closeColumnName]);
  lastNDaysMaxClose<-as.numeric(tickerData[lastNDaysMaxIndex,closeColumnName]);
  lastNDaysMinEmaValue<-as.numeric(emaData[lastNDaysMinIndex,1])
  
  minMaxDiffPerc<-(((lastNDaysMaxClose-lastNDaysMinClose)/lastNDaysMinClose)*100);
  isDiffAboveThreshold<-minMaxDiffPerc>configData$thresholdGain
  countOfBetweenCandles<-lastNDaysMaxIndex-lastNDaysMinIndex
  isCountOfBetweenCandlesAboveThreshold<-countOfBetweenCandles>=configData$thresholdCandleCounts;
  isLowestCloseLessThanEma<-lastNDaysMinClose<lastNDaysMinEmaValue
  
  #i will consider it has improving based on below cases
  #1. 1.5% between max and min 2.atleast 2 candles between the max and min 3.max candle above ema
  #or if the max is still below ema
  #1. 2% between max and min 2.atleast 3 green candles between the max and min
  
  #print (paste('count of bw candles:',countOfBetweenCandles,'max close:',lastNDaysMaxClose,'max index:',lastNDaysMaxIndex,
  #             'min close',lastNDaysMinClose,'min index:',lastNDaysMinIndex,'diff%:',minMaxDiffPerc,'diff%',minMaxDiffPerc>configData$thresholdGain,
  #             'thre:',configData$thresholdGain,'diff:',isDiffAboveThreshold,'count candles:',countOfBetweenCandles>0,
  #             'is less than ema:',isLowestCloseLessThanEma,'ema value:',lastNDaysMinEmaValue,'date:',rowDate))
  status<- (isDiffAboveThreshold&&(countOfBetweenCandles>0)&&isLowestCloseLessThanEma&&isCountOfBetweenCandlesAboveThreshold)
  if(status==TRUE){
    print(paste("index:",dataIndex,"min:",lastNDaysMinIndex,"max:",lastNDaysMaxIndex,'date:',rowDate))
  }
  return (status);
}


backtestBySymbolAndRange<-function(tickerName,emaValue,startRange=NA,endRange=NA){
  purchase.positions.new <<- data.frame(Symbol=numeric(),BuyDate=character(),BuyPrice=numeric(),SellDate=character(),SellPrice=numeric(),Profit=numeric(),ProfitPerc=numeric()) 
  fqnTickerName<-sprintf('%s%s',tickerName,'.NS')
  tickerData<-getOrgTickerData(fqnTickerName)
  startRange<-ifelse(is.na(startRange),1,startRange)
  endRange<-ifelse(is.na(endRange),nrow(tickerData),endRange)
  backtestByDataAndRange(fqnTickerName,tickerData,emaValue,startRange,endRange)
  print("after every")
}
backtestByDataAndRange<-function(fqnTickerName,tickerData,emaValue,startRange,endRange){
  
  closeColumnName<-sprintf('%s%s',fqnTickerName,".Close")
  tickerEmaData <-   EMA(tickerData[,closeColumnName], emaValue);
  hasPurchased<-FALSE;
  configData=list(nRecentDays=7,thresholdCandleCounts=3,thresholdGain=1.5)
  for(index in startRange:endRange){
    rowData=tickerData[index,]
    closingPrice=as.numeric(rowData[1,closeColumnName])
    emaByIndex=as.numeric(tickerEmaData[index,1])
    rowDate=index(rowData)
    #print(paste(index,emaByIndex))
    if(is.na(emaByIndex)==FALSE){
      if(hasPurchased==FALSE){
        isImproving<-isStockImprovingAfterDip(fqnTickerName,tickerData,tickerEmaData,index,configData)
        if(isImproving==TRUE){
         
          purchase.positions.new[nrow(purchase.positions.new)+1, ] <<- c(fqnTickerName,as.character(rowDate),closingPrice,NA,NA,NA,NA)
          print(purchase.positions.new[1,])
          print("after adding purchase position")
          hasPurchased=TRUE;
        }
      } else if(hasPurchased==TRUE){
        recentPurchase<<-purchase.positions.new[nrow(purchase.positions.new), ];
        print(recentPurchase)
        purchasePrice<-as.numeric(recentPurchase$BuyPrice);
        profit<-closingPrice-purchasePrice
        profitPerc<-((profit/purchasePrice)*100)
        #print(paste('profit:',profit))
        if(profitPerc>=5){
          hasPurchased<-FALSE;
          #purchase.positions.new[nrow(purchase.positions.new)+1, ] <<- c(fqnTickerName,rowDate,4,rowDate,5,4,4)
          purchase.positions.new[nrow(purchase.positions.new), ] <<- c(fqnTickerName, recentPurchase$BuyDate,purchasePrice,
                                                                       as.character(rowDate),closingPrice,profit,profitPerc)
        }
      } else{
        print(paste(rowDate,"not doing anything"))
      }
    }
  }
}
backtestBySymbolAndRange('ASIANPAINT',26,1947,1953)


rowIndex=which(index(tickerData) == "2020-11-26")
rowIndex=which(index(tickerData) == "2020-12-01")


tested<-function(start=1){
  print(start)
}
tested();

ee<-NA
print(ee)

dude<- data.frame(Symbol=numeric(),ddate=character() ) 

checking<-function(){
  for(i in 10:120){
    print("edone")
    dude[nrow(dude)+1, ] <<- c(1,as.character(as.Date("2018-02-02")))
  }
}

checking()




fqnTickerName<-'ASIANPAINT.NS'
tickerData<-getOrgTickerData(fqnTickerName)
tickerEmaData <-EMA(tickerData[,"ASIANPAINT.NS.Close"], 26);
configData=list(nRecentDays=7,thresholdCandleCounts=3,thresholdGain=1.5)
for(index in 1:nrow(tickerData)){
  isStockImprovingAfterDip('ASIANPAINT.NS',tickerData,tickerEmaData,index,configData)
}
isStockImprovingAfterDip('ASIANPAINT.NS',tickerData,tickerEmaData,1953,configData)

index(tickerData[1950])
tickerEmaData[1950]

tickerData[1954]
minIndex=8
indexJustBeforeTheRange=(1947-7-1)
tickerData[indexJustBeforeTheRange+1,]
closeColumnName<-'ASIANPAINT.NS.Close'
rowdata<-tickerData[1947,closeColumnName]
min(tickerData[(1947-7):1947,"ASIANPAINT.NS.Close"])


rowdata[[closeColumnName]]

rowIndex=which(index(tickerData) == "2020-11-26")
rowIndex=which(index(tickerData) == "2020-12-18")

a=list(b=2)
a$b

last7Days<-ASIANPAINT.NS[1960:1966,]
last7Days[1,][[4]]

last7Days.list <- split(last7Days, seq(nrow(last7Days)))
what<-Reduce(function (sofar, data) sofar|data[[4]]>4000, last7Days.list,FALSE)

inZeroAndOnes<-Map({function (ticker) ifelse(ticker[[4]]-ticker[[1]]>0,1,0)}, last7Days.list)
what<-Reduce(function (x, y) x&y, inZeroAndOnes[(length(inZeroAndOnes)-7):length(inZeroAndOnes)])


last7Days.list



tickerData
getColValueByDate<-function(timeData,date,columnName){
  return(as.numeric(timeData[date,columnName]))
}

startIndex=which(index(dailyMacdData) == "2018-01-01")

#checking whether symbols are downloaded
for(symbol in tickers$Symbol){
  tryCatch({
    print(paste('starting==>',symbol,i))
    get(sprintf('%s%s',symbol,'.NS'))
    print(paste('eding==>',symbol))
    i<-i+1
  },
  error=function(cond){
    print(paste('error================>',symbol))
  })
  
}


fun<-list(a=2,b=2)
fun$a
#how to use map function
x=1:100
hello="ASIANPAINT.NS.Open"
last7Days.list[[1]](hello)
y=Map({function (a) a[[4]]-a[[4]]}, last7Days.list)
y=unlist(y)


x=seq(1,10,0.5)
Reduce(function (x, y) x+y, x)

x=1:10
Filter(function (x) x%%2==0, x)
