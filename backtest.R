f_add<- function(x,y){ x + y }
f_subtract<- function(x,y){ x - y }
f_multi<- function(x,y){ x * y }

operation<- function(FUN, x, y){ FUN(x , y)}
operation(f_add, 9,2)

countOfAjdacentIncreasingCloses<-function(closeData){
  countOfIncreases=0;
  for(i in 1:length(closeData)){
    if(i!=1){
      if(as.numeric(closeData[i])>as.numeric(closeData[i-1])){
        countOfIncreases<-countOfIncreases+1;
      }
    }
  }
  return(countOfIncreases)
}

countOfAjdacentIncreasingCloses(tickerData[20:25,"ASIANPAINT.NS.Close"]);
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
  #lastNDaysMaxIndex<-(indexJustBeforeTheRange+which.max(tickerData[(dataIndex-configData$nRecentDays):dataIndex,closeColumnName]));
  #print(paste(lastNDaysMinIndex,lastNDaysMaxIndex,'min Date',index(lastNDaysMinIndex)))
  
  lastNDaysMin<-tickerData[lastNDaysMinIndex,];
  #lastNDaysMax<-tickerData[lastNDaysMaxIndex,];
  
  lastNDaysMinClose<-as.numeric(tickerData[lastNDaysMinIndex,closeColumnName]);
  #lastNDaysMaxClose<-as.numeric(tickerData[lastNDaysMaxIndex,closeColumnName]);
  lastNDaysMinEmaValue<-as.numeric(emaData[lastNDaysMinIndex,1])
  
  minMaxDiffPerc<-(((closingPrice-lastNDaysMinClose)/lastNDaysMinClose)*100);
  isDiffAboveThreshold<-minMaxDiffPerc>configData$thresholdGain
  countOfBetweenCandles<-dataIndex-lastNDaysMinIndex
  isCountOfBetweenCandlesAboveThreshold<-countOfBetweenCandles>=configData$thresholdCandleCounts;
  isLowestCloseLessThanEma<-lastNDaysMinClose<lastNDaysMinEmaValue
  adjIncreasingCloseCount<-countOfAjdacentIncreasingCloses(tickerData[lastNDaysMinIndex:dataIndex,closeColumnName]);
  #print(increasingCloseCount)
  
  #i will consider it has improving based on below cases
  #1. 1.5% between max and min 2.atleast 2 candles between the max and min 3.max candle above ema
  #or if the max is still below ema
  #1. 2% between max and min 2.atleast 3 green candles between the max and min
  
  #print (paste('count of bw candles:',countOfBetweenCandles,'max close:',lastNDaysMaxClose,'max index:',lastNDaysMaxIndex,
  #             'min close',lastNDaysMinClose,'min index:',lastNDaysMinIndex,'diff%:',minMaxDiffPerc,'diff%',minMaxDiffPerc>configData$thresholdGain,
  #             'thre:',configData$thresholdGain,'diff:',isDiffAboveThreshold,'count candles:',countOfBetweenCandles>0,
  #             'is less than ema:',isLowestCloseLessThanEma,'ema value:',lastNDaysMinEmaValue,'date:',rowDate))
  status<- (isDiffAboveThreshold&&isLowestCloseLessThanEma&&isCountOfBetweenCandlesAboveThreshold
            &&(TRUE))
  #adjIncreasingCloseCount>=configData$thresholdIncreasingCloseCount
  if(status==TRUE){
    #print(paste("index:",dataIndex,"min:",lastNDaysMinIndex,"curr Index:",dataIndex,'date:',rowDate))
  }
  returnObj=list(status=status,lastNDaysMinIndex=lastNDaysMinIndex)
  return (returnObj);
}

#get cummulative sum of profit percentages
getCummSumOfProfitPerc<-function(){
  purchase.positions.list <- split(purchase.positions.new, seq(nrow(purchase.positions.new)))
  #filter the entry which has only purchase
  purchase.positions.list<-Filter(function (item) !is.na(item[["SellDate"]]), purchase.positions.list)
  cummPercentage<-Reduce(function (sofar, data) {
    currPerc<-as.numeric(data[["ProfitPerc"]])/100
    return(sofar*(1+currPerc))
  }, purchase.positions.list,1)
  return (cummPercentage)
}

backtestBySymbolAndRange<-function(tickerName,emaValue,startRange=NA,endRange=NA){
  purchase.positions.new <<- data.frame(Symbol=numeric(),LastNDaysMinDate=character(),BuyDate=character(),BuyPrice=numeric(),SellDate=character(),SellPrice=numeric(),Profit=numeric(),ProfitPerc=numeric()) 
  fqnTickerName<-sprintf('%s%s',tickerName,'.NS')
  tickerData<-getOrgTickerData(fqnTickerName)
  startRange<-ifelse(is.na(startRange),1,startRange)
  endRange<-ifelse(is.na(endRange),nrow(tickerData),endRange)
  backtestByDataAndRange(fqnTickerName,tickerData,emaValue,startRange,endRange)
  return (getCummSumOfProfitPerc())
  #print("after every")
}
backtestByDataAndRange<-function(fqnTickerName,tickerData,emaValue,startRange,endRange){
  
  closeColumnName<-sprintf('%s%s',fqnTickerName,".Close")
  tickerEmaData <-   EMA(tickerData[,closeColumnName], emaValue);
  hasPurchased<-FALSE;
  for(index in startRange:endRange){
    rowData=tickerData[index,]
    closingPrice=as.numeric(rowData[1,closeColumnName])
    emaByIndex=as.numeric(tickerEmaData[index,1])
    rowDate=index(rowData)
    #print(paste(index,emaByIndex))
    if(is.na(emaByIndex)==FALSE){
      if(hasPurchased==FALSE){
        #check to make sure purchase is not made on previous rise
        if(nrow(purchase.positions.new)>0){
          prevPurchase<-purchase.positions.new[nrow(purchase.positions.new), ];
          if(!is.na(prevPurchase$SellDate)){
            #there needs to be diffferenc of 'nrecentDays' between previous and new purchase to make sure its not using the same rise
            prevSellDateIndex=which(index(tickerData) == prevPurchase$SellDate)
            if(index-prevSellDateIndex<configData$nRecentDays){
              #print(paste("reached here in skipping!!",prevSellDateIndex,prevPurchase$SellDate))
              #next
            }
          }
        }
        statusObj<-isStockImprovingAfterDip(fqnTickerName,tickerData,tickerEmaData,index,configData)
        if(statusObj$status==TRUE){
          lastNDaysMinDate=as.character(index(tickerData[statusObj$lastNDaysMinIndex]))
          purchase.positions.new[nrow(purchase.positions.new)+1, ] <<- c(fqnTickerName,lastNDaysMinDate,
                                                                         as.character(rowDate),closingPrice,NA,NA,NA,NA)
          #print(purchase.positions.new[1,])
          #print("after adding purchase position")
          hasPurchased=TRUE;
        }
      } else if(hasPurchased==TRUE){
        recentPurchase<<-purchase.positions.new[nrow(purchase.positions.new), ];
        #print(recentPurchase)
        purchasePrice<-as.numeric(recentPurchase$BuyPrice);
        profit<-closingPrice-purchasePrice
        profitPerc<-((profit/purchasePrice)*100)
        #print(paste('profit:',profit))
        if(profitPerc>=configData$thresholdProfitPerc){
          hasPurchased<-FALSE;
          #purchase.positions.new[nrow(purchase.positions.new)+1, ] <<- c(fqnTickerName,rowDate,4,rowDate,5,4,4)
          purchase.positions.new[nrow(purchase.positions.new), ] <<- c(fqnTickerName,recentPurchase$LastNDaysMinDate, recentPurchase$BuyDate,purchasePrice,
                                                                       as.character(rowDate),closingPrice,profit,profitPerc)
        } 
        # else if(profitPerc<=-3){
        #   hasPurchased<-FALSE;
        #   purchase.positions.new[nrow(purchase.positions.new), ] <<- c(fqnTickerName,recentPurchase$LastNDaysMinDate, recentPurchase$BuyDate,purchasePrice,
        #                                                                as.character(rowDate),closingPrice,profit,profitPerc)
        # }
      } else{
        print(paste(rowDate,"not doing anything"))
      }
    }
  }
}

hightTickers<-list("BAJFINANCE","HDFC","AARTIIND","PIDILITIND","HDFCBANK","BERGEPAINT","BAJAJFINSV","KOTAKBANK","PGHH","ASIANPAINT","SRF","BRITANNIA","HAVELLS","TCS","NAVINFLUOR","IGL","HCLTECH","CUB","HINDUNILVR","DABUR","DIVISLAB","BATAINDIA","RELIANCE","PIIND","TRENT")
configData=list(nRecentDays=7,thresholdCandleCounts=3,thresholdGain=1.5,thresholdIncreasingCloseCount=3,thresholdProfitPerc=6)
for(ticker in hightTickers){
  #print(ticker)
  cumm<-backtestBySymbolAndRange(ticker,26,which(index(tickerData) == "2015-01-01"),which(index(tickerData) == "2015-12-31"))
  print(cumm)
}
validDates=list(list(start="2"))

backtestBySymbolAndRange('ASIANPAINT',26,which(index(tickerData) == "2014-01-01"),which(index(tickerData) == "2020-12-18"))

backtestBySymbolAndRange('ASIANPAINT',26,which(index(tickerData) == "2019-01-01"),which(index(tickerData) == "2019-12-31"))
backtestBySymbolAndRange('BAJFINANCE',26,which(index(tickerData) == "2020-01-01"),which(index(tickerData) == "2020-12-18"))


getSumOfProfitPerc()


Reduce(function (sofar, data) sofar*(1+data/100), list(5,14,7,9,6,5,7),1)

Reduce(function (sofar, data) sofar+data, list(5,14,7,9,6,5,7),0)
Reduce(function (sofar, data) sofar+data, list(66,66,66,66,66,66,66),0)
Reduce(function (sofar, data) sofar*(1+data/100), list(66,66,66,66,66,66,66),1)
Reduce(function (sofar, data) sofar*(1+data/100), list(5.567706444,14.7900592,7.678078602,9.572523149,6.874905659,5.323817052,6.056839314,5.665152051,5.036528857,5.463621092,5.444251573,11.10578227,5.587258021,7.10719127,5.907044064,7.450459168,8.455542602,5.347169642,5.914156487,7.018032785,5.308044484,5.14164855,5.342272799,6.168593027,8.651649793,5.191547605,5.792978786,5.391991182,5.248792778,5.853519154,5.502262646,6.087291257,5.05285508,6.89804229,8.918824957,6.920933632,8.297242119,6.361359544,11.31521008),1)




rowIndex=which(index(tickerData) == "2019-01-01")
rowIndex=which(index(tickerData) == "2019-12-31")


tested<-function(start=1){
  print(start)
}
tested();

ee<-NA
print(ee)

dude<- data.frame(Symbol=numeric(),ddate=character(),haidate=character() ) 
dude[2,]<-c(1,"eeeee","ee")
rownames(dude)

checking<-function(){
  hi<-dude[2,]
  print(hi)
  for(i in 0:-1){
    print(hi)
    #dude[nrow(dude)+1, ] <<- c(1,as.character(as.Date("2018-02-02")))
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


x=seq(1,3)
hai<-Reduce(function (sofar,data) {
  return(sofar+data)
  }, x)

x=1:10
Filter(function (x) x%%2==0, x)
