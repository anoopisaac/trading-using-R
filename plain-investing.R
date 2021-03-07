validDates=list( list(start="2013-01-01",end="2013-12-31")
                ,list(start="2014-01-01",end="2014-12-31")
                ,list(start="2015-01-01",end="2015-12-31")
                ,list(start="2016-01-01",end="2016-12-30")
                ,list(start="2017-01-02",end="2017-12-29")
                ,list(start="2018-01-01",end="2018-12-31")
                ,list(start="2019-01-01",end="2019-12-31")
                ,list(start="2020-01-01",end="2020-12-18")
                )

#hightTickers<-list("BAJFINANCE")
#gets the profit perc for each date range present in the validatearagnes.
#validDateRanges has as list of entries with  start and end date spaced by span
printProfitByDateRange<-function(validDateRanges,tickerNameList){
  #creating global varialbe to hold monthly profit.
  monthlyProfiPerc <<- as.data.frame(matrix( ncol =length(validDateRanges), nrow = length(tickerNameList)))
  
  #going through each ticker passed
  for(tickerIndex in 1:length(tickerNameList)){
    tickerName<-tickerNameList[tickerIndex]
    fqnTickerName<-sprintf('%s%s',tickerName,'.NS')
    tickerData<-getOrgTickerData(fqnTickerName);
    closeColumnName<-sprintf('%s%s',fqnTickerName,".Close")
    #profit percentage will be added to the bleow list, by simply finding stock price based on start and end date.
    profitList=list();
    #going through each of the span in the list
    for (validDateRange in validDateRanges){
      beginCandle<-tickerData[which(index(tickerData) == validDateRange$start),]
      endCandle<-tickerData[which(index(tickerData) == validDateRange$end),]
      startValue<-as.numeric(beginCandle[1,closeColumnName])
      endValue<-as.numeric(endCandle[1,closeColumnName])
      #print(paste(startValue,endValue))
      profitPerc<-((endValue-startValue)/startValue)
      profitList[[length(profitList)+1]]=profitPerc
      #validDateRange[["profit"]]<-profitPerc
      #print(paste("start:",yearBeginCandle[1,closeColumnName],"end:",yearEndCandle[1,closeColumnName],"profit:",profitPerc))
    }
    #   print(paste(profitPerYear[["2013-01-01"]],profitPerYear[["2014-01-01"]],profitPerYear[["2015-01-01"]],
    #               profitPerYear[["2016-01-01"]],profitPerYear[["2017-01-02"]],profitPerYear[["2018-01-01"]]
    #               ,profitPerYear[["2019-01-01"]],profitPerYear[["2020-01-01"]]))
    # 
    #onlyProfits<-Map({function (validDateRange) validDateRange[["profit"]]}, validDateRanges)
    print(sprintf("%s",profitList))
    monthlyProfiPerc[tickerIndex,]<<-profitList
    
  }
}

#generate total value by taking the sum of the tickers
getSeriesTickerSum<-function(tickerDates,tickerNameList){
  for (tickerDate in tickerDates){
    
    tickerClosePrices<-Map({function (tickerName) {
      fqnTickerName<-sprintf('%s%s',tickerName,'.NS')
      closeColumnName<-sprintf('%s%s',fqnTickerName,".Close")
      tickerData<-getOrgTickerData(fqnTickerName);
      tickerDataByDate<-tickerData[which(index(tickerData) == tickerDate),]
      closeValue<-as.numeric(tickerDataByDate[1,closeColumnName])
      return (closeValue)
    }}, tickerNameList)
  }
  print(tickerClosePrices)
  
  #inZeroAndOnes<-Map({function (ticker) ifelse(ticker[[4]]-ticker[[1]]>0,1,0)}, tickerNameList)
  #Reduce(function (sofar, data) sofar*(1+data/100), list(5,14,7,9,6,5,7),1)
}

hightTickers<-list("BAJFINANCE","HDFC","AARTIIND","PIDILITIND","HDFCBANK","BERGEPAINT","BAJAJFINSV","KOTAKBANK","PGHH","ASIANPAINT","SRF","BRITANNIA","HAVELLS","TCS","NAVINFLUOR","IGL","HCLTECH","CUB","HINDUNILVR","DABUR","DIVISLAB","BATAINDIA","RELIANCE","PIIND","TRENT")

printProfitByDateRange(months,list("BAJFINANCE"))
allDates<-getAllDaysAsString(tickerData)


getAllDaysAsString<-function(tickerData){
  dates=list()
  for(i in 1:nrow(tickerData)){
    day<-index(tickerData[i,])
    dates[[length(dates)+1]]=day
  }
  #print(paste(dates[[1]],dates[[length(dates)]]))
  return (dates)
}


getAllStartAndEndMonthDates<-function(monthSpan){
  #using one of the tickers to find all teh date
  fqnTickerName<-sprintf('%s%s',"ASIANPAINT",'.NS')
  tickerData<-getOrgTickerData(fqnTickerName)
  allDates<-getAllDaysAsString(tickerData)
  prevIndicator<-0
  prevDate<-"2013-01-01"
  #ticket months will have listMonthEntry in it, which start date and end date depending on the span.
  tickerMonths<-list();
  listMonthEntry<-list(start="2013-01-01");
  for(tickerDate in allDates){
    currMonth<-format(as.Date(tickerDate), "%m")
    #months might have to span across multiple months., 
    
    currIndicator<-as.integer((as.numeric(currMonth)-1)/monthSpan)
    if(currIndicator!=prevIndicator){
      #addding end date to current listMontEntry
      listMonthEntry[["end"]]=prevDate;
      tickerMonths[[length(tickerMonths)+1]]<-listMonthEntry
      listMonthEntry<-list(start=tickerDate)
      #print(paste("changed!",prevDate,tickerDate))
    }
    #Indicator is based on span
    prevIndicator<-currIndicator
    prevDate<-tickerDate
  }
  #need to add 'end' to current listMonthENtry; last one in the list
  listMonthEntry[["end"]]=allDates[[length(allDates)]]
  tickerMonths[[length(tickerMonths)+1]]<-listMonthEntry
  #sprintf("%s",tickerMonths)
  return(tickerMonths)
}

months<-getAllStartAndEndMonthDates()
tickerData[which(index(tickerData) == months[[1]]$start),]

printProfits<-function(){
  #monthSPan passed as parameter decides how the span should be. monthly,quarterly,halfyearly
  months<-getAllStartAndEndMonthDates(1)
  hightTickers<-list("BAJFINANCE","HDFC","AARTIIND","PIDILITIND","HDFCBANK","BERGEPAINT","BAJAJFINSV","KOTAKBANK","PGHH","ASIANPAINT","SRF","BRITANNIA","HAVELLS","TCS","NAVINFLUOR","IGL","HCLTECH","CUB","HINDUNILVR","DABUR","DIVISLAB","BATAINDIA","RELIANCE","PIIND","TRENT")
  printProfitByDateRange(months,hightTickers)
  
  profitableMonths <<- data.frame(Symbol=numeric(),countProfitableMonths=numeric())
  #monthlyProfiPerc ths one is common global field pouplated by above printProfitByDateRange method
  for(index in 1:nrow(monthlyProfiPerc)){
    profitableMonths[index,1]<<-c(hightTickers[index],)
  }
}
printProfits()


sink("output.txt")
print(sprintf("%s",months))
sink()

capture.output(sprintf("%s",months), file = "My New File.txt")


mylist = list() 
mylist[[1]] = "a"
mylist[[2]] = "b"

write.table(ASIANPAINT.NS,file="mylist.csv", quote=F,sep=",",row.names=F)

mylist=list()
mylist[[1]]=list(3,4,4)
mylist[[2]]=list(5,4,0)

lapply(mylist, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))

hai<-c(1,2)
hai[[4]]=5
hai
my.df <- as.data.frame()

myList <- list(a = c(1, 2, 3), b = c(4, 5, 6))
myDf <- as.data.frame(list())
myDf[1,]<-c(1, 2, 3)

m <- as.data.frame(matrix(0, ncol = 5, nrow = 2))
m[1,]<-list(1, 2, 3,5,6)
m[2,]<-list(1, "b", 6,5,6)
m[3,]<-c(1, 2, 6,"a",6)

write.table(m,file="mylist.csv", quote=F,sep=",",row.names=F)



  