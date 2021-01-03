validDates=list( list(start="2013-01-01",end="2013-12-31")
                ,list(start="2014-01-01",end="2014-12-31")
                ,list(start="2015-01-01",end="2015-12-31")
                ,list(start="2016-01-01",end="2016-12-30")
                ,list(start="2017-01-02",end="2017-12-29")
                ,list(start="2018-01-01",end="2018-12-31")
                ,list(start="2019-01-01",end="2019-12-31")
                ,list(start="2020-01-01",end="2020-12-18")
                )

hightTickers<-list("BAJFINANCE","HDFC","AARTIIND","PIDILITIND","HDFCBANK","BERGEPAINT","BAJAJFINSV","KOTAKBANK","PGHH","ASIANPAINT","SRF","BRITANNIA","HAVELLS","TCS","NAVINFLUOR","IGL","HCLTECH","CUB","HINDUNILVR","DABUR","DIVISLAB","BATAINDIA","RELIANCE","PIIND","TRENT")
for(tickerName in hightTickers){
  fqnTickerName<-sprintf('%s%s',tickerName,'.NS')
  tickerData<-getOrgTickerData(fqnTickerName);
  closeColumnName<-sprintf('%s%s',fqnTickerName,".Close")
  profitPerYear=list();
  for (year in validDates){
    yearBeginCandle<-tickerData[which(index(tickerData) == year$start),]
    yearEndCandle<-tickerData[which(index(tickerData) == year$end),]
    yearStartValue<-as.numeric(yearBeginCandle[1,closeColumnName])
    yearEndValue<-as.numeric(yearEndCandle[1,closeColumnName])
    profitPerc<-(yearEndValue-yearStartValue)/yearStartValue
    profitPerYear[[year$start]]=profitPerc
    #print(paste("start:",yearBeginCandle[1,closeColumnName],"end:",yearEndCandle[1,closeColumnName],"profit:",profitPerc))
  }
  print(paste(profitPerYear[["2013-01-01"]],profitPerYear[["2014-01-01"]],profitPerYear[["2015-01-01"]],
              profitPerYear[["2016-01-01"]],profitPerYear[["2017-01-02"]],profitPerYear[["2018-01-01"]]
              ,profitPerYear[["2019-01-01"]],profitPerYear[["2020-01-01"]]))
}

test=list()
test[["end"]]="w"
length(test)
