library(quantmod)

setwd('/Users/anoopisaac/projects/trading-using-R')
getwd()
tickers <- read.csv(file=file.path("nifty", "200"), header=T)
i<-1
for(symbol in tickers$Symbol){
  print(paste('starting==>',symbol,i))
  getSymbols(sprintf('%s%s',symbol,'.NS'),from="2013-01-01")
  print(paste('eding==>',symbol))
  i<-i+1
}

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

missingOnes<-c('EMAMILTD','FORTIS','GODREJPROP')
difftime("2020-12-10", "2013-01-01", units = "days") 

get(sprintf('%s%s','COFORGE','.NS'))
getSymbols(sprintf('%s%s','GODREJPROP','.NS'),from="2013-01-01")


startDate<-"2013-01-01"
endDate<-"2020-12-10"
macdGoodCountsData <- data.frame(Symbol=character(), CountAboveZero=numeric(),TotalCount=numeric()) 
#macdGoodCountsData<-macdGoodCountsData[0,]
for(symbol in tickers$Symbol){
  tryCatch({
    symbolDataName<-sprintf('%s%s',symbol,'.NS')
    orgTickerData<-getOrgTickerData(symbolDataName)
    macdData=getMacdDailyDataByTicker(orgTickerData,50,100,9)
    macdDataDateFiltered<-macdData[index(macdData)>startDate & index(macdData)<=endDate,]
    macdValueFiltered<-macdDataDateFiltered[macdDataDateFiltered$macd>0]
    print(paste(symbol,nrow(macdValueFiltered),nrow(macdDataDateFiltered)))
    macdGoodCountsData[nrow(macdGoodCountsData)+1, ] <- c(symbol, nrow(macdValueFiltered),nrow(macdDataDateFiltered))
  },
  error=function(cond){
    print(paste('error=======================>',symbol))
  })
}
macdData[macdData$macd>0]
paste("Today is", date())
hai="hello"
print(hai,"were")
paste('starting==>',hai)
get('LTI.NS')


asianData<-getOrgTickerData('ASIANPAINT.NS')
macdData=getMacdDailyDataByTicker(asianData,50,100,9)
nrow(macdData)
macdDataDateFiltered<-macdData[index(macdData)>startDate & index(macdData)<=endDate,]
macdValueFiltered<-macdDataDateFiltered[macdDataDateFiltered$macd>0]
print(paste(symbol,nrow(macdValueFiltered),nrow(macdDataDateFiltered)))
macdGoodCountsData[nrow(macdGoodCountsData)+1, ] <- c(symbol, nrow(macdValueFiltered),nrow(macdDataDateFiltered))

