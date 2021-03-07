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


# stocks with macd above zero most no. of times is shown high in the order. for ex: above zero shows that their fast ema is greater than slow ema.
getStocksBasedOnMacd<-function(startDate,endDate,macdType,fastValue,slowValue){
  macdGoodCountsData <<- data.frame(Symbol=character(), CountAboveZero=numeric(),TotalCount=numeric()) 
  for(symbol in tickers$Symbol){
    tryCatch({
      symbolDataName<-sprintf('%s%s',symbol,'.NS')
      # getting the ticker which is alreay downloaded
      orgTickerData<-getOrgTickerData(symbolDataName)
      #get daily macd data
      #macdData=getMacdDailyDataByTicker(orgTickerData,50,100,9)
      macdData=getMacdDailyDataByTicker(orgTickerData,fastValue,slowValue,9)
      
      #filtering macd data to avo
      macdDataDateFiltered<-macdData[index(macdData)>startDate & index(macdData)<=endDate,]
      macdValueFiltered<-macdDataDateFiltered[macdDataDateFiltered$macd>0]
      print(paste(symbol,nrow(macdValueFiltered),nrow(macdDataDateFiltered)))
      macdGoodCountsData[nrow(macdGoodCountsData)+1, ] <<- c(symbol, nrow(macdValueFiltered),nrow(macdDataDateFiltered))
    },
    error=function(cond){
      print(paste('error=======================>',symbol))
    })
  }
  sorted.macdGoodData <<- macdGoodCountsData[order(-as.numeric(as.character(macdGoodCountsData$CountAboveZero))), ]
}

startDate<-"2013-01-01"
# going only till feb of 2020 to remove covid market down effect
endDate<-"2020-02-10"
getStocksBasedOnMacd(startDate,endDate,"day",100,150)

#macdGoodCountsData<-macdGoodCountsData[0,]


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

isBelowEma('ASIANPAINT')

lastCloseTicker<-asianData[nrow(asianData),"ASIANPAINT.NS.Close"]
asianData[ 1:9, ]

loopAndGetBelowEma()
loopAndGetBelowEma<-function(){
  sorted.macdGoodData <- macdGoodCountsData[order(-as.numeric(as.character(macdGoodCountsData$CountAboveZero))), ]
  topStocks<-sorted.macdGoodData[ 1:20, ]
  for(symbol in topStocks$Symbol){
    isBelow<-isBelowEma(symbol,50)
    if(isBelow){
      print(paste("is below ema 50====>",symbol))
    }
    isBelow<-isBelowEma(symbol,100)
    if(isBelow){
      print(paste("is below ema 100====>",symbol))
    }
    isBelow<-isBelowEma(symbol,150)
    if(isBelow){
      print(paste("is below ema 150====>",symbol))
    }
    isBelow<-isBelowEma(symbol,200)
    if(isBelow){
      print(paste("is below ema 200====>",symbol))
    }
  }
}

isBelowEma<-function(symbol,emaValue){
  symbolDataName<-sprintf('%s%s',symbol,'.NS')
  symbolTickerData<-getOrgTickerData(symbolDataName)
  closeColumnName<-sprintf('%s%s',symbolDataName,".Close")
  symbolEmaData <-   EMA(symbolTickerData[,closeColumnName], emaValue);
  lastCloseTicker<-symbolTickerData[nrow(symbolTickerData),closeColumnName]
  lastEma<-symbolEmaData[nrow(symbolEmaData),"EMA"]
  return(if(as.numeric(lastCloseTicker)<as.numeric(lastEma)) TRUE else FALSE)
}