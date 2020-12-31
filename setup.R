
#Below steps needs to be done if you want to refresh bse and nse 200 stocks data, which should be rare thing as you might be interesed only top 20
#Step1
setwd('C:/Users/anoop/dream/running/r-stock-trading')
getwd()


#Step.2 initiating dataframes
#init quarterly/monthly return data
return.stats<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(return.stats) <- c("symbol", "success-quarters","success-macd-by-week")


#reading all 200 from bse and nse
#Step.3
combinedTickers <- read.csv(file=file.path("both", "200nb"), header=T)
#for(symbol in combinedTickers$Symbol){
# getting only the desired ones
stableSymbolList=c('BAJFINANCE.NS','BAJAJFINSV.NS','HDFCBANK.NS','HAVELLS.NS','BERGEPAINT.NS','PIDILITIND.NS','ASIANPAINT.NS','MARICO.NS','SRF.NS','KOTAKBANK.NS','RELIANCE.NS')
for(symbol in stableSymbolList){
  print(symbol)
  getSymbols(symbol,from="2014-01-01",to = (Sys.Date() + days(1)))
}


#code that was used to come up with initial list

#step.2
#init ticker symbols
tickers.ns <- read.csv(file=file.path("nifty", "200"), header=T)
tickers.bs.all <- read.csv(file=file.path("bse", "all"), header=T)
tickers.bs.200.code<- read.csv(file=file.path("bse", "200.code"), header=T)
tickers.bs<- read.csv(file=file.path("bse", "200"), header=T)
desiredSymbols<-c()
count=0
for (securityCode in tickers.bs.200.code$code){
  count=count+1
  desiredSymbol=subset(tickers.bs.all,code==securityCode)
  tickers.bs.200.code[count,'Symbol']=desiredSymbol[2]
  # print(desiredSymbol)
  # print('wreewr')
  #[length(desiredSymbols)+1]=desiredSymbol[2]
  #print(desiredCode)
}
write.csv(tickers.bs.200.code,'C:\\Users\\anoop\\dream\\running\\r-stock-trading\\bse\\200', row.names = FALSE)


#to combine both nse and bse
tickers.ns <- read.csv(file=file.path("nifty", "200"), header=T)
tickers.bs<- read.csv(file=file.path("bse", "200"), header=T)
combinedTickers<- data.frame(Symbol=numeric(), CompanyName=numeric(),Type=numeric())
c
  symbol=tickers.ns[index,'Symbol']
  companyName=tickers.ns[index,1]
  symbolName=paste(symbol,'.NS',sep = '')
  combinedTickers[nrow(combinedTickers)+1,]<-c(as.character(symbolName),as.character(companyName),'NSE')
  #combinedTickers[count,]<-c(ticker[,'Symbol'],ticker[,'Company Name'])
}
for(index in 1:nrow(tickers.bs)){
  symbol=tickers.bs[index,'Symbol']
  companyName=tickers.bs[index,'company']
  symbolName=paste(symbol,'.BO',sep = '')
  combinedTickers[nrow(combinedTickers)+1,]<-c(as.character(symbolName),as.character(companyName),'BSE')
  #combinedTickers[count,]<-c(ticker[,'Symbol'],ticker[,'Company Name'])
}
write.csv(combinedTickers,'C:\\Users\\anoop\\dream\\running\\r-stock-trading\\bse\\200nb', row.names = FALSE)
write.csv(return.stats.ns,'C:\\Users\\anoop\\dream\\running\\r-stock-trading\\returns.csv', row.names = FALSE)


paste("and some more", 'fun',sep="")
