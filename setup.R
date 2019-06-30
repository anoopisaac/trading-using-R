
#Step1.1
setwd('C:/Users/anoop/dream/running/r-stock-trading')
getwd()


#Step.1.2 initiating dataframes
#init quarterly/monthly return data
return.stats<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(return.stats) <- c("symbol", "success-quarters","success-macd-by-week")

#Step.1.3 initiating dataframes
#init quarterly/monthly return data
return.stats.bse<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(return.stats.bse) <- c("symbol", "success-quarters","success-macd-by-week")

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
for(index in 1:nrow(tickers.ns)){
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

paste("and some more", 'fun',sep="")
