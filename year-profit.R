stableSymbolList=c('BAJFINANCE.NS','BAJAJFINSV.NS','HDFCBANK.NS','HAVELLS.NS','BERGEPAINT.NS','PIDILITIND.NS','ASIANPAINT.NS','MARICO.NS','RELIANCE.NS','SRF.NS','KOTAKBANK.NS')
distPercList=c(0.25,0.2,0.15,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05)
#.96,.92,.92,.90,.89,.88,.87,.86,.86,.85,.84
yearlyProfit=0
for(index in 1:10){
  symbol=stableSymbolList[index]
  distPerc=distPercList[index];
  endDate=Sys.Date()
  startDate=endDate - years(1);
  buyingPrice=getClosingPrice(symbol,as.Date("2018-07-5"))
  sellingPrice=getClosingPrice(symbol,endDate)
  
  #cat(buyingPrice,sellingPrice,'\n')
  profitPerc=(sellingPrice-buyingPrice)/buyingPrice
  cat(buyingPrice,sellingPrice,symbol,profitPerc,'\n')
  diviedProfitPerc=profitPerc*distPerc
  #print(profitPerc)
  yearlyProfit=yearlyProfit+diviedProfitPerc
}
print(yearlyProfit)

# BAJFINANCE	22	18	0.9617021
# BAJAJFINSV	22	19	0.9234043
# HDFCBANK	22	19	0.9234043
# HAVELLS	22	17	0.9021277
# BERGEPAINT	22	15	0.8978723
# PIDILITIND	22	15	0.8851064
# ASIANPAINT	22	17	0.8765957
# MARICO	22	17	0.8553191
# SRF	22	17	0.8468085
# KOTAKBANK	22	18	0.8425532
# RELIANCE	22	13	0.8425532


getSymbols("BAJFINANCE.NS",from="2019-06-01",to = Sys.Date(),periodicity="daily")


