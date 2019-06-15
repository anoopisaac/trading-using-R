library(quantmod)
setwd('C:/Users/anoop/dream/running/r-stock-trading')
getwd()
tickers <- read.csv(file=file.path("nifty", "200"), header=T)
tickers$Symbol
asian <- getSymbols("ASIANPAINT.NS",auto.assign = FALSE,from="2016-01-01")
plot(asian$ASIANPAINT.NS.Close)
#returns
asian.month<-monthlyReturn(asian$ASIANPAINT.NS.Close)
plot(asian.month)
hist(asian.month)
summary(asian.month)
#greater.than.zero=asian.month[which(monthly.returns)>0]
greater.than.zero=which(asian.month$monthly.returns>0)
length(greater.than.zero)
length(asian.month)


profitMonths <- function(tickData) 
{
  monthly.return.data<-monthlyReturn(tickData[,4])
  greater.than.zero=which(monthly.return.data$monthly.returns>0)
  return(length(greater.than.zero))
}

u<-profitMonths(asian)
print(u)
