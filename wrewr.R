age.data <- data.frame(age=c(10,15,20,25,30,35),salary=c(50,55,60,65,70,75))
age.data <- data.frame(age=c(10,15,20,25,30,35),salary=c(50,50,50,50,50,50))
age.data <- data.frame(age=c(10,15,20,25,30,35,40,45),salary=c(50,55,50,55,50,55,55,55))
age.data <- data.frame(age=c(0,1,2,3,4,5,6,7,8,9),salary=c(50,55,50,55,5,5,5,5,5,5))
attach(age.data)
sd(age.data$salary)
quantile(age.data$age,probs = .1)
cor(age.data$age,age.data$salary,method = "spearman")

library(dplyr) 
age.data <- data.frame(age=c(10,15,15,20,25,30,35),salary=c(50,70,55,60,65,70,75))
age.data %>% filter(age > 10)%>% group_by(age) %>% summarise(n = sum(n))
age.data %>%  group_by(age.data$age) 

age.data.1 <- data.frame(age4=c(10,15,15,20,25,30,35),salary=c(50,4,55,60,65,70,75))
age.data.1 %>%  group_by(age4) %>% summarise(Frequency = sum(salary))

check<-tail(asianpaint.macd.data$macd )
check %>%  group_by(macd)
class(age.data.1)
class(check)
as.data.table(asianpaint.macd.data)
check<-data.frame(date=index(asianpaint.macd.data), coredata(asianpaint.macd.data))
class(check)
check<-check %>%  group_by(date)
library(xts)
ts.month <- apply.monthly(as.xts(asianpaint.macd.data$macd),FUN=count)
asianpaint.macd.data$success=asianpaint.macd.data$macd>0

EMA(age.data$age,3)
age.data[,1]


chai<-data.frame(age=c(1:100))
chai$ema12=EMA(chai[,1],12)
chai$ema26=EMA(chai[,1],26)



chai200<-data.frame(age=c(1:200))
chai200$ema12=EMA(chai200[,1],12)
chai200$ema26=EMA(chai200[,1],26)
chai200$sma26=SMA(chai200[,1],26)
chai200$dummy=c(1:200)

chai200[1:30,]=c(1)

#creating xts using order by , good one
time.data<-ts(1:40, start=c(1922,1), frequency=12)
xts1 <- xts(x=1:10, order.by=Sys.Date()-1:10)
xts2 <- xts(x=1:1000,order.by = Sys.Date()-1000:1)
colnames(xts2)<-c('test')
xts2$dummy<-c(1:1)
monthlyTS <- apply.monthly(xts(xts2), mean)
monthlyTS <- apply.monthly(xts(xts2), colSums)
quantile(monthlyTS$dummy,probs = .8)
mean(monthlyTS$dummy)
mean()
#https://github.com/joshuaulrich/xts/issues/124
monthlyTS <- apply.monthly(xts(xts2), function(x) mean(x))

## simple creation and plotting
x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
x.data <- xts(rnorm(5), x.Date)
