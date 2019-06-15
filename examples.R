my.matrx <- matrix(c(1:5, 6:10, 11:15), nrow = 5, ncol = 3)
my.matrx
apply(my.matrx, 1, sum)
vec <- c(1:5)
BOD
A <- function(x) x + 1
wifi <- data.frame(replicate(9,1:4))
wifi
A(2)
wifi[,4:9]
wifi[,4:9]<-apply(wifi[,4:9], MARGIN=2, FUN=A)
check
wifi
#werew
apply(wifi[,4:9], 1, SMA, n=1)
wifi
library(TTR)  #load TTR library for SMA functions

date = seq(as.Date("2016-01-01"),as.Date("2016-01-10"),"day")
value =c(1,2,3,4,5,6,7,8,9,10)
mydata = data.frame (date, value,value)
mydata[,2]<-apply(mydata[,2], MARGIN=2,FUN=function(x) x+1)

mydata['check']<-c(1,2,3,4,5,6,7,8,9,10)
mydata[,'check']<-apply(mydata[,'check',drop=F], 2,function(x) x+1)
mydata['check']<-lapply(mydata[,'check'],function(x) x+1)
lapply(mydata[,'check'],function(x) x+1)
mydata['check']=c(1,2,3,4,5,6,7,8,9,11)
mydata['hello']<-sapply(mydata['check'],function(x) x+1)


df <- data.frame('a'=c(1,2,3,4,5), 'b'=c(1,20,3,4,50))
df$c <- with(df, ifelse(a==b, a+b, b-a))
df$x <- apply(df, 1, FUN = function(x) if(x[1]==x[2]) x[1]+x[2] else x[2]-x[1])
df['y'] = lapply(df['y'],FUN = function(x) if(x[1]==x[2]) x[1]+x[2] else x[2]-x[1])

df$y=apply(df,1,FUN=function(x)2)
df[1]

mapply(df,FUN=function(x)2)


movies <- c(1,2,3,4,5,6,7,8,9,10)
movies_lower <-lapply(movies, tolower)
movies_lower <-sapply(movies, tolower)
mydata['movies']=sapply(movies, function(x)x)
str(movies_lower)

data("ttrc")
