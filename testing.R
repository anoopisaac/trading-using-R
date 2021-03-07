test<-list(a="eeee",b= "ewre")
test[["a"]]
test["c"]="werew"

hello<-list()
hello[[2]]=test
hello[2]


tickerDataList<-list();
tickerName<-"ASIANPAINT"
fqnTickerName<-sprintf('%s%s',tickerName,'.NS')
closeColumnName<-sprintf('%s%s',fqnTickerName,".Close")
tickerData<-getOrgTickerData(fqnTickerName);
tickerDataList[tickerName]=tickerData
hello[["erewr"]]<-tickerData
hello[["erewr"]]


