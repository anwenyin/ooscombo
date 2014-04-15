setwd("U:/gwdata")
rm(list=ls())
library(xlsx)
library(sandwich)
source('feval.r')
source('goos.r')
source('supfun.r')

unemp <- read.xlsx2("unemploy.xlsx", sheetIndex=1,colClasses=rep('numeric',14))
unemp <- unemp[,c(-1,-14)]
une <-t(unemp[1,])
for (i in 2:nrow(unemp)){
  une <-rbind(une,t(unemp[i,]))
}

une<-une[-(length(une)-8):-length(une)]
une<-ts(une,start=c(1948,1),freq=12)
plot.ts(une,col=4,ylab='%Rate',main="US Monthly Unemployment Rate")
abline(h=5,col=2,lty=2)

# AR(1) forecast model, forecast the difference of unemploymenty rate (unit root process)
T<-length(une)
une.lead<-diff(une[-1])
une.lag1<-diff(une[-T])
feval(y=une.lead,X=une.lag1,P=120, Window='recursive')$mat
summary(lm(une.lead~une.lag1)) # model check
