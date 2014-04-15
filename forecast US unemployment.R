setwd("U:/gwdata")
rm(list=ls())
library(xlsx)
library(sandwich)
source('gcv.r')
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
plot.ts(une,col=4,ylab='Monthly Unemp Rate')
abline(h=5,col=2)

# AR(1) forecast model
T<-length(une)
une.lead<-diff(une[-1])
une.lag1<-diff(une[-T])
gcv(y=une.lead,X=une.lag1,P=100, Window='recursive')$mat
summary(lm(une.lead~une.lag1)) # model check

over <- numeric(197)
for (i in 2:floor(198)){
  over[i]<-gcv(y=une.lead,X=une.lag1,P=i, Window='recursive')$mat[2,2]
}
sum(over)/(197) # averaging over all prediction windows