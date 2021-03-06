#setwd("U:/gwdata")
rm(list=ls())
library(xlsx)
library(sandwich)
source('R/goos.r')
source('R/feval.r')
source('R/supfun.r')

gdp <- read.xlsx2("Data/gdp.xlsx", sheetIndex=1,colClasses=rep('numeric',7))
gdp<-gdp[-1:-8,]

### Quarterly Data, 2009 real dollar ###########

gdp.qr09<-gdp[,7] # 1947:Q2 - 2013:Q4, 2009 dollar real growth rate
gdp.qr09<-ts(gdp.qr09,start=c(1947,2),freq=4)
plot.ts(gdp.qr09,col=4,ylab="%Rate",xlab="Year",main="US Quarterly Real GDP Growth Rate")
abline(h=0,col=2,lty=2)

# AR(1) forecast model
T<-length(gdp.qr09)
gdp.lead<-gdp.qr09[-1]
gdp.lag1<-gdp.qr09[-T]
feval(y=gdp.lead,X=gdp.lag1,P=66, Window='recursive')$mat
summary(lm(gdp.lead~gdp.lag1)) # model check

# AR(2) forecast model

gdp.lead<-gdp.qr09[-1:-2]
gdp.lag1<-gdp.qr09[c(-1,-T)]
gdp.lag2<-gdp.qr09[-(T-1):-T]
feval(y=gdp.lead,X=cbind(gdp.lag1,gdp.lag2),P=66, Window='recursive')$mat
summary(lm(gdp.lead~gdp.lag1+gdp.lag2)) # model check, AR(2) coef insignificant

### Annual Data, 2009 real dollar #############

gdp.yr09<-gdp[1:84,3] # 1930 - 2013, 2009 dollar real growth rate
gdp.yr09<-ts(gdp.yr09,start=c(1930),freq=1)
plot.ts(gdp.yr09,col=4,ylab="%Rate",xlab="Year",main="US Annual Real GDP Growth Rate")
abline(h=0,col=2,lty=2)

# AR(1) forecast model
yr<-length(gdp.yr09)
ygdp.lead<-gdp.yr09[-1]
ygdp.lag1<-gdp.yr09[-yr]
feval(y=ygdp.lead,X=ygdp.lag1,P=25, Window='recursive')$mat
summary(lm(ygdp.lead~ygdp.lag1)) # model check