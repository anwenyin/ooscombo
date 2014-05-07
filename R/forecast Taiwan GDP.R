rm(list=ls())
library(xlsx)
library(sandwich)
source('R/goos.r')
source('R/feval.r')
source('R/supfun.r')

tw <- read.csv("Data/twgdp.csv")

### Quarterly Data, 2006 real NTW dollar ###########

gdp.qr06<-tw[-1:-4,5] # 1962:Q1 - 2013:Q4, 2006 NTW dollar real growth rate
gdp.qr06<-ts(gdp.qr06,start=c(1962,1),freq=4)
plot.ts(gdp.qr06,col=4,ylab="%Rate",xlab="Year",main="Taiwan Quarterly Real GDP Growth Rate")
abline(h=0,col=2,lty=2)

# AR(1) forecast model
T<-length(gdp.qr06)
gdp.lead<-gdp.qr06[-1]
gdp.lag1<-gdp.qr06[-T]
feval(y=gdp.lead,X=gdp.lag1,P=5, Window='recursive')$mat
summary(lm(gdp.lead~gdp.lag1)) # model check

for (i in seq(20,50,by=5)){
  print(feval(y=gdp.lead,X=gdp.lag1,P=i, Window='recursive')$mat)
}


# AR(2) forecast model

gdp.lead<-gdp.qr06[-1:-2]
gdp.lag1<-gdp.qr06[c(-1,-T)]
gdp.lag2<-gdp.qr06[-(T-1):-T]
feval(y=gdp.lead,X=cbind(gdp.lag1,gdp.lag2),P=50, Window='recursive')$mat
summary(lm(gdp.lead~gdp.lag1+gdp.lag2)) # model check, AR(2) coef insignificant

for (i in seq(15,50,by=5)){
  print(feval(y=gdp.lead,X=cbind(gdp.lag1,gdp.lag2),P=i, Window='recursive')$mat)
}

### Annual Data, 2006 real NTW dollar #############

gdp.yr06<-tw[1:62,2] # 1952 - 2013, 2006 NTW dollar real growth rate
gdp.yr06<-ts(gdp.yr06,start=c(1952),freq=1)
plot.ts(gdp.yr06,col=4,ylab="%Rate",xlab="Year",main="Taiwan Annual Real GDP Growth Rate")
abline(h=0,col=2,lty=2)

# AR(1) forecast model
yr<-length(gdp.yr06)
ygdp.lead<-gdp.yr06[-1]
ygdp.lag1<-gdp.yr06[-yr]
feval(y=ygdp.lead,X=ygdp.lag1,P=25, Window='recursive')$mat
summary(lm(ygdp.lead~ygdp.lag1)) # model check

# AR(2) forecast model

gdp.lead<-gdp.yr06[-1:-2]
gdp.lag1<-gdp.yr06[c(-1,-yr)]
gdp.lag2<-gdp.yr06[-(yr-1):-yr]
feval(y=gdp.lead,X=cbind(gdp.lag1,gdp.lag2),P=25, Window='recursive')$mat
summary(lm(gdp.lead~gdp.lag1+gdp.lag2)) # model check, AR(2) coef insignificant