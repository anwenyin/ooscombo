rm(list=ls())
library(xlsx)
library(sandwich)
source('R/goos.r')
source('R/feval.r')
source('R/supfun.r')

gdp <- read.xlsx2("Data/gdp.xlsx", sheetIndex=1,colClasses=rep('numeric',7))
gdp<-gdp[-1:-8,]

tw <- read.csv("Data/twgdp.csv")

gdp.qr09<-gdp[,7] # 1947:Q2 - 2013:Q4, 2009 dollar real growth rate
gdp.qr09<-ts(gdp.qr09,start=c(1947,2),freq=4)

gdp.qr06<-tw[-1:-4,5] # 1962:Q1 - 2013:Q4, 2006 NTW dollar real growth rate
gdp.qr06<-ts(gdp.qr06,start=c(1962,1),freq=4)

plot.ts(gdp.qr09,col=4,lwd=0.5,
        pch=19,
        cex=0.6,
        ylim=c(-12,20),
        ylab="Rate",xlab="Year",
        main="Real Quarterly GDP Growth Rate")
lines(gdp.qr06,col=2,lwd=0.5,lty=2)
legend("top",horiz=TRUE,
       legend=c("U.S.", "Taiwan"),
       col=c(4,2),lty=c(1,2), bty="n")