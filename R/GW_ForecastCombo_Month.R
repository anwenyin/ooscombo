#### Monthly Data #########################################

rm(list=ls())
library(sandwich)
source('R/goos.r')
source('R/feval.r')
source('R/supfun.r')

gw.m <- read.table("Data/gw_month_192701_201312.txt", sep="|", header=TRUE)
gw.q <- read.table("Data/gw_quarter_19471_20134.txt", sep="|", header=TRUE)
gw.a <- read.table("Data/gw_annual_1927_2013.txt", sep="|", header=TRUE)

P.m <- 684 # OOS starts from 1957:01 to 2013:12
P.q <- 196 # OOS starts from 1965:1 to 2013:4
# P.q.1 <- 152 # OOS starts from 1976:1 to 2013:4
P.a <- 49  # OOS starts from 1965 to 2013
pdf(file="Graph/combo_month.pdf")
par(mfrow=c(2,3))

attach(gw.m)
P <- P.m
w <- 1/14

benchmark <- feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable']

method <- 'Cp'

m1 <- feval(y=e.ret,X=dy,P=P)$forecast[,method]*w
m2 <- feval(y=e.ret,X=dp,P=P)$forecast[,method]*w
m3 <- feval(y=e.ret,X=ep,P=P)$forecast[,method]*w
m4 <- feval(y=e.ret,X=de,P=P)$forecast[,method]*w
m5 <- feval(y=e.ret,X=svar,P=P)$forecast[,method]*w
m6 <- feval(y=e.ret,X=bm,P=P)$forecast[,method]*w
m7 <- feval(y=e.ret,X=ntis,P=P)$forecast[,method]*w
m8 <- feval(y=e.ret,X=tbl,P=P)$forecast[,method]*w
m9 <- feval(y=e.ret,X=ltr,P=P)$forecast[,method]*w
m10 <- feval(y=e.ret,X=lty,P=P)$forecast[,method]*w
m11 <- feval(y=e.ret,X=tms,P=P)$forecast[,method]*w
m12 <- feval(y=e.ret,X=dfy,P=P)$forecast[,method]*w
m13 <- feval(y=e.ret,X=dfr,P=P)$forecast[,method]*w
m14 <- feval(y=e.ret,X=infl,P=P)$forecast[,method]*w
f.combo <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12+m13+m14

T <- length(e.ret)
R <- T - P
yp <- e.ret[(R+1):T]
sfe.combo <- (yp - f.combo)^2

plot.ts(ts(cumsum(benchmark)-cumsum(sfe.combo), start=c(1957,1), freq=12),col=4,ylim=c(-0.156, 0.156), xlab="", ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Cp <- 100*(1 - sum(sfe.combo)/sum(benchmark)) 

method <- 'S-W'

m1 <- feval(y=e.ret,X=dy,P=P)$forecast[,method]*w
m2 <- feval(y=e.ret,X=dp,P=P)$forecast[,method]*w
m3 <- feval(y=e.ret,X=ep,P=P)$forecast[,method]*w
m4 <- feval(y=e.ret,X=de,P=P)$forecast[,method]*w
m5 <- feval(y=e.ret,X=svar,P=P)$forecast[,method]*w
m6 <- feval(y=e.ret,X=bm,P=P)$forecast[,method]*w
m7 <- feval(y=e.ret,X=ntis,P=P)$forecast[,method]*w
m8 <- feval(y=e.ret,X=tbl,P=P)$forecast[,method]*w
m9 <- feval(y=e.ret,X=ltr,P=P)$forecast[,method]*w
m10 <- feval(y=e.ret,X=lty,P=P)$forecast[,method]*w
m11 <- feval(y=e.ret,X=tms,P=P)$forecast[,method]*w
m12 <- feval(y=e.ret,X=dfy,P=P)$forecast[,method]*w
m13 <- feval(y=e.ret,X=dfr,P=P)$forecast[,method]*w
m14 <- feval(y=e.ret,X=infl,P=P)$forecast[,method]*w
f.combo <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12+m13+m14

T <- length(e.ret)
R <- T - P
yp <- e.ret[(R+1):T]
sfe.combo <- (yp - f.combo)^2

plot.ts(ts(cumsum(benchmark)-cumsum(sfe.combo), start=c(1957,1), freq=12),col=4,ylim=c(-0.156, 0.156),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.SW <- 100*(1 - sum(sfe.combo)/sum(benchmark))      

method <- 'Equal'

m1 <- feval(y=e.ret,X=dy,P=P)$forecast[,method]*w
m2 <- feval(y=e.ret,X=dp,P=P)$forecast[,method]*w
m3 <- feval(y=e.ret,X=ep,P=P)$forecast[,method]*w
m4 <- feval(y=e.ret,X=de,P=P)$forecast[,method]*w
m5 <- feval(y=e.ret,X=svar,P=P)$forecast[,method]*w
m6 <- feval(y=e.ret,X=bm,P=P)$forecast[,method]*w
m7 <- feval(y=e.ret,X=ntis,P=P)$forecast[,method]*w
m8 <- feval(y=e.ret,X=tbl,P=P)$forecast[,method]*w
m9 <- feval(y=e.ret,X=ltr,P=P)$forecast[,method]*w
m10 <- feval(y=e.ret,X=lty,P=P)$forecast[,method]*w
m11 <- feval(y=e.ret,X=tms,P=P)$forecast[,method]*w
m12 <- feval(y=e.ret,X=dfy,P=P)$forecast[,method]*w
m13 <- feval(y=e.ret,X=dfr,P=P)$forecast[,method]*w
m14 <- feval(y=e.ret,X=infl,P=P)$forecast[,method]*w
f.combo <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12+m13+m14

T <- length(e.ret)
R <- T - P
yp <- e.ret[(R+1):T]
sfe.combo <- (yp - f.combo)^2

plot.ts(ts(cumsum(benchmark)-cumsum(sfe.combo), start=c(1957,1), freq=12),col=4,ylim=c(-0.156, 0.156),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Equal <- 100*(1 - sum(sfe.combo)/sum(benchmark))    

method <- 'SIC'

m1 <- feval(y=e.ret,X=dy,P=P)$forecast[,method]*w
m2 <- feval(y=e.ret,X=dp,P=P)$forecast[,method]*w
m3 <- feval(y=e.ret,X=ep,P=P)$forecast[,method]*w
m4 <- feval(y=e.ret,X=de,P=P)$forecast[,method]*w
m5 <- feval(y=e.ret,X=svar,P=P)$forecast[,method]*w
m6 <- feval(y=e.ret,X=bm,P=P)$forecast[,method]*w
m7 <- feval(y=e.ret,X=ntis,P=P)$forecast[,method]*w
m8 <- feval(y=e.ret,X=tbl,P=P)$forecast[,method]*w
m9 <- feval(y=e.ret,X=ltr,P=P)$forecast[,method]*w
m10 <- feval(y=e.ret,X=lty,P=P)$forecast[,method]*w
m11 <- feval(y=e.ret,X=tms,P=P)$forecast[,method]*w
m12 <- feval(y=e.ret,X=dfy,P=P)$forecast[,method]*w
m13 <- feval(y=e.ret,X=dfr,P=P)$forecast[,method]*w
m14 <- feval(y=e.ret,X=infl,P=P)$forecast[,method]*w
f.combo <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12+m13+m14

T <- length(e.ret)
R <- T - P
yp <- e.ret[(R+1):T]
sfe.combo <- (yp - f.combo)^2

plot.ts(ts(cumsum(benchmark)-cumsum(sfe.combo), start=c(1957,1), freq=12),col=4,ylim=c(-0.156, 0.156),xlab="",ylab="CDSFE",main=paste("DMSFE"));abline(h=0,col=2,lty=2)       

R2.SIC <- 100*(1 - sum(sfe.combo)/sum(benchmark)) 

method <- 'Break'

m1 <- feval(y=e.ret,X=dy,P=P)$forecast[,method]*w
m2 <- feval(y=e.ret,X=dp,P=P)$forecast[,method]*w
m3 <- feval(y=e.ret,X=ep,P=P)$forecast[,method]*w
m4 <- feval(y=e.ret,X=de,P=P)$forecast[,method]*w
m5 <- feval(y=e.ret,X=svar,P=P)$forecast[,method]*w
m6 <- feval(y=e.ret,X=bm,P=P)$forecast[,method]*w
m7 <- feval(y=e.ret,X=ntis,P=P)$forecast[,method]*w
m8 <- feval(y=e.ret,X=tbl,P=P)$forecast[,method]*w
m9 <- feval(y=e.ret,X=ltr,P=P)$forecast[,method]*w
m10 <- feval(y=e.ret,X=lty,P=P)$forecast[,method]*w
m11 <- feval(y=e.ret,X=tms,P=P)$forecast[,method]*w
m12 <- feval(y=e.ret,X=dfy,P=P)$forecast[,method]*w
m13 <- feval(y=e.ret,X=dfr,P=P)$forecast[,method]*w
m14 <- feval(y=e.ret,X=infl,P=P)$forecast[,method]*w
f.combo <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12+m13+m14

T <- length(e.ret)
R <- T - P
yp <- e.ret[(R+1):T]
sfe.combo <- (yp - f.combo)^2

plot.ts(ts(cumsum(benchmark)-cumsum(sfe.combo), start=c(1957,1), freq=12),col=4,ylim=c(-0.156, 0.156),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Break <- 100*(1 - sum(sfe.combo)/sum(benchmark))      

method <- 'Stable'

m1 <- feval(y=e.ret,X=dy,P=P)$forecast[,method]*w
m2 <- feval(y=e.ret,X=dp,P=P)$forecast[,method]*w
m3 <- feval(y=e.ret,X=ep,P=P)$forecast[,method]*w
m4 <- feval(y=e.ret,X=de,P=P)$forecast[,method]*w
m5 <- feval(y=e.ret,X=svar,P=P)$forecast[,method]*w
m6 <- feval(y=e.ret,X=bm,P=P)$forecast[,method]*w
m7 <- feval(y=e.ret,X=ntis,P=P)$forecast[,method]*w
m8 <- feval(y=e.ret,X=tbl,P=P)$forecast[,method]*w
m9 <- feval(y=e.ret,X=ltr,P=P)$forecast[,method]*w
m10 <- feval(y=e.ret,X=lty,P=P)$forecast[,method]*w
m11 <- feval(y=e.ret,X=tms,P=P)$forecast[,method]*w
m12 <- feval(y=e.ret,X=dfy,P=P)$forecast[,method]*w
m13 <- feval(y=e.ret,X=dfr,P=P)$forecast[,method]*w
m14 <- feval(y=e.ret,X=infl,P=P)$forecast[,method]*w
f.combo <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12+m13+m14

T <- length(e.ret)
R <- T - P
yp <- e.ret[(R+1):T]
sfe.combo <- (yp - f.combo)^2

plot.ts(ts(cumsum(benchmark)-cumsum(sfe.combo), start=c(1957,1), freq=12),col=4,ylim=c(-0.156, 0.156),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Stable <- 100*(1 - sum(sfe.combo)/sum(benchmark))      


results<-round(c(R2.Cp, R2.SW, R2.Equal, R2.SIC, R2.Break, R2.Stable),3)
names(results)<-c('Cp', 'DMSFE', 'Equal', 'SIC', 'Break', 'Stable')
print(results)

dev.off()
detach(gw.m)
print("DONE!")

#   Cp    DMSFE  Equal   SIC   Break Stable 
# 10.484 10.441 10.406 10.406 10.635 10.143 