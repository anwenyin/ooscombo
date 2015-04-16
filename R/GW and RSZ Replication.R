rm(list=ls())
#setwd("M:/My R Projects/ooscombo/ooscombo")
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

attach(gw.q)

method <- 'Stable'
P <- P.q

pdf(file="Graph/cdsfe_RSZ.pdf")
par(mfrow = c(4,4))

benchmark <- ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable']),start=c(1965,1), freq=4)

plot.ts(benchmark - cumsum(feval(y=e.ret,X=dp,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('dp'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=dy,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('dy'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=ep,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('ep'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=de,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('de'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=tms,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('tms'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=dfy,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('dfy'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=dfr,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('dfr'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=svar,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('svar'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=bm,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('bm'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=ntis,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('ntis'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=tbl,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('tbl'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=lty,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('lty'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=ltr,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('ltr'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=infl,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.156, 0.1),main=paste('infl'));abline(h=0,col=2,lty=2)

dev.off()
detach(gw.q)

##### Annual Data ###

attach(gw.a)

method <- 'Stable'
P <- P.a

par(mfrow = c(4,4))

benchmark <- ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable']),start=c(1965), freq=1)

plot.ts(benchmark - cumsum(feval(y=e.ret,X=dp,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('dp'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=dy,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('dy'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=ep,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('ep'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=de,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('de'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=tms,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('tms'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=dfy,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('dfy'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=dfr,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('dfr'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=svar,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('svar'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=bm,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('bm'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=ntis,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('ntis'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=tbl,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('tbl'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=lty,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('lty'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=ltr,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('ltr'));abline(h=0,col=2,lty=2)
plot.ts(benchmark - cumsum(feval(y=e.ret,X=infl,P=P)$sfe[,method]),col=4,ylab="", xlab="", ylim=c(-0.2, 0.15),main=paste('infl'));abline(h=0,col=2,lty=2)

detach(gw.a)
print("DONE!")

################################
#### Forecast Combination ######
################################

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
method <- 'CV'
w <- 1/14

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

benchmark <- ts(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'], start = c(1957,1),freq=12)

plot.ts(cumsum(benchmark) - cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.15), xlab="", ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.CV <- 100*(1 - sum(sfe.combo)/sum(benchmark)) # CV is better than Cp      

method <- 'Cp'
w <- 1/14
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

plot.ts(cumsum(benchmark)-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.15), xlab="", ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Cp <- 100*(1 - sum(sfe.combo)/sum(benchmark)) 

method <- 'S-W'
w <- 1/14
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

plot.ts(cumsum(benchmark)-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.15),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.SW <- 100*(1 - sum(sfe.combo)/sum(benchmark))      

method <- 'Equal'
w <- 1/14
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

plot.ts(cumsum(benchmark)-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.15),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Equal <- 100*(1 - sum(sfe.combo)/sum(benchmark))    


method <- 'Stable'
w <- 1/14
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

plot.ts(cumsum(benchmark)-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.15),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Stable <- 100*(1 - sum(sfe.combo)/sum(benchmark)) # CV is better than Cp      

method <- 'SIC'
w <- 1/14
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

plot.ts(cumsum(benchmark)-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.15),xlab="",ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.SIC <- 100*(1 - sum(sfe.combo)/sum(benchmark)) # CV is better than Cp 


result<-c(R2.CV,R2.Cp,R2.SW,R2.Equal,R2.Stable,R2.SIC)
names(result)<-c('CV','Cp','S-W','Equal','Stable','SIC')
print(result)
dev.off()
detach(gw.m)
print("DONE!")

#    CV       Cp      S-W    Equal   Stable      SIC 
# 10.14259 10.48364 10.44052 10.40646 10.14259 10.40638 

######### Annual Data #####################################

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
pdf(file="Graph/combo_annual.pdf")
par(mfrow=c(2,3))

attach(gw.a)
P <- P.a
method <- 'CV'
w <- 1/14

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

benchmark <- ts(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'], start = c(1965),freq=1) 

plot.ts(cumsum(benchmark) - cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.CV <- 100*(1 - sum(sfe.combo)/sum(benchmark)) # CV is better than Cp      

method <- 'Cp'
w <- 1/m
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Cp <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp 

method <- 'S-W'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.SW <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp      

method <- 'Equal'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Equal <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp      


method <- 'Stable'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Stable <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp      

method <- 'SIC'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.SIC <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp 


result<-c(R2.CV,R2.Cp,R2.SW,R2.Equal,R2.Stable,R2.SIC)
names(result)<-c('CV','Cp','S-W','Equal','Stable','SIC')
print(result)
dev.off()
detach(gw.a)
print("DONE!")

#   CV       Cp      S-W    Equal   Stable      SIC 
# 3.897004 3.608327 3.458617 3.206485 3.897004 3.156816 

###### Quarterly Data ##################################

rm(list=ls())
setwd("C:/Users/Anwen Yin/Documents/Projects/ooscombo")
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
pdf(file="Graph/combo_quarter.pdf")
par(mfrow=c(2,3))

attach(gw.q)
P <- P.q
method <- 'CV'
w <- 1/14

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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable']) - cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.CV <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp      

method <- 'Cp'
w <- 1/m
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Cp <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp 

method <- 'S-W'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.SW <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp      

method <- 'Equal'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Equal <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp      


method <- 'Stable'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.Stable <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp      

method <- 'SIC'
w <- 1/14
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

plot.ts(cumsum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylim=c(-0.156, 0.1),ylab="CDSFE",main=paste(method));abline(h=0,col=2,lty=2)       

R2.SIC <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=NULL,P=P)$sfe[,'Stable'])) # CV is better than Cp 

result<-c(R2.CV,R2.Cp,R2.SW,R2.Equal,R2.Stable,R2.SIC)
names(result)<-c('CV','Cp','S-W','Equal','Stable','SIC')
print(result)
dev.off()
detach(gw.q)
print("DONE!")

#   CV       Cp      S-W    Equal   Stable      SIC
# 5.071173 6.170982 6.007619 5.834679 5.071173  5.826474

