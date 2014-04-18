setwd("U:/gwdata")
rm(list=ls())
library(xlsx)
library(sandwich)
source('R/goos.r')
source('R/feval.r')
source('R/supfun.r')


#############################################################
##############  Goyal-Welch 2008 Data #######################
#############################################################

# sheet 1: monthly data, 1704 obs, 18 col
# sheet 2: quarterly data, 568 obs, 22 col
# sheet 3: annual data, 142 obs, 21 col

# This exercise follows Rapach, Strauss and Guo 2010 RFS results

gw <- read.xlsx2("Data/gw.xlsx", sheetIndex=2,colClasses=rep('numeric',22))

# RSG uses quarterly data from 1947:Q1 to 2005:Q4, they use 3 OOS periods
# OOS-1: P = 164, 1965:Q1 - 2005:Q4
# OOS-2: P = 120, 1976:Q1 - 2005:Q4
# OOS-3: P = 24,  2000:Q1 - 2005:Q4

dta <- gw[305:540,] #1947.1 - 2005.4, Zhou and Rapach Sample
#dta <- gw[305:568,] #1947.1 - 2012.4, New Sample

n <- nrow(dta) # 236 obs
P<-164

dta.ret <- ts(gw[304:540,])
# dta.ret <- ts(gw[304:568,]) # New Sample
ret <- (dta.ret[,'Index'] + dta.ret[,'D12'])/lag(dta.ret[,'Index'],k=-1)-1
e.ret <- log1p(ret)-log1p(dta$Rfree) # excess return
dy <- log(dta.ret[,'D12'])-log(lag(dta.ret[,'Index'],-1))


dp<-(log(dta$D12)-log(dta$Index)) # log divident price ratio
ep<-(log(dta$E12)-log(dta$Index)) # log earnings price ratio
de<-(log(dta$D12)-log(dta$E12)) #log dividend payout ratio

svar<-dta$svar # stock variance
csp<-dta$csp # cross sectional premium, NaN end of sample
bm<-dta$b.m # book to market ratio
ntis<-dta$ntis # net equity expansiongw

tbl<-dta$tbl # short term treasury bills
ltr<-dta$ltr
lty<-dta$lty # long term government bond yield
tms<-(dta$lty-dta$tbl) # term spread or slope of yield curve

dfy<-(dta$AAA-dta$BAA) # Default Yield Spread or Default premium
dfr<-(dta$corpr-dta$lty) # default return spread
infl<-dta$infl # inflation

elag1<-e.ret[-n]
e.ret <- e.ret[-1]

dy<-dy[-n]
dp<-dp[-n]
ep<-ep[-n]
de<-de[-n]
svar<-svar[-n]
csp<-csp[-n]
bm<-bm[-n]
ntis<-ntis[-n]
tbl<-tbl[-n]
ltr<-ltr[-n]
lty<-lty[-n]
tms<-tms[-n]
dfy<-dfy[-n]
dfr<-dfr[-n]
infl<-infl[-n]

theta <- 1 # Stock-Watson combination discount factor
feval(y=e.ret,X=dy,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=dp,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=ep,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=de,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=svar,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=bm,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=ntis,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=tbl,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=ltr,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=lty,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=tms,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=dfy,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=dfr,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=infl,P=P, theta=theta,Window='recursive')$mat  


##### Indivisual Cumulative Square Forecast Error Comparison relative to stable historical average benchmark #####
method <- 'CV'
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=dy,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('dy',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=dp,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('dp',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=ep,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('ep',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=de,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('de',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=svar,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('svar',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=bm,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('bm',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=ntis,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('ntis',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=tbl,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('tbl',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=ltr,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('ltr',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=lty,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('lty',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=tms,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('tms',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=dfy,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('dfy',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=dfr,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('dfr',method));abline(h=0,col=2,lty=2)
plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(feval(y=e.ret,X=infl,P=P)$sfe[,method]),col=4,ylab="CSFE Diff",main=paste('infl',method));abline(h=0,col=2,lty=2)

##### Double Combination (equal weight across models) Cumulative Square Forecast Error Comparison relative to stable historical average benchmark #####
method <- 'Cp'
m <- 14
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

plot.ts(cumsum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])-cumsum(sfe.combo),col=4,ylab="CSFE Diff",main=paste('D-Combo',method));abline(h=0,col=2,lty=2)       

CT.R2 <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])) # CV is better than Cp      
CT.R2