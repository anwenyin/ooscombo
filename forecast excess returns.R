setwd("U:/gwdata")
rm(list=ls())
library(xlsx)
library(sandwich)
source('goos.r')
source('supfun.r')
source('feval.r')


#############################################################
##############  Goyal-Welch 2008 Data #######################
#############################################################

# sheet 1: monthly data, 1704 obs, 18 col
# sheet 2: quarterly data, 568 obs, 22 col
# sheet 3: annual data, 142 obs, 21 col

# This exercise follows Rapach, Strauss and Guo 2010 RFS results

gw <- read.xlsx2("gw.xlsx", sheetIndex=2,colClasses=rep('numeric',22))

# RSG uses quarterly data from 1947:Q1 to 2005:Q4, they use 3 OOS periods
# OOS-1: P = 164, 1965:Q1 - 2005:Q4
# OOS-2: P = 120, 1976:Q1 - 2005:Q4
# OOS-3: P = 24,  2000:Q1 - 2005:Q4

dta <- gw[305:540,] #1947.1 - 2005.4, Zhou and Rapach Sample

n <- nrow(dta) # 236 obs
P<-120

e.ret <- dta$CRSP_SPvwx-dta$Rfree # excess return

dy<-numeric(n)
for (i in 1:n){
  dy[i]<-log(dta$D12[i+1])-log(dta$Index[i]) # log dividend-yield ratio
}



dp<-(log(dta$D12)-log(dta$Index)) # log divident price ratio
ep<-(log(dta$E12)-log(dta$Index)) # log earnings price ratio
de<-(log(dta$D12)-log(dta$E12)) #log dividend payout ratio

svar<-dta$svar # stock variance
csp<-dta$csp # cross sectional premium, NaN end of sample
bm<-dta$b.m # book to market ratio
ntis<-dta$ntis # net equity expansiongw

tbl<-dta$tbl # short term treasury bills
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
dfy<-dfy[-n]
dfr<-dfr[-n]
infl<-infl[-n]

theta<-1 # Stock-Watson combination discount factor
feval(y=e.ret,X=dy,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=dp,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=ep,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=de,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=svar,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=bm,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=ntis,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=dfy,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=dfr,P=P, theta=theta,Window='recursive')$mat
feval(y=e.ret,X=infl,P=P, theta=theta,Window='recursive')$mat  


##### Kitchen Sink Model  ####

X<-cbind(dy,dp,ep,de,svar,bm,ntis,dfy,dfr,infl)
# check and compare forecasts
x1<-dy
tau1 <- bdate(bound=0.15,Y=e.ret,X=x1)$break.fraction
bm <- goos(y=e.ret,X=x1,P=60,Window='recursive',Break=TRUE,tau=tau1)
sm <- goos(y=e.ret,X=x1,P=60,Window='recursive')

plot.ts(round(bm$Forecast-sm$Forecast,2),col=2)
plot.ts(round(bm$Beta[,2]-sm$Beta[,2],2),col=4)

feval(y=e.ret,X=X,P=P, Window='recursive')$mat
