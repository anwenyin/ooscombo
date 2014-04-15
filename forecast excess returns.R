setwd("U:/gwdata")
rm(list=ls())
library(xlsx)
library(sandwich)
source('gcv.r')
source('goos.r')
source('supfun.r')
source('feval.r')


#####################################################
##############  on Goyal-Welch Data #######################

gw <- read.xlsx2("gw.xlsx", sheetIndex=2,colClasses=rep('numeric',22)) # sheetIndex 1: monthly data, 2: quarterly, 3: annual data
dta <- gw[305:540,] #1947.1 - 2005.4, Zhou and Rapach Sample

n <- nrow(dta)
P<-120

e.ret <- dta$CRSP_SPvwx-dta$Rfree

dy<-numeric(n)
for (i in 1:n){
  dy[i]<-log(dta$D12[i+1])-log(dta$Index[i])
}



dp<-(log(dta$D12)-log(dta$Index)) # divident price ratio
ep<-(log(dta$E12)-log(dta$Index)) # earnings price ratio
de<-(log(dta$D12)-log(dta$E12)) #dividend payout ratio

svar<-dta$svar # stock variance
csp<-dta$csp # cross sectional premium
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

theta<-0.9
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


##### Multi-variable Model  

X<-cbind(dy,dp,ep,de,svar,bm,ntis,dfy,dfr,infl)
# check and compare forecasts
x1<-dy
tau1 <- bdate(bound=0.15,Y=e.ret,X=x1)$break.fraction
bm <- goos(y=e.ret,X=x1,P=60,Window='recursive',Break=TRUE,tau=tau1)
sm <- goos(y=e.ret,X=x1,P=60,Window='recursive')

plot.ts(round(bm$Forecast-sm$Forecast,2),col=2)
plot.ts(round(bm$Beta[,2]-sm$Beta[,2],2),col=4)

feval(y=e.ret,X=X,P=P, Window='recursive')$mat
