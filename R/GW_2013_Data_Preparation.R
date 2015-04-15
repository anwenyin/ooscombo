rm(list=ls())
library(sandwich)
source('R/goos.r')
source('R/feval.r')
source('R/supfun.r')

#############################################################
##############  Goyal-Welch 2013 Data #######################
#############################################################

# sheet 1: monthly data, 1704 obs, 18 col
# sheet 2: quarterly data, 568 obs, 22 col
# sheet 3: annual data, 142 obs, 21 col

# RSG uses quarterly data from 1947:Q1 to 2005:Q4, they consider 3 OOS periods
# OOS-1: P = 164, 1965:Q1 - 2005:Q4
# OOS-2: P = 120, 1976:Q1 - 2005:Q4
# OOS-3: P = 24,  2000:Q1 - 2005:Q4

# gwm05 <- read.table("Data/gw05month.txt", header=TRUE, sep="\t")
# gwq05 <- read.table("Data/gw05quarter.txt", header=TRUE, sep="\t")
# gwa05 <- read.table("Data/gw05annual.txt", header=TRUE, sep="\t")

gwm <- read.table("Data/gw13month.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
gwq <- read.table("Data/gw13quarter.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
gwa <- read.table("Data/gw13annual.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

# gwm: monthly data, 1716 obs of 18 variables
# gwq: quarterly data, 572 obs of 22 variables
# gwa: annual data, 143 obs of 21 variables

# change format from "1,234" to "1234
gwa <- within(gwa, Index <- gsub(",", "", Index))
gwq <- within(gwq, Index <- gsub(",", "", Index))
gwm <- within(gwm, Index <- gsub(",", "", Index))

gwa <- within(gwa, Index <- as.numeric(Index))
gwq <- within(gwq, Index <- as.numeric(Index))
gwm <- within(gwm, Index <- as.numeric(Index))

gwq <- gwq[,c("yyyyp","Index","D12","E12","b.m","tbl","AAA","BAA","lty","ntis","Rfree","infl","ltr","corpr","svar","csp","ik")]

gw <- gwq

dta <- gw[305:540,] #1947.1 - 2005.4, Zhou and Rapach Sample
#dta <- gw[305:568,] #1947.1 - 2012.4, New Sample

n <- nrow(dta) # 236 obs
P<-164

dta.ret <- ts(gw[304:540,], start=c(1946,4), freq=4)


ret <- (dta.ret[dta.ret[,1] >= 19471,'Index'] + dta.ret[dta.ret[,1] >= 19471,'D12'])/dta.ret[dta.ret[,1] >= 19464 & dta.ret[,1] <= 20053,'Index'] - 1


dta <- gw[305:540,] #1947.1 - 2005.4, Zhou and Rapach Sample
#dta <- gw[305:568,] #1947.1 - 2012.4, New Sample

n <- nrow(dta) # 236 obs
P<-164

dta.ret <- ts(gw[304:540,], start=c(1946,4), freq=4)
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

