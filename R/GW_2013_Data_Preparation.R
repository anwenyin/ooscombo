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

# change format from "1,234" to "1234"
gwa <- within(gwa, Index <- gsub(",", "", Index))
gwq <- within(gwq, Index <- gsub(",", "", Index))
gwm <- within(gwm, Index <- gsub(",", "", Index))

gwa <- within(gwa, Index <- as.numeric(Index))
gwq <- within(gwq, Index <- as.numeric(Index))
gwm <- within(gwm, Index <- as.numeric(Index))



# For quarterly data
# gw <- gwq

dta <- ts(gwq[gwq$yyyyq >= 19464,], start=c(1946,4), freq=4) #1947.1 - 2005.4, Zhou and Rapach Sample
n <- nrow(dta) - 1 # 236 obs
# P<-164

ret <- (dta[dta[,1] >= 19471,'Index'] + dta[dta[,1] >= 19471,'D12'])/(dta[dta[,1] >= 19464 & dta[,1] <= 20133,'Index']) - 1
ret <- ts(ret, start=c(1947,1), freq=4)
e.ret <- log1p(ret) - log1p(dta[,"Rfree"]) # excess returns

dy <- log(dta[dta[,1] >= 19471,'D12']) - log(dta[dta[,1] >= 19464 & dta[,1] <= 20133,'Index']) # dividend yield

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

