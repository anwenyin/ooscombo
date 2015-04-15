rm(list=ls())
#library(sandwich)
#source('R/goos.r')
#source('R/feval.r')
#source('R/supfun.r')

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

# change format from "1,234" to "1234", and convert to numeric
gwa <- within(gwa, {Index <- gsub(",", "", Index); Index <- as.numeric(Index)})
gwq <- within(gwq, {Index <- gsub(",", "", Index); Index <- as.numeric(Index)})
gwm <- within(gwm, {Index <- gsub(",", "", Index); Index <- as.numeric(Index)})


ret.m <- (gwm[gwm[,1] >= 192612,'Index'] + gwm[gwm[,1] >= 192612,'D12']) / (gwm[gwm[,1] >= 192611 & gwm[,1] <= 201311,'Index']) - 1
ret.q <- (gwq[gwq[,1] >= 19464,'Index'] + gwq[gwq[,1] >= 19464,'D12']) / (gwq[gwq[,1] >= 19463 & gwq[,1] <= 20133,'Index']) - 1
ret.a <- (gwa[gwa[,1] >= 1926,'Index'] + gwa[gwa[,1] >= 1926,'D12']) / (gwa[gwa[,1] >= 1925 & gwa[,1] <= 2012,'Index']) - 1

e.ret.m <- log1p(ret.m) - log1p(gwm[gwm[,1] >= 192612, "Rfree"]) # monthly data excess returns
e.ret.q <- log1p(ret.q) - log1p(gwq[gwq[,1] >= 19464, "Rfree"]) # quarterly data excess returns
e.ret.a <- log1p(ret.a) - log1p(gwa[gwa[,1] >= 1926, "Rfree"]) # annual data excess returns

# plot.ts(e.ret.m, col="steelblue")
# plot.ts(e.ret.q, col="steelblue")
# plot.ts(e.ret.a, col="steelblue")

# gwm <- gwm[gwm[,1] >= 192611, ]
# gwq <- gwq[gwq[,1] >= 19463, ]
# gwa <- gwa[gwa[,1] >= 1925, ]

# Monthly Data Variables

dp.m <- log(gwm[gwm[,1] >= 192612,'D12']) - log(gwm[gwm[,1] >= 192612,'Index']) 
dy.m <- log(gwm[gwm[,1] >= 192612,'D12']) - log(gwm[gwm[,1] >= 192611 & gwm[,1] <= 201311,'Index'])
ep.m <- log(gwm[gwm[,1] >= 192612,'E12']) - log(gwm[gwm[,1] >= 192612,'Index'])
de.m <- log(gwm[gwm[,1] >= 192612,'D12']) - log(gwm[gwm[,1] >= 192612,'E12'])

tms.m <- gwm[gwm[,1] >= 192612,'lty'] - gwm[gwm[,1] >= 192612,'tbl']
dfy.m <- gwm[gwm[,1] >= 192612,'BAA'] - gwm[gwm[,1] >= 192612,'AAA']
dfr.m <- gwm[gwm[,1] >= 192612,'corpr'] - gwm[gwm[,1] >= 192612,'ltr']

svar.m <- gwm[gwm[,1] >= 192612,'svar']
bm.m <- gwm[gwm[,1] >= 192612,'b.m']
ntis.m <- gwm[gwm[,1] >= 192612,'ntis']
tbl.m <- gwm[gwm[,1] >= 192612,'tbl']
lty.m <- gwm[gwm[,1] >= 192612,'lty']
ltr.m <- gwm[gwm[,1] >= 192612,'ltr']
infl.m <- gwm[gwm[,1] >= 192612,'infl']

dta.month <- as.data.frame(cbind(dp.m, dy.m, ep.m, de.m, tms.m, dfy.m, dfr.m, svar.m, bm.m, ntis.m, tbl.m, lty.m, ltr.m, infl.m))
dta.month <-  cbind(e.ret.m[-1], dta.month[-nrow(dta.month),])
names(dta.month) <- c("e.ret","dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl")

# Quarterly Data Variables

dp.q <- log(gwq[gwq[,1] >= 19464,'D12']) - log(gwq[gwq[,1] >= 19464,'Index']) 
dy.q <- log(gwq[gwq[,1] >= 19464,'D12']) - log(gwq[gwq[,1] >= 19463 & gwq[,1] <= 20133,'Index'])
ep.q <- log(gwq[gwq[,1] >= 19464,'E12']) - log(gwq[gwq[,1] >= 19464,'Index'])
de.q <- log(gwq[gwq[,1] >= 19464,'D12']) - log(gwq[gwq[,1] >= 19464,'E12'])

tms.q <- gwq[gwq[,1] >= 19464,'lty'] - gwq[gwq[,1] >= 19464,'tbl']
dfy.q <- gwq[gwq[,1] >= 19464,'BAA'] - gwq[gwq[,1] >= 19464,'AAA']
dfr.q <- gwq[gwq[,1] >= 19464,'corpr'] - gwq[gwq[,1] >= 19464,'ltr']

svar.q <- gwq[gwq[,1] >= 19464,'svar']
bm.q <- gwq[gwq[,1] >= 19464,'b.m']
ntis.q <- gwq[gwq[,1] >= 19464,'ntis']
tbl.q <- gwq[gwq[,1] >= 19464,'tbl']
lty.q <- gwq[gwq[,1] >= 19464,'lty']
ltr.q <- gwq[gwq[,1] >= 19464,'ltr']
infl.q <- gwq[gwq[,1] >= 19464,'infl']
ik.q <- gwq[gwq[,1] >= 19464,'ik']

dta.quarter <- as.data.frame(cbind(dp.q, dy.q, ep.q, de.q, tms.q, dfy.q, dfr.q, svar.q, bm.q, ntis.q, tbl.q, lty.q, ltr.q, infl.q, ik.q))
dta.quarter <-  cbind(e.ret.q[-1], dta.quarter[-nrow(dta.quarter),])
names(dta.quarter) <- c("e.ret","dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl","ik")

# Annual Data Variables

dp.a <- log(gwa[gwa[,1] >= 1926,'D12']) - log(gwa[gwa[,1] >= 1926,'Index']) 
dy.a <- log(gwa[gwa[,1] >= 1926,'D12']) - log(gwa[gwa[,1] >= 1925 & gwa[,1] <= 2012,'Index'])
ep.a <- log(gwa[gwa[,1] >= 1926,'E12']) - log(gwa[gwa[,1] >= 1926,'Index'])
de.a <- log(gwa[gwa[,1] >= 1926,'D12']) - log(gwa[gwa[,1] >= 1926,'E12'])

tms.a <- gwa[gwa[,1] >= 1926,'lty'] - gwa[gwa[,1] >= 1926,'tbl']
dfy.a <- gwa[gwa[,1] >= 1926,'BAA'] - gwa[gwa[,1] >= 1926,'AAA']
dfr.a <- gwa[gwa[,1] >= 1926,'corpr'] - gwa[gwa[,1] >= 1926,'ltr']

svar.a <- gwa[gwa[,1] >= 1926,'svar']
bm.a <-   gwa[gwa[,1] >= 1926,'b.m']
ntis.a <- gwa[gwa[,1] >= 1926,'ntis']
tbl.a <-  gwa[gwa[,1] >= 1926,'tbl']
lty.a <-  gwa[gwa[,1] >= 1926,'lty']
ltr.a <-  gwa[gwa[,1] >= 1926,'ltr']
infl.a <- gwa[gwa[,1] >= 1926,'infl']
ik.a <-   gwa[gwa[,1] >= 1926,'ik']
eqis.a <- gwa[gwa[,1] >= 1926,'eqis']

dta.annual <- as.data.frame(cbind(dp.a, dy.a, ep.a, de.a, tms.a, dfy.a, dfr.a, svar.a, bm.a, ntis.a, tbl.a, lty.a, ltr.a, infl.a, ik.a, eqis.a))
dta.annual <-  cbind(e.ret.a[-1], dta.annual[-nrow(dta.annual),])
names(dta.annual) <- c("e.ret","dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl","ik","eqis")

write.table(dta.annual, "Data/gw_annual_1927_2013.txt", row.names=FALSE, sep="|")
write.table(dta.quarter, "Data/gw_quarter_19471_20134.txt", row.names=FALSE, sep="|")
write.table(dta.month, "Data/gw_month_192701_201312.txt", row.names=FALSE, sep="|")
