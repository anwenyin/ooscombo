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

