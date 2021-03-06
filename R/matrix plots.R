rm(list=ls())

gw.m <- read.table("Data/gw_month_192701_201312.txt", sep="|", header=TRUE)
gw.q <- read.table("Data/gw_quarter_19471_20134.txt", sep="|", header=TRUE)
gw.a <- read.table("Data/gw_annual_1927_2013.txt", sep="|", header=TRUE)

# Correlation Matrix Plot

library("corrplot")

gw.q.1 <- gw.q[,-length(gw.q)]
gw.a.1 <- gw.a[,-c(16,17)]

pdf(file="Graph/monthly_data_corr_mplot.pdf")
corrplot(cor(gw.m), method = "circle", diag = FALSE) # type = "lower" 
dev.off()

pdf(file="Graph/quarterly_data_corr_mplot.pdf")
corrplot(cor(gw.q.1), method = "circle", diag = FALSE)
dev.off()

pdf(file="Graph/annual_data_corr_mplot.pdf")
corrplot(cor(gw.a.1), method = "circle", diag = FALSE)
dev.off()

#### Monthly Data #########################################

pdf(file="Graph/monthly_matrix_plot.pdf", width = 13)

par(mfrow=c(3,5))

with(gw.m, {
  
  plot.ts(ts(e.ret, start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Equity Premium")
  plot.ts(ts(dp,    start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Log Dividend-Price Ratio")
  plot.ts(ts(dy,    start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Log Dividend Yield")
  plot.ts(ts(ep,    start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Log Earnings-Price Ratio")
  plot.ts(ts(de,    start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Log Dividend-Payout Ratio")
  plot.ts(ts(tms,   start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Term Spread")
  plot.ts(ts(dfy,   start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Default Yield Spread")
  plot.ts(ts(dfr,   start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Default Return Spread")
  plot.ts(ts(svar,  start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Stock Market Variance")
  plot.ts(ts(bm,    start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Book to Market Ratio")
  plot.ts(ts(ntis,  start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Net Equity Expansion")
  plot.ts(ts(tbl,   start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Treasury Bill Rate")
  plot.ts(ts(lty,   start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Long Term Yield")
  plot.ts(ts(ltr,   start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Long Term Return")
  plot.ts(ts(infl,  start=c(1927,1), freq=12), col=4, xlab="", ylab="", main="Inflation")
  
  }
)
dev.off()

#### Quarterly Data #########################################

pdf(file="Graph/quarterly_matrix_plot.pdf", width = 13)

par(mfrow=c(3,5))

with(gw.q, {
  
  plot.ts(ts(e.ret, start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Equity Premium")
  plot.ts(ts(dp,    start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Log Dividend-Price Ratio")
  plot.ts(ts(dy,    start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Log Dividend Yield")
  plot.ts(ts(ep,    start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Log Earnings-Price Ratio")
  plot.ts(ts(de,    start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Log Dividend-Payout Ratio")
  plot.ts(ts(tms,   start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Term Spread")
  plot.ts(ts(dfy,   start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Default Yield Spread")
  plot.ts(ts(dfr,   start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Default Return Spread")
  plot.ts(ts(svar,  start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Stock Market Variance")
  plot.ts(ts(bm,    start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Book to Market Ratio")
  plot.ts(ts(ntis,  start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Net Equity Expansion")
  plot.ts(ts(tbl,   start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Treasury Bill Rate")
  plot.ts(ts(lty,   start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Long Term Yield")
  plot.ts(ts(ltr,   start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Long Term Return")
  plot.ts(ts(infl,  start=c(1947,1), freq=4), col=4, xlab="", ylab="", main="Inflation")
  
  }
)
dev.off()


#### Annual Data #########################################

pdf(file="Graph/annual_matrix_plot.pdf", width = 13)

par(mfrow=c(3,5))

with(gw.a, {
  
  plot.ts(ts(e.ret, start=c(1927), freq=1), col=4, xlab="", ylab="", main="Equity Premium")
  plot.ts(ts(dp,    start=c(1927), freq=1), col=4, xlab="", ylab="", main="Log Dividend-Price Ratio")
  plot.ts(ts(dy,    start=c(1927), freq=1), col=4, xlab="", ylab="", main="Log Dividend Yield")
  plot.ts(ts(ep,    start=c(1927), freq=1), col=4, xlab="", ylab="", main="Log Earnings-Price Ratio")
  plot.ts(ts(de,    start=c(1927), freq=1), col=4, xlab="", ylab="", main="Log Dividend-Payout Ratio")
  plot.ts(ts(tms,   start=c(1927), freq=1), col=4, xlab="", ylab="", main="Term Spread")
  plot.ts(ts(dfy,   start=c(1927), freq=1), col=4, xlab="", ylab="", main="Default Yield Spread")
  plot.ts(ts(dfr,   start=c(1927), freq=1), col=4, xlab="", ylab="", main="Default Return Spread")
  plot.ts(ts(svar,  start=c(1927), freq=1), col=4, xlab="", ylab="", main="Stock Market Variance")
  plot.ts(ts(bm,    start=c(1927), freq=1), col=4, xlab="", ylab="", main="Book to Market Ratio")
  plot.ts(ts(ntis,  start=c(1927), freq=1), col=4, xlab="", ylab="", main="Net Equity Expansion")
  plot.ts(ts(tbl,   start=c(1927), freq=1), col=4, xlab="", ylab="", main="Treasury Bill Rate")
  plot.ts(ts(lty,   start=c(1927), freq=1), col=4, xlab="", ylab="", main="Long Term Yield")
  plot.ts(ts(ltr,   start=c(1927), freq=1), col=4, xlab="", ylab="", main="Long Term Return")
  plot.ts(ts(infl,  start=c(1927), freq=1), col=4, xlab="", ylab="", main="Inflation")
  
  }
)
dev.off()
