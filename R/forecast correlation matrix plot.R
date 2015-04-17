rm(list=ls())
library(sandwich)
library(corrplot)
source('R/goos.r')
source('R/feval.r')
source('R/supfun.r')

gw.m <- read.table("Data/gw_month_192701_201312.txt", sep="|", header=TRUE)
gw.q <- read.table("Data/gw_quarter_19471_20134.txt", sep="|", header=TRUE)
gw.a <- read.table("Data/gw_annual_1927_2013.txt", sep="|", header=TRUE)

P.m <- 684 # OOS starts from 1957:01 to 2013:12
P.q <- 196 # OOS starts from 1965:1 to 2013:4 # set to 152 # OOS starts from 1976:1 to 2013:4
P.a <- 49  # OOS starts from 1965 to 2013

#### Monthly 

forecast.month <- matrix(0, nrow = P.m, ncol = 14)
forecast.error.month <- matrix(0, nrow = P.m, ncol = 14)
colnames(forecast.month) <- c("dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl")
colnames(forecast.error.month) <- c("dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl")

with(gw.m, {
  
  forecast.month[,1]  <<- goos(y=e.ret, X = dp,   P = P.m)$Forecast
  forecast.month[,2]  <<- goos(y=e.ret, X = dy,   P = P.m)$Forecast
  forecast.month[,3]  <<- goos(y=e.ret, X = ep,   P = P.m)$Forecast
  forecast.month[,4]  <<- goos(y=e.ret, X = de,   P = P.m)$Forecast
  forecast.month[,5]  <<- goos(y=e.ret, X = tms,  P = P.m)$Forecast
  forecast.month[,6]  <<- goos(y=e.ret, X = dfy,  P = P.m)$Forecast
  forecast.month[,7]  <<- goos(y=e.ret, X = dfr,  P = P.m)$Forecast
  forecast.month[,8]  <<- goos(y=e.ret, X = svar, P = P.m)$Forecast
  forecast.month[,9]  <<- goos(y=e.ret, X = bm,   P = P.m)$Forecast
  forecast.month[,10] <<- goos(y=e.ret, X = ntis, P = P.m)$Forecast
  forecast.month[,11] <<- goos(y=e.ret, X = tbl,  P = P.m)$Forecast
  forecast.month[,12] <<- goos(y=e.ret, X = lty,  P = P.m)$Forecast
  forecast.month[,13] <<- goos(y=e.ret, X = ltr,  P = P.m)$Forecast
  forecast.month[,14] <<- goos(y=e.ret, X = infl, P = P.m)$Forecast
  
  forecast.error.month[,1]  <<- goos(y=e.ret, X = dp,   P = P.m)$Error
  forecast.error.month[,2]  <<- goos(y=e.ret, X = dy,   P = P.m)$Error
  forecast.error.month[,3]  <<- goos(y=e.ret, X = ep,   P = P.m)$Error
  forecast.error.month[,4]  <<- goos(y=e.ret, X = de,   P = P.m)$Error
  forecast.error.month[,5]  <<- goos(y=e.ret, X = tms,  P = P.m)$Error
  forecast.error.month[,6]  <<- goos(y=e.ret, X = dfy,  P = P.m)$Error
  forecast.error.month[,7]  <<- goos(y=e.ret, X = dfr,  P = P.m)$Error
  forecast.error.month[,8]  <<- goos(y=e.ret, X = svar, P = P.m)$Error
  forecast.error.month[,9]  <<- goos(y=e.ret, X = bm,   P = P.m)$Error
  forecast.error.month[,10] <<- goos(y=e.ret, X = ntis, P = P.m)$Error
  forecast.error.month[,11] <<- goos(y=e.ret, X = tbl,  P = P.m)$Error
  forecast.error.month[,12] <<- goos(y=e.ret, X = lty,  P = P.m)$Error
  forecast.error.month[,13] <<- goos(y=e.ret, X = ltr,  P = P.m)$Error
  forecast.error.month[,14] <<- goos(y=e.ret, X = infl, P = P.m)$Error
  
}
)

pdf(file="Graph/monthly_oos_forecast_mplot.pdf")
corrplot(cor(forecast.month), method = "circle", diag = FALSE)
dev.off()

pdf(file="Graph/monthly_oos_forecast_error_mplot.pdf.pdf")
corrplot(cor(forecast.error.month), method = "circle", diag = FALSE)
dev.off()

######## quarterly

forecast.quarter <- matrix(0, nrow = P.q, ncol = 14)
forecast.error.quarter <- matrix(0, nrow = P.q, ncol = 14)
colnames(forecast.quarter) <- c("dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl")
colnames(forecast.error.quarter) <- c("dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl")

with(gw.q, {
  
  forecast.quarter[,1]  <<- goos(y=e.ret, X = dp,   P = P.q)$Forecast
  forecast.quarter[,2]  <<- goos(y=e.ret, X = dy,   P = P.q)$Forecast
  forecast.quarter[,3]  <<- goos(y=e.ret, X = ep,   P = P.q)$Forecast
  forecast.quarter[,4]  <<- goos(y=e.ret, X = de,   P = P.q)$Forecast
  forecast.quarter[,5]  <<- goos(y=e.ret, X = tms,  P = P.q)$Forecast
  forecast.quarter[,6]  <<- goos(y=e.ret, X = dfy,  P = P.q)$Forecast
  forecast.quarter[,7]  <<- goos(y=e.ret, X = dfr,  P = P.q)$Forecast
  forecast.quarter[,8]  <<- goos(y=e.ret, X = svar, P = P.q)$Forecast
  forecast.quarter[,9]  <<- goos(y=e.ret, X = bm,   P = P.q)$Forecast
  forecast.quarter[,10] <<- goos(y=e.ret, X = ntis, P = P.q)$Forecast
  forecast.quarter[,11] <<- goos(y=e.ret, X = tbl,  P = P.q)$Forecast
  forecast.quarter[,12] <<- goos(y=e.ret, X = lty,  P = P.q)$Forecast
  forecast.quarter[,13] <<- goos(y=e.ret, X = ltr,  P = P.q)$Forecast
  forecast.quarter[,14] <<- goos(y=e.ret, X = infl, P = P.q)$Forecast
  
  forecast.error.quarter[,1]  <<- goos(y=e.ret, X = dp,   P = P.q)$Error
  forecast.error.quarter[,2]  <<- goos(y=e.ret, X = dy,   P = P.q)$Error
  forecast.error.quarter[,3]  <<- goos(y=e.ret, X = ep,   P = P.q)$Error
  forecast.error.quarter[,4]  <<- goos(y=e.ret, X = de,   P = P.q)$Error
  forecast.error.quarter[,5]  <<- goos(y=e.ret, X = tms,  P = P.q)$Error
  forecast.error.quarter[,6]  <<- goos(y=e.ret, X = dfy,  P = P.q)$Error
  forecast.error.quarter[,7]  <<- goos(y=e.ret, X = dfr,  P = P.q)$Error
  forecast.error.quarter[,8]  <<- goos(y=e.ret, X = svar, P = P.q)$Error
  forecast.error.quarter[,9]  <<- goos(y=e.ret, X = bm,   P = P.q)$Error
  forecast.error.quarter[,10] <<- goos(y=e.ret, X = ntis, P = P.q)$Error
  forecast.error.quarter[,11] <<- goos(y=e.ret, X = tbl,  P = P.q)$Error
  forecast.error.quarter[,12] <<- goos(y=e.ret, X = lty,  P = P.q)$Error
  forecast.error.quarter[,13] <<- goos(y=e.ret, X = ltr,  P = P.q)$Error
  forecast.error.quarter[,14] <<- goos(y=e.ret, X = infl, P = P.q)$Error
  
}
)

pdf(file="Graph/quarterly_oos_forecast_mplot.pdf")
corrplot(cor(forecast.quarter), method = "circle", diag = FALSE)
dev.off()

pdf(file="Graph/quarterly_oos_forecast_error_mplot.pdf.pdf")
corrplot(cor(forecast.error.quarter), method = "circle", diag = FALSE)
dev.off()

######## annual

forecast.year <- matrix(0, nrow = P.a, ncol = 14)
forecast.error.year <- matrix(0, nrow = P.a, ncol = 14)
colnames(forecast.year) <- c("dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl")
colnames(forecast.error.year) <- c("dp","dy","ep","de","tms","dfy","dfr","svar","bm","ntis","tbl","lty","ltr","infl")

with(gw.a, {
  
  forecast.year[,1]  <<- goos(y=e.ret, X = dp,   P = P.a)$Forecast
  forecast.year[,2]  <<- goos(y=e.ret, X = dy,   P = P.a)$Forecast
  forecast.year[,3]  <<- goos(y=e.ret, X = ep,   P = P.a)$Forecast
  forecast.year[,4]  <<- goos(y=e.ret, X = de,   P = P.a)$Forecast
  forecast.year[,5]  <<- goos(y=e.ret, X = tms,  P = P.a)$Forecast
  forecast.year[,6]  <<- goos(y=e.ret, X = dfy,  P = P.a)$Forecast
  forecast.year[,7]  <<- goos(y=e.ret, X = dfr,  P = P.a)$Forecast
  forecast.year[,8]  <<- goos(y=e.ret, X = svar, P = P.a)$Forecast
  forecast.year[,9]  <<- goos(y=e.ret, X = bm,   P = P.a)$Forecast
  forecast.year[,10] <<- goos(y=e.ret, X = ntis, P = P.a)$Forecast
  forecast.year[,11] <<- goos(y=e.ret, X = tbl,  P = P.a)$Forecast
  forecast.year[,12] <<- goos(y=e.ret, X = lty,  P = P.a)$Forecast
  forecast.year[,13] <<- goos(y=e.ret, X = ltr,  P = P.a)$Forecast
  forecast.year[,14] <<- goos(y=e.ret, X = infl, P = P.a)$Forecast
  
  forecast.error.year[,1]  <<- goos(y=e.ret, X = dp,   P = P.a)$Error
  forecast.error.year[,2]  <<- goos(y=e.ret, X = dy,   P = P.a)$Error
  forecast.error.year[,3]  <<- goos(y=e.ret, X = ep,   P = P.a)$Error
  forecast.error.year[,4]  <<- goos(y=e.ret, X = de,   P = P.a)$Error
  forecast.error.year[,5]  <<- goos(y=e.ret, X = tms,  P = P.a)$Error
  forecast.error.year[,6]  <<- goos(y=e.ret, X = dfy,  P = P.a)$Error
  forecast.error.year[,7]  <<- goos(y=e.ret, X = dfr,  P = P.a)$Error
  forecast.error.year[,8]  <<- goos(y=e.ret, X = svar, P = P.a)$Error
  forecast.error.year[,9]  <<- goos(y=e.ret, X = bm,   P = P.a)$Error
  forecast.error.year[,10] <<- goos(y=e.ret, X = ntis, P = P.a)$Error
  forecast.error.year[,11] <<- goos(y=e.ret, X = tbl,  P = P.a)$Error
  forecast.error.year[,12] <<- goos(y=e.ret, X = lty,  P = P.a)$Error
  forecast.error.year[,13] <<- goos(y=e.ret, X = ltr,  P = P.a)$Error
  forecast.error.year[,14] <<- goos(y=e.ret, X = infl, P = P.a)$Error
  
}
)

pdf(file="Graph/annual_oos_forecast_mplot.pdf")
corrplot(cor(forecast.year), method = "circle", diag = FALSE)
dev.off()

pdf(file="Graph/annual_oos_forecast_error_mplot.pdf.pdf")
corrplot(cor(forecast.error.year), method = "circle", diag = FALSE)
dev.off()
