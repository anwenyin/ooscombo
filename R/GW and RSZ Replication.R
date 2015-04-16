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
