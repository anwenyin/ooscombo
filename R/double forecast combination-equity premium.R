par(mfrow=c(2,2))
P<-60

method <- 'CV'
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

CT.CV <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])) # CV is better than Cp      


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

CT.Cp <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])) # CV is better than Cp      



method <- 'S-W'
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

CT.SW <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])) # CV is better than Cp      



method <- 'Equal'
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

CT.Equal <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])) # CV is better than Cp      


method <- 'Stable'
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

CT.Stable <- 100*(1 - sum(sfe.combo)/sum(feval(y=e.ret,X=0,P=P)$sfe[,'Stable'])) # CV is better than Cp      

result<-c(CT.CV,CT.Cp,CT.SW,CT.Equal,CT.Stable)
names(result)<-c('CV','Cp','S-W','Equal','Stable')
print(result)
