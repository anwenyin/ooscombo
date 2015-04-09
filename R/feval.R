feval<-function(y,X,P,theta=1,Window=c("recursive","rolling")) {
  
  if(missing(Window)) Window <- "recursive"
  if(missing(P)) P <- 0.2 * length(y)
  if(P >= length(y)) stop("Prediction sample larger than total sample size!")
  Window<-match.arg(Window)
  
  if(is.null(X)){
    
    y <- as.matrix(y)
    P <- as.numeric(P)
    theta<-as.numeric(theta)
    
    T <- length(y)
    R <- T - P
    k <- 0
    pbar <- pbar.val(k = k)
    EsupW <- 2*pbar - k - 1
    
    yr <- y[1:R]
    yp <- y[(R+1):T]
    
    smodel <- lm(yr~1)
    beta.f <- coef(smodel) # stable model coefficient
    
    tau1 <- bdate(bound=0.15,Y=yr,X=0)$break.fraction
    
    F <- numeric(P)
    wcv <- numeric(P)
    wgcv <- numeric(P)
    wb <- numeric(P)
    wsw <- numeric(P)
    beta.cv <- matrix(0,P,k+1)
    beta.gcv <- matrix(0,P,k+1)
    beta.bic <- matrix(0,P,k+1)
    beta.sw <- matrix(0,P,k+1)
    bmodel.oos <- goos(y=y,X=NULL,P=P,Window=Window,Break=TRUE,tau=tau1)
    smodel.oos <- goos(y=y,X=NULL,P=P,Window=Window)
    
    if (Window=="recursive") {
      
      for (i in 1:P) {
        
        F[i] <- ((R+i-1)-2*(k+1))*(smodel.oos$SSE[i] - bmodel.oos$SSE[i])/bmodel.oos$SSE[i]
        
        wcv[i] <- ifelse(F[i] >= pbar, 1-pbar/F[i], 0)
        
        wgcv[i] <- 1-(1/2)*(smodel.oos$LAMBDA[i] + EsupW)/(smodel.oos$SSE[i] - bmodel.oos$SSE[i]) #
        if (wgcv[i] < 0) {wgcv[i] <- 0}
        
        wb[i] <- exp(log(bmodel.oos$SSE[i]/((R+i-1)-2*(k+1)))+2*(k+1)*log(R+i-1)/(R+i-1))/(exp(log(bmodel.oos$SSE[i]/((R+i-1)-2*(k+1)))+2*(k+1)*log(R+i-1)/(R+i-1)) + 
                                                                                             exp(log(smodel.oos$SSE[i]/(R+i-1-k-1))+(k+1)*log(R+i-1)/(R+i-1)))
        wsw[i] <- (sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1)/((sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1) + (sum(theta^(rev(1:i))*smodel.oos$SFE[1:i]))^(-1))
        
        beta.cv[i] <- wcv[i]*bmodel.oos$Beta[i] + (1 - wcv[i])*smodel.oos$Beta[i]
        beta.gcv[i] <- wgcv[i]*bmodel.oos$Beta[i] + (1 - wgcv[i])*smodel.oos$Beta[i]
        beta.bic[i] <- wb[i]*bmodel.oos$Beta[i] + (1 - wb[i])*smodel.oos$Beta[i]
        beta.sw[i] <- wsw[i]*bmodel.oos$Beta[i] + (1 - wsw[i])*smodel.oos$Beta[i]
      }  
      
      all.weight <- cbind(wcv,wgcv,wb,wsw)
      colnames(all.weight) <- c("Cp","CV","SIC","S-W")
      
      y.cv <- beta.cv
      y.gcv <- beta.gcv
      y.stable <- smodel.oos$Beta # stable model
      y.break <- bmodel.oos$Beta # break model
      y.equal <- 0.5*bmodel.oos$Beta + 0.5*smodel.oos$Beta # equal-weight averaging
      y.bic <- beta.bic # Bayesian model averaging
      y.sw <- beta.sw # Stock-Watson weighting with discount factor 1
      
      forecasts <- cbind(y.cv,y.gcv,y.bic,y.sw,y.equal,y.break,y.stable)
      colnames(forecasts) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
      
      SFE.cv <- (yp - y.cv)^2
      SFE.gcv <- (yp - y.gcv)^2
      SFE.stable <- (yp - y.stable)^2
      SFE.break <- (yp - y.break)^2
      SFE.equal <- (yp - y.equal)^2
      SFE.bic <- (yp - y.bic)^2
      SFE.sw <- (yp - y.sw)^2
      
      sfe <- cbind(SFE.cv,SFE.gcv,SFE.bic,SFE.sw,SFE.equal,SFE.break,SFE.stable)
      colnames(sfe) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
      
      RMSFE.cv <- round(sqrt(sum((yp - y.cv)^2)/P),3)
      RMSFE.gcv <- round(sqrt(sum((yp - y.gcv)^2)/P),3)
      RMSFE.stable <- round(sqrt(sum((yp - y.stable)^2)/P),3)
      RMSFE.break <- round(sqrt(sum((yp - y.break)^2)/P),3)
      RMSFE.equal <- round(sqrt(sum((yp - y.equal)^2)/P),3)
      RMSFE.bic <- round(sqrt(sum((yp - y.bic)^2)/P),3)
      RMSFE.sw <- round(sqrt(sum((yp - y.sw)^2)/P),3)
      
      mat <- round(matrix(c(RMSFE.cv,RMSFE.gcv,RMSFE.bic,RMSFE.sw,RMSFE.equal,RMSFE.stable,RMSFE.break,
                            RMSFE.cv/RMSFE.equal, RMSFE.gcv/RMSFE.equal,RMSFE.bic/RMSFE.equal,RMSFE.sw/RMSFE.equal,RMSFE.equal/RMSFE.equal,RMSFE.stable/RMSFE.equal,RMSFE.break/RMSFE.equal),c(2,7),byrow=T),3)
      colnames(mat) <- c("Cp","CV","SIC","S-W","Equal","Stable","Break")
      rownames(mat) <- c("RMSFE","Ratio")
      group <- list(mat, all.weight,forecasts,sfe)
      names(group) <- c("mat","weight","forecast","sfe")
      return(invisible(group))
      # mat<-xtable(mat)
      # print(mat, sanitize.text.function = function(x){x})
    }
    
    if (Window=="rolling") {
      
      for (i in 1:P) {
        
        F[i] <- (R-2*(k+1))*(smodel.oos$SSE[i] - bmodel.oos$SSE[i])/bmodel.oos$SSE[i]
        
        wcv[i] <- ifelse(F[i] >= pbar, 1-pbar/F[i], 0)
        
        wgcv[i] <- 1-(1/2)*(smodel.oos$LAMBDA[i] + EsupW)/(smodel.oos$SSE[i] - bmodel.oos$SSE[i]) #
        if (wgcv[i] < 0) {wgcv[i] <- 0}
        wb[i] <- exp(log(bmodel.oos$SSE[i]/(R-2*(k+1)))+2*(k+1)*log(R)/R)/(exp(log(bmodel.oos$SSE[i]/(R-2*(k+1)))+2*(k+1)*log(R)/R)+exp(log(smodel.oos$SSE[i]/(R-k-1))+(k+1)*log(R)/R))																						   
        wsw[i] <- (sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1)/((sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1) + (sum(theta^(rev(1:i))*smodel.oos$SFE[1:i]))^(-1))
        
        beta.cv[i] <- wcv[i]*bmodel.oos$Beta[i] + (1 - wcv[i])*smodel.oos$Beta[i]
        beta.gcv[i] <- wgcv[i]*bmodel.oos$Beta[i] + (1 - wgcv[i])*smodel.oos$Beta[i]
        beta.bic[i] <- wb[i]*bmodel.oos$Beta[i] + (1 - wb[i])*smodel.oos$Beta[i]
        beta.sw[i] <- wsw[i]*bmodel.oos$Beta[i] + (1 - wsw[i])*smodel.oos$Beta[i]
      }
      
      all.weight <- cbind(wcv,wgcv,wb,wsw)
      colnames(all.weight) <- c("CV","GCV","Bayesian","StockWatson")
      
      y.cv <- beta.cv
      y.gcv <- beta.gcv
      y.stable <- smodel.oos$Beta # stable model
      y.break <- bmodel.oos$Beta # break model
      y.equal <- 0.5*bmodel.oos$Beta + 0.5*smodel.oos$Beta # equal-weight averaging
      y.bic <- beta.bic # Bayesian model averaging
      y.sw <- beta.sw # Stock-Watson weighting with discount factor 1
      
      forecasts <- cbind(y.cv,y.gcv,y.bic,y.sw,y.equal,y.break,y.stable)
      colnames(forecasts) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
      
      SFE.cv <- (yp - y.cv)^2
      SFE.gcv <- (yp - y.gcv)^2
      SFE.stable <- (yp - y.stable)^2
      SFE.break <- (yp - y.break)^2
      SFE.equal <- (yp - y.equal)^2
      SFE.bic <- (yp - y.bic)^2
      SFE.sw <- (yp - y.sw)^2
      
      sfe <- cbind(SFE.cv,SFE.gcv,SFE.bic,SFE.sw,SFE.equal,SFE.break,SFE.stable)
      colnames(sfe) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
      
      RMSFE.cv <-round(sqrt(sum((yp-y.cv)^2)/P),3)
      RMSFE.gcv <- round(sqrt(sum((yp-y.gcv)^2)/P),3)
      RMSFE.stable <- round(sqrt(sum((yp-y.stable)^2)/P),3)
      RMSFE.break <- round(sqrt(sum((yp-y.break)^2)/P),3)
      RMSFE.equal <- round(sqrt(sum((yp-y.equal)^2)/P),3)
      RMSFE.bic <- round(sqrt(sum((yp-y.bic)^2)/P),3)
      RMSFE.sw <- round(sqrt(sum((yp - y.sw)^2)/P),3)
      
      mat <- round(matrix(c(RMSFE.cv,RMSFE.gcv,RMSFE.bic,RMSFE.sw,RMSFE.equal,RMSFE.stable,RMSFE.break,
                            RMSFE.cv/RMSFE.equal, RMSFE.gcv/RMSFE.equal,RMSFE.bic/RMSFE.equal,RMSFE.sw/RMSFE.equal,RMSFE.equal/RMSFE.equal,RMSFE.stable/RMSFE.equal,RMSFE.break/RMSFE.equal),c(2,7),byrow=T),3)
      colnames(mat) <- c("Cp","CV","SIC","S-W","Equal","Stable","Break")
      rownames(mat) <- c("RMSFE","Ratio")
      group <- list(mat, all.weight,forecasts,sfe)
      names(group) <- c("mat","weight","forecast","sfe")
      return(invisible(group))
      # mat<-xtable(mat)
      # print(mat, sanitize.text.function = function(x){x})
    }	
  }
  
  y <- as.matrix(y)
  X <- as.matrix(X)
  P <- as.numeric(P)
  theta<-as.numeric(theta)
  
  T <- length(y)
  k <- ncol(X)
  x1 <- X
  X <- cbind(rep(1,T),X)
  num <- k + 1
  R <- T - P
  pbar <- pbar.val(k = k)
  EsupW <- 2*pbar - k - 1
  
  yr <- y[1:R]
  yp <- y[(R+1):T]
  
  Xr <- X[1:R,]
  Xp <- X[(R+1):T,]
  
  x1.r <- X[1:R,2:num]
  x1.p <- X[(R+1):T,2:num]
  
  smodel <- lm(yr~x1.r)
  beta.f <- coef(smodel) # stable model coefficient
  
  tau1 <- bdate(bound=0.15,Y=yr,X=x1.r)$break.fraction
  
  F <- numeric(P)
  wcv <- numeric(P)
  wgcv <- numeric(P)
  wb <- numeric(P)
  wsw <- numeric(P)
  beta.cv <- matrix(0,P,k+1)
  beta.gcv <- matrix(0,P,k+1)
  beta.bic <- matrix(0,P,k+1)
  beta.sw <- matrix(0,P,k+1)
  bmodel.oos <- goos(y=y,X=x1,P=P,Window=Window,Break=TRUE,tau=tau1)
  smodel.oos <- goos(y=y,X=x1,P=P,Window=Window)
  
  if (Window=="recursive") {
    
    for (i in 1:P) {
      
      F[i] <- ((R+i-1)-2*(k+1))*(smodel.oos$SSE[i] - bmodel.oos$SSE[i])/bmodel.oos$SSE[i]
      
      wcv[i] <- ifelse(F[i] >= pbar, 1-pbar/F[i], 0)
      
      wgcv[i] <- 1-(1/2)*(smodel.oos$LAMBDA[i] + EsupW)/(smodel.oos$SSE[i] - bmodel.oos$SSE[i]) #
      if (wgcv[i] < 0) {wgcv[i] <- 0}
      
      wb[i] <- exp(log(bmodel.oos$SSE[i]/((R+i-1)-2*(k+1)))+2*(k+1)*log(R+i-1)/(R+i-1))/(exp(log(bmodel.oos$SSE[i]/((R+i-1)-2*(k+1)))+2*(k+1)*log(R+i-1)/(R+i-1)) + 
                                                                                           exp(log(smodel.oos$SSE[i]/(R+i-1-k-1))+(k+1)*log(R+i-1)/(R+i-1)))
      wsw[i] <- (sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1)/((sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1) + (sum(theta^(rev(1:i))*smodel.oos$SFE[1:i]))^(-1))
      
      beta.cv[i,] <- wcv[i]*bmodel.oos$Beta[i,] + (1 - wcv[i])*smodel.oos$Beta[i,]
      beta.gcv[i,] <- wgcv[i]*bmodel.oos$Beta[i,] + (1 - wgcv[i])*smodel.oos$Beta[i,]
      beta.bic[i,] <- wb[i]*bmodel.oos$Beta[i,] + (1 - wb[i])*smodel.oos$Beta[i,]
      beta.sw[i,] <- wsw[i]*bmodel.oos$Beta[i,] + (1 - wsw[i])*smodel.oos$Beta[i,]
    }	
    
    all.weight <- cbind(wcv,wgcv,wb,wsw)
    colnames(all.weight) <- c("Cp","CV","SIC","S-W")
    
    y.cv <- rowSums(Xp*beta.cv)
    y.gcv <- rowSums(Xp*beta.gcv)
    y.stable <- rowSums(Xp*smodel.oos$Beta) # stable model
    y.break <- rowSums(Xp*bmodel.oos$Beta) # break model
    y.equal <- rowSums(Xp*(0.5*bmodel.oos$Beta + 0.5*smodel.oos$Beta)) # equal-weight averaging
    y.bic <- rowSums(Xp*beta.bic) # Bayesian model averaging
    y.sw <- rowSums(Xp*beta.sw) # Stock-Watson weighting with discount factor 1
    
    forecasts <- cbind(y.cv,y.gcv,y.bic,y.sw,y.equal,y.break,y.stable)
    colnames(forecasts) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
    
    SFE.cv <- (yp - y.cv)^2
    SFE.gcv <- (yp - y.gcv)^2
    SFE.stable <- (yp - y.stable)^2
    SFE.break <- (yp - y.break)^2
    SFE.equal <- (yp - y.equal)^2
    SFE.bic <- (yp - y.bic)^2
    SFE.sw <- (yp - y.sw)^2
    
    sfe <- cbind(SFE.cv,SFE.gcv,SFE.bic,SFE.sw,SFE.equal,SFE.break,SFE.stable)
    colnames(sfe) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
    
    RMSFE.cv <- round(sqrt(sum((yp - y.cv)^2)/P),3)
    RMSFE.gcv <- round(sqrt(sum((yp - y.gcv)^2)/P),3)
    RMSFE.stable <- round(sqrt(sum((yp - y.stable)^2)/P),3)
    RMSFE.break <- round(sqrt(sum((yp - y.break)^2)/P),3)
    RMSFE.equal <- round(sqrt(sum((yp - y.equal)^2)/P),3)
    RMSFE.bic <- round(sqrt(sum((yp - y.bic)^2)/P),3)
    RMSFE.sw <- round(sqrt(sum((yp - y.sw)^2)/P),3)
    
    mat <- round(matrix(c(RMSFE.cv,RMSFE.gcv,RMSFE.bic,RMSFE.sw,RMSFE.equal,RMSFE.stable,RMSFE.break,
                          RMSFE.cv/RMSFE.equal, RMSFE.gcv/RMSFE.equal,RMSFE.bic/RMSFE.equal,RMSFE.sw/RMSFE.equal,RMSFE.equal/RMSFE.equal,RMSFE.stable/RMSFE.equal,RMSFE.break/RMSFE.equal),c(2,7),byrow=T),3)
    colnames(mat) <- c("Cp","CV","SIC","S-W","Equal","Stable","Break")
    rownames(mat) <- c("RMSFE","Ratio")
    group <- list(mat, all.weight,forecasts,sfe)
    names(group) <- c("mat","weight","forecast","sfe")
    return(invisible(group))
    # mat<-xtable(mat)
    # print(mat, sanitize.text.function = function(x){x})
  }
  
  if (Window=="rolling") {
    
    for (i in 1:P) {
      
      F[i] <- (R-2*(k+1))*(smodel.oos$SSE[i] - bmodel.oos$SSE[i])/bmodel.oos$SSE[i]
      
      wcv[i] <- ifelse(F[i] >= pbar, 1-pbar/F[i], 0)
      
      wgcv[i] <- 1-(1/2)*(smodel.oos$LAMBDA[i] + EsupW)/(smodel.oos$SSE[i] - bmodel.oos$SSE[i]) #
      if (wgcv[i] < 0) {wgcv[i] <- 0}
      wb[i] <- exp(log(bmodel.oos$SSE[i]/(R-2*(k+1)))+2*(k+1)*log(R)/R)/(exp(log(bmodel.oos$SSE[i]/(R-2*(k+1)))+2*(k+1)*log(R)/R)+exp(log(smodel.oos$SSE[i]/(R-k-1))+(k+1)*log(R)/R))																						   
      wsw[i] <- (sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1)/((sum(theta^(rev(1:i))*bmodel.oos$SFE[1:i]))^(-1) + (sum(theta^(rev(1:i))*smodel.oos$SFE[1:i]))^(-1))
      
      beta.cv[i,] <- wcv[i]*bmodel.oos$Beta[i,] + (1 - wcv[i])*smodel.oos$Beta[i,]
      beta.gcv[i,] <- wgcv[i]*bmodel.oos$Beta[i,] + (1 - wgcv[i])*smodel.oos$Beta[i,]
      beta.bic[i,] <- wb[i]*bmodel.oos$Beta[i,] + (1 - wb[i])*smodel.oos$Beta[i,]
      beta.sw[i,] <- wsw[i]*bmodel.oos$Beta[i,] + (1 - wsw[i])*smodel.oos$Beta[i,]
    }
    
    all.weight <- cbind(wcv,wgcv,wb,wsw)
    colnames(all.weight) <- c("CV","GCV","Bayesian","StockWatson")
    
    y.cv <- rowSums(Xp*beta.cv)
    y.gcv <- rowSums(Xp*beta.gcv)
    y.stable <- rowSums(Xp*smodel.oos$Beta) # stable model
    y.break <- rowSums(Xp*bmodel.oos$Beta) # break model
    y.equal <- rowSums(Xp*(0.5*bmodel.oos$Beta + 0.5*smodel.oos$Beta)) # equal-weight averaging
    y.bic <- rowSums(Xp*beta.bic) # Bayesian model averaging
    y.sw <- rowSums(Xp*beta.sw) # Stock-Watson weighting with discount factor 1
    
    forecasts <- cbind(y.cv,y.gcv,y.bic,y.sw,y.equal,y.break,y.stable)
    colnames(forecasts) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
    
    SFE.cv <- (yp - y.cv)^2
    SFE.gcv <- (yp - y.gcv)^2
    SFE.stable <- (yp - y.stable)^2
    SFE.break <- (yp - y.break)^2
    SFE.equal <- (yp - y.equal)^2
    SFE.bic <- (yp - y.bic)^2
    SFE.sw <- (yp - y.sw)^2
    
    sfe <- cbind(SFE.cv,SFE.gcv,SFE.bic,SFE.sw,SFE.equal,SFE.break,SFE.stable)
    colnames(sfe) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
    
    RMSFE.cv <-round(sqrt(sum((yp-y.cv)^2)/P),3)
    RMSFE.gcv <- round(sqrt(sum((yp-y.gcv)^2)/P),3)
    RMSFE.stable <- round(sqrt(sum((yp-y.stable)^2)/P),3)
    RMSFE.break <- round(sqrt(sum((yp-y.break)^2)/P),3)
    RMSFE.equal <- round(sqrt(sum((yp-y.equal)^2)/P),3)
    RMSFE.bic <- round(sqrt(sum((yp-y.bic)^2)/P),3)
    RMSFE.sw <- round(sqrt(sum((yp - y.sw)^2)/P),3)
    
    mat <- round(matrix(c(RMSFE.cv,RMSFE.gcv,RMSFE.bic,RMSFE.sw,RMSFE.equal,RMSFE.stable,RMSFE.break,
                          RMSFE.cv/RMSFE.equal, RMSFE.gcv/RMSFE.equal,RMSFE.bic/RMSFE.equal,RMSFE.sw/RMSFE.equal,RMSFE.equal/RMSFE.equal,RMSFE.stable/RMSFE.equal,RMSFE.break/RMSFE.equal),c(2,7),byrow=T),3)
    colnames(mat) <- c("Cp","CV","SIC","S-W","Equal","Stable","Break")
    rownames(mat) <- c("RMSFE","Ratio")
    group <- list(mat, all.weight,forecasts,sfe)
    names(group) <- c("mat","weight","forecast","sfe")
    return(invisible(group))
    # mat<-xtable(mat)
    # print(mat, sanitize.text.function = function(x){x})
  }
}
