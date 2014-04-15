pbar.val <- function(k,pi1=0.15){
  k <- as.numeric(k)
  if (k > 19) stop("k must be less than 20!")
  m <- c(2.49,4.05,5.47,6.8,8.09,9.36,10.6,11.8,13,14.2,15.4,16.6,17.8,18.9,20,21.2,22.4,23.5,24.6,25.8)
  return(m[k+1])
}

sse <- function(tau,Y,X,bound = 0.15){
  if(tau > 1 - bound || tau < bound) stop("The break fraction tau must fall into the bounded interval")
  n <- length(Y)
  tau <- floor(tau*n)
  Y1 <- Y[1:tau]
  Y2 <- Y[(tau+1):n]
  X <- cbind(rep(1,n),X)
  num <- ncol(X)
  reg1 <- X[1:tau,2:num]
  reg2 <- X[(tau + 1):n,2:num]
  X1 <- X[1:tau,]
  X2 <- X[(tau + 1):n,]
  pre <- lm(Y1~reg1)
  post <- lm(Y2~reg2)
  beta1 <- pre$coef
  beta2 <- post$coef
  sse.b <- sum((Y1-X1%*%beta1)^2) + sum((Y2 - X2%*%beta2)^2)
  return(sse.b)
}

#EXAMPLE:
#Y<-rnorm(100); X<-matrix(rnorm(300),100,3); sse(0.2,Y,X); sse(0.1,Y,X)

bdate <- function(bound = 0.15,Y,X){
  if(bound > 1 || bound < 0) stop("The bound parameter must fall into the interval[0, 1]!")
  result <- optimise(sse,c(bound,1 - bound),Y=Y,X=X,bound = bound)
  break.fraction <- result$min
  break.date <- floor(break.fraction * length(Y))
  return(list(break.fraction = break.fraction, break.date = break.date))
}

#EXAMPLE:
#Y<-rnorm(100); X<-matrix(rnorm(300),100,3);bdate(0.2,Y=Y,X=X)

dgp<-function(T,k,sigma=1,Break=FALSE,hetero=FALSE, tau=0.3,delta=1.2, P, mis=FALSE, fig=FALSE) {
  if(missing(P)) P <- floor(0.25 * T)
  
  if (Break==FALSE && hetero==FALSE && mis==FALSE){
    Beta <- matrix(sample(1:(2*k+1),k+1,rep=T),k+1,1)
    e <- rnorm(T,0,sigma^2)
    y <- numeric(T)
    ones <- rep(1,T)
    x1 <- matrix(rnorm(T*k,k,sigma^2),T,k)
    X <- cbind(ones,x1)
    
    for (i in 1:T) {
      y[i]<-X[i,]%*%Beta + e[i]
    }
    if (fig == TRUE) plot.ts(y,col=4,xlab="Time")
    group<-list(y,x1,Beta)
    names(group)<-c("Y","X","Beta")
    return(invisible(group))
  }
  
  if (Break==TRUE && hetero==FALSE && mis==FALSE){
    R <- T - P
    Beta <- matrix(sample(1:(2*k+1),k+1,rep=T),k+1,1)
    Beta.post <- Beta * delta
    e <- rnorm(T,0,sigma^2)
    y <- numeric(T) 
    ones <- rep(1,T)
    x1 <- matrix(rnorm(T*k,k,sigma^2),T,k)
    X <- cbind(ones,x1)
    t0 <- floor(tau*R)
    
    for (i in 1:t0) {
      y[i]<-X[i,]%*%Beta + e[i]
    }
    for (i in (t0+1):T) {
      y[i]<-X[i,]%*%(delta*Beta) + e[i]
    }
    
    if (fig == TRUE) plot.ts(y,col=4,xlab="Time")
    group<-list(y,x1,t0,Beta,Beta.post)
    names(group)<-c("Y","X","Break.date","Beta.pre","Beta.post")
    return(invisible(group))
  }
  
  if (Break==FALSE && hetero==TRUE && mis==FALSE){
    Beta <- matrix(sample(1:(2*k+1),k+1,rep=T),k+1,1)
    y <- numeric(T)
    ones <- rep(1,T)
    x1 <- matrix(rnorm(T*k,k,sigma^2),T,k)
    e <- numeric(T)
    for (i in 1:T){
      e[i]<-rnorm(1,0,norm(as.matrix(x1[i,]),'F'))
    }
    X <- cbind(ones,x1)
    
    for (i in 1:T) {
      y[i]<-X[i,]%*%Beta + e[i]
    }
    if (fig == TRUE) plot.ts(y,col=4,xlab="Time")
    group<-list(y,x1,Beta)
    names(group)<-c("Y","X","Beta")
    return(invisible(group))
  }
  
  if (Break==TRUE && hetero==TRUE && mis==FALSE){
    R <- T - P
    Beta <- matrix(sample(1:(2*k+1),k+1,rep=T),k+1,1)
    Beta.post <- Beta * delta
    y <- numeric(T)
    ones <- rep(1,T)
    x1 <- matrix(rnorm(T*k,k,sigma^2),T,k)
    e <- numeric(T)
    for (i in 1:T){
      e[i]<-rnorm(1,0,norm(as.matrix(x1[i,]),'F'))
    }
    X <- cbind(ones,x1)
    t0 <- floor(tau*R)
    
    for (i in 1:t0) {
      y[i]<-X[i,]%*%Beta + e[i]
    }
    for (i in (t0+1):T) {
      y[i]<-X[i,]%*%(delta*Beta) + e[i]
    }
    
    if (fig == TRUE) plot.ts(y,col=4,xlab="Time")
    group<-list(y,x1,t0,Beta,Beta.post)
    names(group)<-c("Y","X","Break.date","Beta.pre","Beta.post")
    return(invisible(group))	
  }
  
  if (Break==TRUE && hetero==FALSE && mis==TRUE){
    R <- T - P
    Beta <- matrix(sample(1:(2*k+1),k+1,rep=T),k+1,1)
    g<-rnorm(T,0,1)
    e <- rnorm(T,0,sigma^2)
    y <- numeric(T) 
    ones <- rep(1,T)
    x1 <- matrix(rnorm(T*k,k,sigma^2),T,k)
    X <- cbind(ones,x1)
    
    for (i in 1:T) {
      y[i]<-X[i,]%*%(Beta*g[i]*delta) + e[i]
    }
    
    if (fig == TRUE) plot.ts(y,col=4,xlab="Time")
    group<-list(y,x1,Beta)
    names(group)<-c("Y","X","Beta")
    return(invisible(group))
  }
  
  if (Break==TRUE && hetero==TRUE && mis==TRUE){
    R <- T - P
    Beta <- matrix(sample(1:(2*k+1),k+1,rep=T),k+1,1)
    g<-rnorm(T,0,1)
    y <- numeric(T)
    ones <- rep(1,T)
    x1 <- matrix(rnorm(T*k,k,sigma^2),T,k)
    e <- numeric(T)
    for (i in 1:T){
      e[i]<-rnorm(1,0,norm(as.matrix(x1[i,]),'F'))
    }
    X <- cbind(ones,x1)
    
    for (i in 1:T) {
      y[i]<-X[i,]%*%(Beta*g[i]*delta) + e[i]
    }
    
    if (fig == TRUE) plot.ts(y,col=4,xlab="Time")
    group<-list(y,x1,Beta)
    names(group)<-c("Y","X","Beta")
    return(invisible(group))	
  }
}