# A very preliminary HAC estimator. Low speed.
# X: Regressors Matrix
# e: Model Estimation Residuals
# A very 'rough' bandwidth selection formular picked from 'Econometric Modeling with Time Series'

HAC <- function(X,e){
  X <- as.matrix(X)
  Q <- solve(t(X)%*%X)
  e <- as.matrix(e)
  T <- length(e)
  m <- floor(4*(T/100)^(2/9))
  bag0 <- list(T)
  bag1 <- list(m)
  
  for (i in 1:T){
    bag0[[i]] <- X[i,]%*%t(X[i,])*e[i]*e[i]
  }	
  g0 <- Reduce('+',bag0)
  
  for (i in 1:m){
    for (t in (i+1):T){
      bag1[[i]] <- (1 - abs(i/m))*(X[t,]%*%t(X[t-i,])*e[t]*e[t-i] + X[t-i,]%*%t(X[t,])*e[t-i]*e[t])/T
    }
  }
  g1 <- Reduce('+',bag1)
  
  meat <- g0 + g1
  OMEGA <- Q %*% meat %*% Q
  hac <- list(meat, OMEGA)
  names(hac) <- c('MEAT','OMEGA')
  return(invisible(hac))	
}

# EXAMPLE
# This is an example for this function. It may not be efficient
# X <- cbind(rep(1,100),matrix(rnorm(400,10,5),100,4))
# beta <- matrix(c(2,1.2,1.8,-3,-0.5),5,1)
# y <- X %*% beta + rnorm(100,0,10)
# model <- lm(y~X)
# e <- model$resid
# HAC(X,e)