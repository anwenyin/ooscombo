# This dgp generates a time series according to AR(2) with intercept mean and GARCH error
# T: simulation sample size
# P: out-of-sample size, need this to make sure that break happens in the training sample
# Break: one-time full break in the conditional mean
# hetero: if the error follows a GARCH/ARCH process
# sigma: standard deviation for the error without GARCH effect
# tau: break fraction relative to the training sample
# delta: parameter controls break size
# beta1: GARCH coefficient; if zero, then error follows an ARCH process
# All parameter values are set to ensure regieme-wise stationarity and all GARCH model parameter restrictions


DGP.ar2<-function(T,P,Break=FALSE,hetero=FALSE, sigma=2, tau=0.3,delta=0.5, beta1=0){
  
  if(missing(T)) T <- 100
  if(missing(P)) P <- floor(0.25 * T)
  if(T < 0) stop("Sample size must be non-negative!")
  if(P < 0 | P > T) stop("The size of the prediction sample must be non-negative and smaller than that of the total sample.")
  
  mu <- 2
  rho1 <- 0.4
  rho2 <- 0.2
  alpha0 <- 1
  alpha1 <- 0.4
  
  if(beta1 < 0) stop("GARCH coefficient must be non-negative!")
  if((alpha1 + beta1) >= 1) stop("Invalid parameter value for the GARCH process!")
  
  B <- 40 # burning period to get rid of initial value effect
  y <- numeric(T+B)
  x <- matrix(0,T,2)
  y[1] <- 0
  y[2] <- runif(1)
  
  if (Break==FALSE && hetero==FALSE){
    
    e <- rnorm((T+B),0,sigma)
    
    for (i in 3:(T+B)) {
      y[i] <- mu + rho1*y[i-1] + rho2*y[i-2] + e[i]
    }
    
    x[,1] <- y[B:(T+B-1)]
    x[,2] <- y[(B-1):(T+B-2)]
    y <- y[(B+1):(T+B)]
    Beta <- c(mu, rho1, rho2)
    
    group <- list(y,x,Beta)
    names(group) <- c("y","X","Beta")
    return(invisible(group))
  }
  
  if (Break==TRUE && hetero==FALSE){
    if (any(abs(delta*rho1)>=1,abs(delta*rho2)>=1)) {
      stop("Break has caused unit root or explosive process!")
    } else { 
      R <- T - P
      e <- rnorm((T+B),0,sigma)
      t0 <- floor(tau*R)
      
      for (i in 3:(B+t0)) {
        y[i] <- mu + rho1*y[i-1] + rho2*y[i-2] + e[i] 
      }
      for (i in (B+t0+1):(T+B)) {
        y[i] <- delta*mu + delta*rho1*y[i-1] + delta*rho2*y[i-2] + e[i]
      }
      
      x[,1] <- y[B:(T+B-1)]
      x[,2] <- y[(B-1):(T+B-2)]
      y <- y[(B+1):(T+B)]
      Beta <- c(mu, rho1, rho2, t0, delta*mu, delta*rho1, delta*rho2)
      
      group <- list(y,x,Beta)
      names(group) <- c("y","X","Para")
      return(invisible(group))
    }
  }
  
  if (Break==FALSE && hetero==TRUE){
    
    v <- rnorm(T+B)
    e <- numeric(T+B)
    h <- numeric(T+B)
    v[1] <- rnorm(1)
    h[1] <- rnorm(1)^2
    e[1] <- v[1]*sqrt(h[1])
    
    for (i in 2:(T+B)){
      h[i] <- alpha0 + alpha1*e[i-1]^2 + beta1*h[i-1]
      e[i] <- v[i]*sqrt(h[i])
    }
    
    for (i in 3:(T+B)) {
      y[i] <- mu + rho1*y[i-1] + rho2*y[i-2] + e[i]
    }
    
    x[,1] <- y[B:(T+B-1)]
    x[,2] <- y[(B-1):(T+B-2)]
    y <- y[(B+1):(T+B)]
    Beta <- c(mu, rho1, rho2, alpha0, alpha1, beta1)
    
    group <- list(y,x,Beta)
    names(group) <- c("y","X","Para")
    return(invisible(group))
  }
  
  if (Break==TRUE && hetero==TRUE){
    if (any(abs(delta*rho1)>=1,abs(delta*rho2)>=1)) {
      stop("Break has caused unit root or explosive process!")
    } else {
      R <- T - P
      t0 <- floor(tau*R)
      v <- rnorm(T+B)
      e <- numeric(T+B)
      h <- numeric(T+B)
      v[1] <- rnorm(1)
      h[1] <- rnorm(1)^2
      e[1] <- v[1]*sqrt(h[1])
      
      for (i in 2:(T+B)){
        h[i] <- alpha0 + alpha1*e[i-1]^2 + beta1*h[i-1]
        e[i] <- v[i]*sqrt(h[i])
      }
      
      for (i in 3:(B+t0)) {
        y[i] <- mu + rho1*y[i-1] + rho2*y[i-2] + e[i]
      }
      for (i in (B+t0+1):(T+B)) {
        y[i] <- delta*mu + delta*rho1*y[i-1] + delta*rho2*y[i-2] + e[i]
      }
      
      x[,1] <- y[B:(T+B-1)]
      x[,2] <- y[(B-1):(T+B-2)]
      y <- y[(B+1):(T+B)]
      
      Beta <- c(mu, rho1, rho2, t0, delta*mu, delta*rho1, delta*rho2, alpha0, alpha1, beta1)
      
      group <- list(y,x,Beta)
      names(group) <- c("y","X","Para")
      return(invisible(group))
    }	
  }
}
