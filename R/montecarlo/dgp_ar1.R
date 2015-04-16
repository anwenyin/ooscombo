# This dgp generates a time series according to AR(1) with intercept mean and GARCH error
# T: simulation sample size
# P: out-of-sample size, need this to make sure that break happens in the training sample
# Break: one-time full break in the conditional mean
# hetero: if the error follows a GARCH/ARCH process
# sigma: standard deviation for the error without GARCH effect
# tau: break fraction relative to the training sample
# delta: parameter controls break size
# beta1: GARCH coefficient; if zero, then error follows an ARCH process
# All parameter values are set to ensure regime-wise stationarity and all GARCH model parameter restrictions

DGP.ar1 <- function(T, P, Break=FALSE, hetero=FALSE, sigma=2, tau=0.3, delta=0.5, beta1=0){
  
  if(missing(T)) T <- 100
  if(missing(P)) P <- floor(0.25 * T)
  if(T < 0) stop("Sample size must be non-negative!")
  if(P < 0 | P > T) stop("The size of the prediction sample must be non-negative and smaller than that of the total sample.")
  
  mu <- 2
  rho <- 0.4
  alpha0 <- 1
  alpha1 <- 0.4
  
  if(beta1 < 0) stop("GARCH coefficient must be non-negative!")
  if((alpha1 + beta1) >= 1) stop("Invalid parameter value for the GARCH process!")
  
  B <- 40 # burning period to get rid of initial value effect
  y <- numeric(T+B)
  y[1] <- 0
  
  if (Break==FALSE && hetero==FALSE){
    
    e <- rnorm((T+B),0,sigma)
    
    invisible(unlist(lapply(2:(T+B), function(i) y[i] <<- mu + rho*y[i-1] + e[i])))
    
    x <- y[B:(T+B-1)]
    y <- y[(B+1):(T+B)]
    Beta <- c(mu, rho)
    
    group <- list(y,x,Beta)
    names(group) <- c("y","X","Beta")
    return(invisible(group))
  }
  
  if (Break==TRUE && hetero==FALSE){
    if (abs(delta*rho) >= 1) {
      stop("Break has caused unit root or explosive process!")
    } else { 
      R <- T - P
      e <- rnorm((T+B),0,sigma)
      t0 <- floor(tau*R)
      
      invisible(unlist(lapply(2:(B+t0), function(i) y[i] <<- mu + rho*y[i-1] + e[i])))
      invisible(unlist(lapply((B+t0+1):(T+B), function(i) y[i] <<- mu + rho*y[i-1] + e[i])))
      
      x <- y[B:(T+B-1)]
      y <- y[(B+1):(T+B)]
      Beta <- c(mu, rho, t0, delta*mu, delta*rho)
      
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
    
    invisible(unlist(lapply(2:(T+B), function(i) {h[i] <<- alpha0 + alpha1*e[i-1]^2 + beta1*h[i-1]; e[i] <<- v[i]*sqrt(h[i])})))
    invisible(unlist(lapply(2:(T+B), function(i) y[i] <<- mu + rho*y[i-1] + e[i])))
    
    x <- y[B:(T+B-1)]
    y <- y[(B+1):(T+B)]
    Beta <- c(mu, rho, alpha0, alpha1, beta1)
    
    group <- list(y,x,Beta)
    names(group) <- c("y","X","Para")
    return(invisible(group))
  }
  
  if (Break==TRUE && hetero==TRUE){
    if (abs(delta*rho) >= 1) {
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
      
      invisible(unlist(lapply(2:(T+B), function(i) {h[i] <<- alpha0 + alpha1*e[i-1]^2 + beta1*h[i-1]; e[i] <<- v[i]*sqrt(h[i])})))
      invisible(unlist(lapply(2:(B+t0), function(i) y[i] <<- mu + rho*y[i-1] + e[i])))
      invisible(unlist(lapply((B+t0+1):(T+B), function(i) y[i] <<- delta*mu + delta*rho*y[i-1] + e[i])))
      
      x <- y[B:(T+B-1)]
      y <- y[(B+1):(T+B)]
      
      Beta <- c(mu, rho, t0, delta*mu, delta*rho, alpha0, alpha1, beta1)
      
      group <- list(y,x,Beta)
      names(group) <- c("y","X","Para")
      return(invisible(group))
    }	
  }
}
