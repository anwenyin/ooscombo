# This dgp simulates a time series according to AR(2) plus two other predictors mean and "wild" heteroscedastic error
# We only consider structural break case for this DGP
# T: simulation sample size
# P: out-of-sample size, need this to make sure that break happens in the training sample
# sigma: standard deviation for the error without GARCH effect
# tau: break fraction relative to the training sample
# delta: parameter controls break size

DGP.wild<-function(T,P,sigma=2,tau=0.3,delta=0.5){
  
  if(missing(T)) T <- 100
  if(missing(P)) P <- floor(0.25 * T)
  if(T < 0) stop("Sample size must be non-negative!")
  if(P < 0 | P > T) stop("The size of the prediction sample must be non-negative and smaller than that of the total sample.")
  
  mu <- 2
  rho1 <- 0.4
  rho2 <- 0.2
  
  theta1 <- 0.8 
  theta2 <- -0.4
  
  B <- 40 # burning period to get rid of initial value effect
  y <- numeric(T+B)
  x <- matrix(0,T,4)
  z1 <- rnorm(T+B,0,sigma)
  z2 <- runif(T+B,-2,2)
  
  y[1] <- runif(1)
  y[2] <- runif(1)
  
  if (any(abs(delta*rho1)>=1,abs(delta*rho2)>=1)) {
    stop("Break has caused unit root or explosive process!")
  } else { 
    R <- T - P
    t0 <- floor(tau*R)
    #e <- numeric(T+B)
    
    for (i in 3:(B+t0)) {
      y[i] <- mu + rho1*y[i-1] + rho2*y[i-2] + theta1*z1[i] + theta2*z2[i] + rnorm(1,0,abs(y[i-1]))
    }
    for (i in (B+t0+1):(T+B)) {
      y[i] <- delta*mu + delta*rho1*y[i-1] + delta*rho2*y[i-2] + delta*theta1*z1[i] + delta*theta2*z2[i] + rnorm(1,0,abs(y[i-1]))
    }
    
    x[,1] <- y[B:(T+B-1)]
    x[,2] <- y[(B-1):(T+B-2)]
    x[,3] <- z1[(B+1):(T+B)]
    x[,4] <- z2[(B+1):(T+B)]
    y <- y[(B+1):(T+B)]
    Beta <- c(mu, rho1, rho2, theta1, theta2, t0, delta*mu, delta*rho1, delta*rho2)
    
    group <- list(y,x,Beta)
    names(group) <- c("y","X","Para")
    return(invisible(group))
  } 
}
