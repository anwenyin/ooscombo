# This dgp simulates a "Break-in-GARCH" type time series according to Chen & Hong
# T: simulation sample size
# P: out-of-sample size, need this to make sure that break happens in the training sample
# g.type: break type: 1: one break; 2: two breaks; 3: STGARCH Process


DGP.garch<-function(T,P,g.type=1){
  
  if(missing(T)) T <- 100
  if(missing(P)) P <- floor(0.25 * T)
  g.type <- as.numeric(g.type)
  if(! g.type %in% c(1,2,3)) stop("Please select a break type: 1, 2, or 3.")
  if(T < 0) stop("Sample size must be non-negative!")
  if(P < 0 | P > T) stop("The size of the prediction sample must be non-negative and smaller than that of the total sample.")
  
  B <- 40 # burning period to get rid of initial value effect
  y <- numeric(T+B)
  
  
  if (g.type == 1){
    
    alpha00 <- 0.1
    alpha01 <- 0.2
    beta01 <- 0.4
    
    alpha10 <- 0.3
    alpha11 <- 0.4
    beta11 <- 0.55
    
    tau <- 0.5
    
    R <- T - P
    t0 <- floor(tau*R)
    v <- rnorm(T+B)
    e <- numeric(T+B)
    h <- numeric(T+B)
    v[1] <- rnorm(1)
    h[1] <- rnorm(1)^2
    e[1] <- v[1]*sqrt(h[1])
    y[1] <- e[1]
    
    for (i in 2:(B+t0)) {
      h[i] <- alpha01 + alpha01*e[i-1]^2 + beta01*h[i-1]
      e[i] <- v[i]*sqrt(h[i])
      y[i] <- e[i]
    }
    
    for (i in (B+t0+1):(T+B)) {
      h[i] <- alpha10 + alpha11*e[i-1]^2 + beta11*h[i-1]
      e[i] <- v[i]*sqrt(h[i])
      y[i] <- e[i]
    }
    
    y <- y[(B+1):(T+B)]
    
    group <- list(y)
    names(group) <- c("y")
    return(invisible(group))
  }	
  
  if (g.type == 2){
    
    alpha00 <- 0.1
    alpha01 <- 0.2
    beta01 <- 0.3
    
    alpha10 <- 0.3
    alpha11 <- 0.3
    beta11 <- 0.4
    
    alpha20 <- 0.5
    alpha21 <- 0.4
    beta21 <- 0.55
    
    tau1 <- 0.3
    tau2 <- 0.6
    
    R <- T - P
    t1 <- floor(tau1*R)
    t2 <- floor(tau2*R)
    v <- rnorm(T+B)
    e <- numeric(T+B)
    h <- numeric(T+B)
    v[1] <- rnorm(1)
    h[1] <- rnorm(1)^2
    e[1] <- v[1]*sqrt(h[1])
    y[1] <- e[1]
    
    for (i in 2:(B+t1)) {
      h[i] <- alpha01 + alpha01*e[i-1]^2 + beta01*h[i-1]
      e[i] <- v[i]*sqrt(h[i])
      y[i] <- e[i]
    }
    
    for (i in (B+t1+1):(B+t2)) {
      h[i] <- alpha10 + alpha11*e[i-1]^2 + beta11*h[i-1]
      e[i] <- v[i]*sqrt(h[i])
      y[i] <- e[i]
    }
    
    for (i in (B+t2+1):(B+T)) {
      h[i] <- alpha20 + alpha21*e[i-1]^2 + beta21*h[i-1]
      e[i] <- v[i]*sqrt(h[i])
      y[i] <- e[i]
    }
    
    y <- y[(B+1):(T+B)]
    
    group <- list(y)
    names(group) <- c("y")
    return(invisible(group))
  }	
  
  if (g.type == 3){
    
    alpha00 <- 0.1
    alpha01 <- 0.2
    beta01 <- 0.4	  
    
    R <- T - P
    v <- rnorm(T+B)
    e <- numeric(T+B)
    G <- numeric(T+B)
    h <- numeric(T+B)
    v[1] <- rnorm(1)
    h[1] <- rnorm(1)^2
    e[1] <- v[1]*sqrt(h[1])
    y[1] <- e[1]
    
    for (i in 2:(B+T)) {
      G[i] <- 1/(1 + exp(-5*(i/(T+B) - 0.5)))
      h[i] <- (alpha01 + alpha01*e[i-1]^2 + beta01*h[i-1])*(1 + 0.5*G[i])
      e[i] <- v[i]*sqrt(h[i])
      y[i] <- e[i]
    }
    
    y <- y[(B+1):(T+B)]
    
    group <- list(y)
    names(group) <- c("y")
    return(invisible(group))
  }
  
}