
rm(list=ls())
source('R/dgp_ar1.R')
source('R/dgp_ar2and5.R')
source('R/dgp_wild.R')
source('R/goos.R')
source('R/feval.R')
source('R/supfun.R')

set.seed(1234)
T <- 1000

#######################################################################################
#  																				  #
# In this case, the DGP is AR(2) plus 5, but the forecasting model is just AR(1).	  #
#																					  #
#######################################################################################

########## ARCH

for (i in c(0.01,0.1,0.4)){
  for (j in c(40)){
    sim <- matrix(nrow=T,ncol=7)
    for (m in 1:T){
      dta <- DGP.ar2and5(T=200, P=j, Break=TRUE,hetero=TRUE, sigma=2, tau=0.3,delta=i, beta1=0)
      y <- dta$y
      X <- dta$X[,1]
      temp <- feval(y=y,X=X,P=j,Window='recursive')$mat
      sim[m,1] <- temp[1,1]
      sim[m,2] <- temp[1,2]
      sim[m,3] <- temp[1,3]
      sim[m,4] <- temp[1,4]
      sim[m,5] <- temp[1,5]
      sim[m,6] <- temp[1,6]
      sim[m,7] <- temp[1,7]
    }
    result <- matrix(nrow=2,ncol=7)
    colnames(result) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
    rownames(result) <- c("RMSFE","Ratio")
    result[1,] <-apply(sim,2,mean)
    for (s in 1:7) result[2,s] <- result[1,s]/result[1,5]
    cat("P = ",j,"delta = ",i,"True: Break Model with Heteroskedasticity-ARCH, Recursive Window","\n","\n")
    print(round(result,4))
    cat("\n")
  }
}


#######################################################################################
#    																			  #
# In this case, the DGP is AR(2) plus 5, but the forecasting model is just 4 predictors.	  #
#																					  #
#######################################################################################

########## ARCH

for (i in c(0.01,0.1,0.4)){
  for (j in c(40)){
    sim <- matrix(nrow=T,ncol=7)
    for (m in 1:T){
      dta <- DGP.ar2and5(T=200, P=j, Break=TRUE,hetero=TRUE, sigma=2, tau=0.3,delta=i, beta1=0)
      y <- dta$y
      X <- dta$X[,3:6]
      temp <- feval(y=y,X=X,P=j,Window='recursive')$mat
      sim[m,1] <- temp[1,1]
      sim[m,2] <- temp[1,2]
      sim[m,3] <- temp[1,3]
      sim[m,4] <- temp[1,4]
      sim[m,5] <- temp[1,5]
      sim[m,6] <- temp[1,6]
      sim[m,7] <- temp[1,7]
    }
    result <- matrix(nrow=2,ncol=7)
    colnames(result) <- c("Cp","CV","SIC","S-W","Equal","Break","Stable")
    rownames(result) <- c("RMSFE","Ratio")
    result[1,] <-apply(sim,2,mean)
    for (s in 1:7) result[2,s] <- result[1,s]/result[1,5]
    cat("P = ",j,"delta = ",i,"True: Break Model with Heteroskedasticity-ARCH, Recursive Window","\n","\n")
    print(round(result,4))
    cat("\n")
  }
}