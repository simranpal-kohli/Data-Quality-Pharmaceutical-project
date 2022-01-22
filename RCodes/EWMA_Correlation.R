# econ589multivariateGarch.r
#
# R examples for lectures on multivariate GARCH models
#
# Eric Zivot
# May 8th, 2012
# update history


# load libraries

library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(rmgarch)
options(digits=4)

computer = "work"
if (computer == "home") {
  setwd("C:\\Users\\ezivot\\Dropbox\\econ589\\R\\")
}
if (computer == "work") {
  setwd("C:\\Users\\ezivot.SOCIOLOGY\\Dropbox\\econ589\\R\\")
}

setwd('D:\\OneDrive\\OneDrive\\MSc_Project\\Data')
source("covEWMA.r")

# download data
symbol.vec = c("MSFT", "^GSPC")
getSymbols(symbol.vec, from ="2000-01-03", to = "2012-04-03")
colnames(MSFT)
start(MSFT)
end(MSFT)

# extract adjusted closing prices
MSFT = MSFT[, "MSFT.Adjusted", drop=F]
GSPC = GSPC[, "GSPC.Adjusted", drop=F]

# plot prices
plot(MSFT)
plot(GSPC)

# calculate log-returns for GARCH analysis
MSFT.ret = CalculateReturns(MSFT, method="log")
GSPC.ret = CalculateReturns(GSPC, method="log")


# remove first NA observation
MSFT.ret = MSFT.ret[-1,]
GSPC.ret = GSPC.ret[-1,]
colnames(MSFT.ret) ="MSFT"
colnames(GSPC.ret) = "GSPC"

# create combined data series
MSFT.GSPC.ret = merge(MSFT.ret,GSPC.ret)

# plot returns
plot(MSFT.ret)
plot(GSPC.ret)

# scatterplot of returns
plot( coredata(GSPC.ret), coredata(MSFT.ret), xlab="GSPC", ylab="MSFT",
      type="p", pch=16, lwd=2, col="blue")
abline(h=0,v=0)

#
# compute rolling correlations
#
# chart.RollingCorrelation(MSFT.ret, GSPC.ret, width=20)

cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

roll.cov = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=20,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=20,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="20-day rolling covariances",
     ylab="covariance", lwd=2, col="blue")
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(roll.cor, main="20-day rolling correlations",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))

#
# calculate EWMA covariances and correlations
#
lambda <- 0.94
cov.ewma <- covEWMA(as.data.frame(MSFT.GSPC.ret), lambda=lambda)

## 2. extract conditional variance and correlation
### conditional variance
MSFT.GSPC.cond.cov <- cov.ewma[,2,1];
### conditional correlation
t <- length(cov.ewma[,1,1]);
MSFT.GSPC.cond.cor<- rep(0,t);
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i]<- cov2cor(cov.ewma[i,,])[1,2];
}
### Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,
     type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
     main="EWMA Covariance between MSFT and S&P500");
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor,
     type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
     main="EWMA Correlation between MSFT and S&P500");
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))

# compute rolling covariances and correlations using longer window
roll.cov = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=252,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=252,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="252-day rolling covariances",
     ylab="covariance", lwd=2, col="blue")
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(roll.cor, main="252-day rolling correlations",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))



# compute EWMA covariances and correlations using longer half-life
half.life = 125 
lambda = exp(log(0.5)/half.life)
cov.ewma <- covEWMA(MSFT.GSPC.ret, lambda=lambda)


## 2. extract conditional variance and correlation
### conditional variance
MSFT.GSPC.cond.cov <- cov.ewma[,2,1]
### conditional correlation
t <- length(cov.ewma[,1,1])
MSFT.GSPC.cond.cor<- rep(0,t)
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i]<- cov2cor(cov.ewma[i,,])[1,2]
}
### Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,
     type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
     main="EWMA Covariance between MSFT and S&P500")
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor,
     type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
     main="EWMA Correlation between MSFT and S&P500")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))


#
# DCC estimation
#

# univariate normal GARCH(1,1) for each series
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), 
                          distribution.model = "norm")

# dcc specification - GARCH(1,1) for conditional correlations
dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), 
                           dccOrder = c(1,1), 
                           distribution = "mvnorm")
dcc.garch11.spec

dcc.fit = dccfit(dcc.garch11.spec, data = MSFT.GSPC.ret)
class(dcc.fit)
slotNames(dcc.fit)
names(dcc.fit@mfit)
names(dcc.fit@model)

# many extractor functions - see help on DCCfit object
# coef, likelihood, rshape, rskew, fitted, sigma, 
# residuals, plot, infocriteria, rcor, rcov
# show, nisurface

# show dcc fit
dcc.fit

# plot method
plot(dcc.fit)
# Make a plot selection (or 0 to exit): 
#   
# 1:   Conditional Mean (vs Realized Returns)
# 2:   Conditional Sigma (vs Realized Absolute Returns)
# 3:   Conditional Covariance
# 4:   Conditional Correlation
# 5:   EW Portfolio Plot with conditional density VaR limits

# conditional sd of each series
plot(dcc.fit, which=2)

# conditional correlation
plot(dcc.fit, which=4)

# extracting correlation series
ts.plot(rcor(dcc.fit)[1,2,])

#
# forecasting conditional volatility and correlations
#

dcc.fcst = dccforecast(dcc.fit, n.ahead=100)
class(dcc.fcst)
slotNames(dcc.fcst)
class(dcc.fcst@mforecast)
names(dcc.fcst@mforecast)

# many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# show forecasts
dcc.fcst

# plot forecasts


# ---------------------------------------
covEWMA <- function(factors, lambda=0.96, return.cor=FALSE) {
  ## Inputs:
  ## factors    N x K numerical factors data.  data is class data.frame
  ##            N is the time length and K is the number of the factors.  
  ## lambda     scalar. exponetial decay factor between 0 and 1. 
  ## return.cor Logical, if TRUE then return EWMA correlation matrices
  ## Output:  
  ## cov.f.ewma  array. dimension is N x K x K.
  ## comments:
  ## 1. add optional argument cov.start to specify initial covariance matrix
  ## 2. allow data input to be data class to be any rectangular data object
  
  
  if (is.data.frame(factors)){
    factor.names  = colnames(factors)
    t.factor      = nrow(factors)
    k.factor      = ncol(factors)
    factors       = as.matrix(factors)
    t.names       = rownames(factors)
  } else {
    stop("factor data should be saved in data.frame class.") 
  }
  if (lambda>=1 || lambda <= 0){
    stop("exponential decay value lambda should be between 0 and 1.")
  } else {
    cov.f.ewma = array(,c(t.factor,k.factor,k.factor))
    cov.f = var(factors)  # unconditional variance as EWMA at time = 0 
    FF = (factors[1,]- mean(factors)) %*% t(factors[1,]- mean(factors))
    cov.f.ewma[1,,] = (1-lambda)*FF  + lambda*cov.f
    for (i in 2:t.factor) {
      FF = (factors[i,]- mean(factors)) %*% t(factors[i,]- mean(factors))
      cov.f.ewma[i,,] = (1-lambda)*FF  + lambda*cov.f.ewma[(i-1),,]
    }
    
  }
  # 9/15/11: add dimnames to array
  dimnames(cov.f.ewma) = list(t.names, factor.names, factor.names)
  
  if(return.cor) {
    cor.f.ewma = cov.f.ewma
    for (i in 1:dim(cor.f.ewma)[1]) {
      cor.f.ewma[i, , ] = cov2cor(cov.f.ewma[i, ,])
    }
    return(cor.f.ewma)
  } else{
    return(cov.f.ewma)  
  }
}
