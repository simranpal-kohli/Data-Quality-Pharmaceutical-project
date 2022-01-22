par(mfrow=c(3,1))
# First, generate some Gaussian numbers.
gaussian <- rnorm(1000,0.0,0.05)
gh <- hist(gaussian, breaks=1000)

empirical_cumulative_distribution <- cumsum(gh$counts)/1000

plot(gh$mids, empirical_cumulative_distribution)


uniformize <- function(x) {
  ans_x <- x
  for (idx in seq(1,length(x))){
    max_idx <- max(which(gh$mids < x[idx]))
    ans_x[idx] <- empirical_cumulative_distribution[max_idx]
  }
  return(ans_x)
}

uniform2 <- uniformize(gaussian )
hist(uniform2, breaks=100)
par(mfrow=c(1,1))


#------------------------------------------------
library(tiger)
a <- rnorm(100)
hist(a)
b <- to.uniform(a)
hist(b)
c <- to.uniform(ref=a, val=c(-0.5,0,0.5))
hist(c)

#-------------------------------------------------
library("MASS")
library("bestNormalize")
truehist(final_qcc_df$AIR101)

mean(final_qcc_df$AIR101) #40.81
sd(final_qcc_df$AIR101) #8.517
# Perform some tranformations individually

# arcsinh transformation
arcsinh_obj <- arcsinh_x(final_qcc_df$AIR101)
truehist(arcsinh_obj$x.t)
hist(arcsinh_obj$x.t)
# mean 4.383 and sd 0.1954
arcsinh_obj$mean
arcsinh_obj$sd

# Box Cox's Transformation
(boxcox_obj <- boxcox(final_qcc_df$AIR101))
truehist(boxcox_obj$x.t)
boxcox_obj$mean #1.025
boxcox_obj$sd #0.005843

# Yeo-Johnson's Transformation
(yeojohnson_obj <- yeojohnson(final_qcc_df$AIR101))
truehist(yeojohnson_obj)
yeojohnson_obj$mean #0.9832
yeojohnson_obj$sd #0.9832

# orderNorm Transformation
(orderNorm_obj <- orderNorm(final_qcc_df$AIR101))
truehist(orderNorm_obj$x.t)
orderNorm_obj$ties_status


par(mfrow = c(2,2))
MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = 12)
MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 12)

# Pick the best one automatically
(BNobject <- bestNormalize(final_qcc_df$AIR101))
truehist(BNobject$x.t)

par(mfrow = c(1,2))
MASS::truehist(BNobject$x.t, 
               main = paste("Best Transformation:", 
                            class(BNobject$chosen_transform)[1]), nbins = 12)
plot(final_qcc_df$AIR101, predict(BNobject, newdata = final_qcc_df$AIR101), type = "l", col = 1, 
     main = "Best Normalizing transformation", ylab = "g(x)", xlab = "x")

# Boxplot
par(mfrow = c(1,1))
boxplot(log10(BNobject$oos_preds), yaxt = 'n')
axis(2, at=log10(c(.1,.5, 1, 2, 5, 10)), labels=c(.1,.5, 1, 2, 5, 10))

BNobject=bestNormalize(final_qcc_df$AIR101, allow_orderNorm = FALSE, out_of_sample = FALSE)
par(mfrow = c(1,2))
MASS::truehist(BNobject$x.t, 
               main = paste("Best Transformation:", 
                            class(BNobject$chosen_transform)[1]), nbins = 12)
plot(final_qcc_df$AIR101, predict(BNobject, newdata = final_qcc_df$AIR101), type = "l", col = 1, 
     main = "Best Normalizing transformation", ylab = "g(x)", xlab = "x")

#-----

### PLS MODEL FIT WITH method = 'bidiagpls' and validation = 'oob', i.e. bootstrapping ###

data(Penta)
## Number of bootstraps set to 300 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], method = "bidiagpls",
               ncomp = 2, validation = "oob", boots = 300)
summary(mod1) #Model summary
### PLS MODEL FIT WITH method = 'bidiagpls' and validation = 'loo', i.e. leave-one-out CV ###
## Not run:
mod2 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], method = "bidiagpls",
               ncomp = 2, validation = "loo")

#--------------------------
#--- Test code lines------
# LSL <- -3 # lower specification limit
# USL <- 3 # upper specification limit
# n <- 5 # batch size
# lambda <- 0.1 # EWMA smoothing parameter
# L0 <- 1000 # in-control Average Run Length (ARL)
# z0 <- h.mu(0) # start at minimal defect level
# 
# ucl <- phat.ewma.crit(lambda, L0, 0, n, z0, LSL=LSL, USL=USL)
# h.mu <- function(mu) pnorm(-3-mu) + pnorm(mu-3)
# ewma <- function(x, lambda=0.1, z0=0) filter(lambda*x, 1-lambda, m="r", init=z0)
# 
# x0 <- matrix(rnorm(50*5), ncol=5) # in-control data
# x0
# x1 <- matrix(rnorm(50*5, mean=0.5), ncol=5)# out-of-control data
# x1
# x <- rbind(x0,x1) # all data
# x
# xbar <- apply(x, 1, mean)
# phat <- h.mu(xbar)
# z <- ewma(phat, lambda=lambda, z0=z0)

#----------------------------------
mu1   <- c(-4, 4)
mu2 <- c(3, 3)
Sigma <- matrix(c(16,-2, -2,9), byrow=TRUE, ncol=2)
Nj    <- c(15, 25)
Y1    <- round(rmvnorm(Nj[1], mean=mu1, sigma=Sigma))
Y2  <- round(rmvnorm(Nj[2], mean=mu2, sigma=Sigma))
Y12 <- rbind(Y1, Y2)
IV  <- factor(rep(1:2, Nj))

anova(lm(Y12 ~ IV), test="Hotelling-Lawley")
summary(manova(Y12 ~ IV), test="Hotelling-Lawley")


#----------------------------------
-1.190     0.590     -0.119     0.059        2.1886
0.120     0.900     -0.095     0.143        2.0697
-1.690     0.400     -0.255     0.169        4.8365
0.300     0.460     -0.199     0.198        3.4158
0.890    -0.750     -0.090     0.103        0.7089
0.820     0.980      0.001     0.191        0.9268
-0.300     2.280     -0.029     0.400        4.0018
0.630     1.750      0.037     0.535        6.1657
1.560     1.580      0.189     0.639        7.8554
1.460     3.050      0.316     0.880       14.4158

da=c(-1.190,0.120,-1.690,0.300,0.890,0.820,-0.300,0.630,1.560,1.460)
mean(da)
sd(da)
