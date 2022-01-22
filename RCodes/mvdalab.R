data(College)
MVcis(College, Vars2Plot = c(1, 2), include.zero = TRUE)

# Simulate from a Multivariate Normal, Poisson, Exponential, or
# Skewed Distribution

Sigma <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), 3, 3)
Means <- rep(0, 3)
Sim.dat.norm <- mvrnorm.svd(n = 1000, Means, Sigma, Dist = "normal")
plot(as.data.frame(Sim.dat.norm))
Sim.dat.pois <- mvrnorm.svd(n = 1000, Means, Sigma, Dist = "poisson")
plot(as.data.frame(Sim.dat.pois))
Sim.dat.exp <- mvrnorm.svd(n = 1000, Means, Sigma, Dist = "exp")
plot(as.data.frame(Sim.dat.exp))
Sim.dat.skew <- mvrnorm.svd(n = 1000, Means, Sigma, Dist = "skewnorm")
plot(as.data.frame(Sim.dat.skew))


# PCA with the NIPALS algorithm: VVIMP

my.nipals <- pca.nipals(iris[, 1:4], ncomps = 4, tol = 1e-08)
names(my.nipals)

#Check results
my.nipals$Loadings
svd(scale(iris[, 1:4], scale = FALSE))$v
nipals.scores <- data.frame(my.nipals$Scores)
names(nipals.scores) <- paste("np", 1:4)
svd.scores <- data.frame(svd(scale(iris[, 1:4], scale = FALSE))$u)
names(svd.scores) <- paste("svd", 1:4)
Scores. <- cbind(nipals.scores, svd.scores)
plot(Scores.)
my.nipals$Loading.Space
my.nipals$Score.Space

# PCA pcaFit : VVIMP

data(iris)
pc1 <- pcaFit(iris, scale = TRUE, ncomp = NULL)
pc1
print(pc1) #Model summary
plot(pc1) #MSEP
PE(pc1) #X-explained variance
T2(pc1, ncomp = 2) #T2 plot
Xresids(pc1, ncomp = 2) #X-residuals plot
scoresplot(pc1) #scoresplot variable importance
(SC <- ScoreContrib(pc1, obs1 = 1:9, obs2 = 10:11)) #score contribution
plot(SC) #score contribution plot
loadingsplot(pc1, ncomp = 1) #loadings plot
loadingsplot(pc1, ncomp = 1:2) #loadings plot
loadingsplot(pc1, ncomp = 1:3) #loadings plot
loadingsplot(pc1, ncomp = 1:7) #loadings plot
loadingsplot2D(pc1, comps = c(1, 2)) #2-D loadings plot
loadingsplot2D(pc1, comps = c(2, 3)) #2-D loadings plot

# Plotting function for Significant Multivariate Correlation

data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 3, validation = "loo")
smc(mod1)
plot(smc(mod1))

#  Plot of Auto-correlation Funcion
data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "loo")
acfplot(mod1, parm = NULL)
