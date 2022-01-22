# Percent defective for normal samples
n <- 5
LSL <- -3
USL <- 3
par(mar=c(5, 5, 1, 1) + 0.1)
p.star <- 2*pnorm( (LSL-USL)/2 ) # for p <= p.star pdf and cdf vanish
p_ <- seq(p.star+1e-10, 0.07, 0.0001) # define support of Figure 1
# Figure 1 (c)
pp_ <- pphat(p_, n)
plot(p_, pp_, type="l", xlab="p", ylab=expression(P( hat(p) <= p )),
     xlim=c(0, 0.06), ylim=c(0,1), lwd=2)

#

gX <- 5
gY <- 24
kL <- 16
kU <- 24
mu0 <- 22
L0 <- euklid.ewma.arl(gX, gY, kL, kU, mu0, mu0)
L0
# should be 1219.2