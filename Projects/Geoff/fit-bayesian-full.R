##
## Percent budburst and the beta-binomial distribution
##
## Date: April 21 2020
## By: Geoffrey Legault, University of British Columbia

## Load VGAM library. This has convenient functions for generating beta-binomial random variables
## and for fitting beta-binomial distributions to data via maximum likelihood estimation
## install.packages("VGAM")
library(VGAM)

## Set parameter values
param <- list(s1 = 2,
              s2 = 7,
              trials = 10)

## Generate samples from (unshifted) beta-binomial distribution
set.seed(20200329) # set seed for reproducibility
nsamples <- 2000 # how many samples?
sim.stor <- rbetabinom.ab(n = nsamples, # use VGAM generator
                          size = param[["trials"]],
                          shape1 = param[["s1"]],
                          shape2 = param[["s2"]])

## Plot simulated data
plot(NA,
     xlim = c(0, param[["trials"]] + 7),
     ylim = c(0, .25),
     xlab = "Day of flowering",
     ylab = "Proportion",
     )
tab.sim <- table(sim.stor)
points((tab.sim / nsamples) ~ c(0:10), type = "l", col = "blue")
points((tab.sim / nsamples) ~ c(0:10), type = "p", pch = 16, col = "blue", cex = 2)

## Load rstan library
library(rstan)
options(mc.cores = parallel::detectCores())

## Put simulated data into list for stan
germ.stan <- list(N = length(sim.stor),
                  G = sim.stor,
                  D = 8,
                  Days = c(max(sim.stor):(max(sim.stor)+7)))

## Fit model
fit.stanmarg <- stan("betabinom-marg.stan",
                 data = germ.stan,
                 iter = 4000,
                 warmup = 2000,
                 chains = 4)

summary(fit.stanmarg, pars = c("shape1", "shape2", "D_new"))$summary
extract.stanmarg <- extract(fit.stanmarg)

# Shape1
plot(density(extract.stanmarg$shape1), main = "Shape1")
abline(v = 2, col = "blue")

# Shape2
plot(density(extract.stanmarg$shape2), main = "Shape2")
abline(v = 7, col = "blue")

# Day (last day of budburst)
tab1 <- table(extract.stanmarg$D_new)
plot(as.vector(tab1) ~ c(10:17), main = c("Last day"))
     
## ## Plot simulated data with predictions
## plot(NA,
##      xlim = c(0, param[["trials"]]),
##      ylim = c(0, .5),
##      xlab = "Day of flowering",
##      ylab = "Proportion",
##      )
## tab.sim <- table(sim.stor)
## points((tab.sim / nsamples) ~ c(0:(length(tab.sim) - 1)), type = "l", col = rgb(0, 0, 1, alpha = .4))
## points((tab.sim / nsamples) ~ c(0:(length(tab.sim) - 1)), type = "p", pch = 16, col = rgb(0, 0, 1, alpha = .4), cex = 1.5)
## tab.pred <- table(extract.stan$G_new)
## points((tab.pred / sum(tab.pred)) ~ c(0:(length(tab.pred) - 1)), type = "l", col = rgb(1, 0, 0, alpha = .4))
## points((tab.pred / sum(tab.pred)) ~ c(0:(length(tab.pred) - 1)), type = "p", pch = 16, col = rgb(1, 0, 0, alpha = .4), cex = 1.5)
## legend("topright", c("Simulated data", "Predictions"), pch = 16, bty = "n", inset = 0.03, cex = 1.5,
##        col = c(rgb(0, 0, 1, alpha = .4), rgb(1, 0, 0, alpha = .4)))

