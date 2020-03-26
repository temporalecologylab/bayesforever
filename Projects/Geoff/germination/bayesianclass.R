##
## Percent budburst and the beta-binomial distribution
##
## Date: March 26 2020
## By: Geoffrey Legault, University of British Columbia

##
## Part 1: Simulations and frequentist inference
##

## Load VGAM library. This has convenient functions for generating beta-binomial random variables
## and for fitting beta-binomial distributions to data via maximum likelihood estimation
## install.packages("VGAM")
library(VGAM)

## Set parameter values
param <- list(s1 = 2,
              s2 = 7,
              trials = 10)

## Generate samples from (unshifted) beta-binomial distribution
set.seed(20200326) # set seed for reproducibility
nsamples <- 2000 # how many samples?
sim.stor <- rbetabinom.ab(n = nsamples, # use VGAM generator
                          size = param[["trials"]],
                          shape1 = param[["s1"]],
                          shape2 = param[["s2"]])

## Plot simulated data
plot(NA,
     xlim = c(0, param[["trials"]]),
     ylim = c(0, .25),
     xlab = "Day of flowering",
     ylab = "Proportion",
     )
tab.sim <- table(sim.stor)
points((tab.sim / nsamples) ~ c(0:(length(tab.sim) - 1)), type = "l", col = "blue")
points((tab.sim / nsamples) ~ c(0:(length(tab.sim) - 1)), type = "p", pch = 16, col = "blue", cex = 2)

## Fit beta-binomial model to simulated data
fit.vgam <- vglm(cbind(sim.stor, param[["trials"]] - sim.stor) ~ 1,
                 family = betabinomialff,
                 trace = TRUE)
### Summarize
summary(fit.vgam)
### Compare to true values
t(param[c("s1", "s2")]) # true values
exp(coef(fit.vgam)) # estimated values

## It will be useful later on to 

## Source beta-binomial function
source("betabinomial.R")$value
## Source probability mass function
source("betabinomial-pmf.R")$value
## Source likelihood function
source("nll.R")$value

## Parameters that must be positive
positives <- c("shape1", "shape2")

## Fit model
fit.nll <- optim(par = list(shape1 = log(param[["s1"]]),
                            shape2 = log(param[["s2"]])),
                 fn = nll,
                 lb = 0,
                 ub = param[["trials"]],
                 data = sim.stor,
                 pmf = betabinomial.pmf,
                 pos = positives,
                 method = "Nelder-Mead",
                 control = list(maxit = 10000, trace = 1))
fit.nll
### Compare to true values and VGAM
t(param[c("s1", "s2")]) # true values
exp(coef(fit.vgam)) # estimated values (VGAM)
exp(fit.nll$par) # estimated values (nll)
### Compare log likelihoods
-fit.vgam@criterion$loglikelihood # VGAM
fit.nll$value # nll

##
## Part 2: Simulations and Bayesian inference
##

## Prior predictive check
##
## shape1 ~ Uniform(.5, 6)
## shape2 ~ Uniform(5, 10)
##
### Generate samples
sample.shape1 <- runif(n = nsamples, # sample from prior for shape1
                       min = 0,
                       max = 4)
sample.shape2 <- runif(n = nsamples, # sample from prior for shape2
                       min = 0,
                       max = 10)
prior.simstor <- rbetabinom.ab(n = nsamples,  # sample from beta-binomial using prior samples
                               size = param[["trials"]],
                               shape1 = sample.shape1,
                               shape2 = sample.shape2)
### Plot prior predictions
plot(NA,
     xlim = c(0, param[["trials"]]),
     ylim = c(0, .25),
     xlab = "Day of flowering",
     ylab = "Proportion",
     )
tab.prior <- table(prior.simstor)
points((tab.prior / nsamples) ~ c(0:(length(tab.prior) - 1)), type = "l", col = "blue")
points((tab.prior / nsamples) ~ c(0:(length(tab.prior) - 1)), type = "p", pch = 16, col = "blue", cex = 2)

## Load rstan library
library(rstan)
options(mc.cores = parallel::detectCores())

## Put simulated data into list for stan
sim.stan <- list(N = length(sim.stor),
                 G = sim.stor,
                 D = param[["trials"]])
## Fit model
fit.stan <- stan("Stan/betabinom.stan",
                 data = sim.stan,
                 iter = 2000,
                 warmup = 1000,
                 chains = 4,
                 verbose = TRUE)

### Compare to true values
t(param[c("s1", "s2")]) # true values
summary(fit.stan, pars = c("shape1", "shape2"))$summary

## Extract samples from fitted model
extract.stan <- extract(fit.stan)

## Bonus: Check quantiles of extracted samples against summary
quantile(x = extract.stan$shape1, probs = c(0.025, 0.975))
quantile(x = extract.stan$shape2, probs = c(0.025, 0.975))
summary(fit.stan, pars = c("shape1", "shape2"))$summary

## Plot simulated data with predictions
plot(NA,
     xlim = c(0, param[["trials"]]),
     ylim = c(0, .5),
     xlab = "Day of flowering",
     ylab = "Proportion",
     )
tab.sim <- table(sim.stor)
points((tab.sim / nsamples) ~ c(0:(length(tab.sim) - 1)), type = "l", col = rgb(0, 0, 1, alpha = .4))
points((tab.sim / nsamples) ~ c(0:(length(tab.sim) - 1)), type = "p", pch = 16, col = rgb(0, 0, 1, alpha = .4), cex = 1.5)
tab.pred <- table(extract.stan$G_new)
points((tab.pred / sum(tab.pred)) ~ c(0:(length(tab.pred) - 1)), type = "l", col = rgb(1, 0, 0, alpha = .4))
points((tab.pred / sum(tab.pred)) ~ c(0:(length(tab.pred) - 1)), type = "p", pch = 16, col = rgb(1, 0, 0, alpha = .4), cex = 1.5)
legend("topright", c("Simulated data", "Predictions"), pch = 16, bty = "n", inset = 0.03, cex = 1.5,
       col = c(rgb(0, 0, 1, alpha = .4), rgb(1, 0, 0, alpha = .4)))

##
## Part 3: Germination Data and Bayesian inference
##

## Read Dan's germination data
##
## Hesperis matronalis subset (forcing, no chilling)
germ <- read.csv("germination.csv", header = TRUE)

## Plot data
plot(NA,
     xlim = c(0, max(germ$Day)),
     ylim = c(0, .6),
     xlab = "Day of flowering",
     ylab = "Proportion",
     )
tab.germ <- table(germ$Day)
points((tab.germ / sum(tab.germ)) ~ c(0:(length(tab.germ) - 1)), type = "l", col = "blue")
points((tab.germ / sum(tab.germ)) ~ c(0:(length(tab.germ) - 1)), type = "p", pch = 16, col = "blue", cex = 1.5)
legend("topright", c(expression(italic("Hesperis matronalis"))), pch = 16, col = "blue", cex = 1.5, bty = "n")

## Put simulated data into list for stan
germ.stan <- list(N = nrow(germ),
                 G = germ$Day,
                 D = max(germ$Day))
## Fit model
fit.stan <- stan("Stan/betabinom2.stan",
                 data = germ.stan,
                 iter = 4000,
                 warmup = 2000,
                 chains = 1,
                 verbose = TRUE,
                 control = list(adapt_delta = 0.99))
## Something wrong here

## Check pairs plot
pairs(fit.stan)
