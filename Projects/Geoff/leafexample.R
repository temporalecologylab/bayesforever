## Set seed
set.seed(107844)

## Generate leaf type
leaf <- rbinom(n = 2000, size = 1, prob = .3)
## Conditional sampling
data <- dim(length(leaf))
for(i in 1:length(leaf)){
    if(leaf[i] == 0){
        data[i] <- rnorm(n = 1, mean = 10, sd = sqrt(5))
    } else{
        data[i] <- rnorm(n = 1, mean = 25, sd = sqrt(2))
    }
}

library(rstan)
options(mc.cores = parallel::detectCores())

## Put simulated data into list for stan
data.stan <- list(N = length(data),
                  G = data)

## Fit model
fit.stanmarg <- stan("leaf-marg.stan",
                 data = data.stan,
                 iter = 4000,
                 warmup = 2000,
                 chains = 4)

summary(fit.stanmarg, pars = c("mu_fresh", "mu_dried", "sigma_fresh", "sigma_dried", "p"))$summary

extract.stanmarg <- extract(fit.stanmarg)
