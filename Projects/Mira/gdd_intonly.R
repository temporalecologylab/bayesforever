## vassal interphenophase with GDD simulated data. Started 8 Oct 2020 by Mira Garner.

# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/model/")
} else setwd("~/Documents/git/vassalphen/model/")

# setwd for class
#setwd("~/Documents/git/bayes2020/Projects/Mira/")


####---------------------------------------------------------------####

# Intercept only model - predict gdd for variety

# gdd = avar + error
# gdd ~ N(mu, sigma)
# mu = avar
# avar ~ N(mu.var, sig.var)
# sigma ~ N()

# pinot noir ver-mat ~555, syrah ver-mat ~880

## simulated data
param <- list(var.mean = 700,
              var.sigma = 60,
              gdd.sigma = 10)

## Number of varieties
nvarieties <- 100
## Number of observations per variety
nobs <- 8

## Generate the variety intercepts
variety.intercepts <- rnorm(n = nvarieties, mean = param[["var.mean"]], sd = param[["var.sigma"]])

## Storage for loop
stor <- data.frame()

## Generate the observations given the intercepts
for(i in 1:nvarieties){
    temp <- data.frame(variety = i,
                       gdd = rnorm(n = nobs, mean = variety.intercepts[i], sd = param[["gdd.sigma"]]))
    stor <- rbind(stor, temp)
}

dat <- cbind(stor, variety.intercepts)

library(ggplot2)
ggplot(stor, aes(x=variety, y=gdd, group=variety)) + geom_boxplot()


library(rstan)
library(rethinking)

# STAN model

#x <- stor$year
y <- stor$gdd
variety <- stor$variety
N <- nrow(stor)
Nv <- nvarieties

# data to stan
stan.data <- list(y=y, variety=variety, N=N, Nv=Nv)

modone <- stan("gddvassal.stan", data = stan.data, iter = 2000, warmup = 1000)

summo <- summary(modone)$summary
#summo[grep("mu_", rownames(summo)),] 
#summo[grep("s\\_", rownames(summo)),] 
#summo[grep("sigma_y", rownames(summo)),] 
precis(modone)

shinystan::launch_shinystan(modone)
