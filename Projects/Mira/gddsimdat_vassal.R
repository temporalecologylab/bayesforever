## vassal interphenophases with GDD simulated data. Started 8 Oct 2020 by Mira Garner.

# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/model/")
} else setwd("~/Documents/git/vassalphen/model/")

# setwd for class
#setwd("~/Documents/git/bayes2020/Projects/Mira/")


####---------------------------------------------------------------####

# Slope and intercept model - predict gdd for variety

# gdd = avar + bvar(year) + error
# gdd ~ N(mu, sigma)
# mu = avar + bvar(year)
# avar ~ N(mu.a, sig.a)
# bvar ~ N(mu.b, sig.b)
# sigma ~ N()

# pinot noir ver-mat ~555, syrah ver-mat ~880

## simulated data

## Number of varieties
nvarieties <- 100
## Number of observations per variety
#nobs <- 20


# list of years
# data in d2 starts 1956, ends 2013
year_0 <- 1980 #hinge year
sigma_yr <- 5
yr_per_var <- round(runif(nvarieties, 5, 57)) #number of years of data for each variety
variety <- rep(1:nvarieties, yr_per_var) # replicate each variety name the number of years of observations (yr_per_sp)
len.yr <- length(variety)
year <- rep(NA, len.yr)
for (v in 1:nvarieties){
  year[variety == v] <- rev(runif(1, 1961, 2013) - 1:(yr_per_var[v])) - year_0
}
dat.yrs <- data.frame(variety, year)

dat.sim <- dat.yrs #so I keep the original

# parameters for single slope
#param <- list(mu.a = 700,
#              sig.a = 60,
#              single.b = 0.01,
#              gdd.sigma = 10)


# multislope parameters
param <- list(mu.a = 700,
              sig.a = 60,
              mu.b = 0,
              sig.b = 0.5,
              gdd.sigma = 10)

## Generate the variety intercepts
variety.intercepts <- rnorm(n = nvarieties, mean = param[["mu.a"]], sd = param[["sig.a"]])
# list of alpha repeating for num of years of observations
dat.sim$a.var <- rep(variety.intercepts, yr_per_var)

# generate slope values
#dat.sim$b.var <- param[["single.b"]] #single slope version
#nultislope version
variety.slopes <- rnorm(n = nvarieties, mean = param[["mu.b"]], sd = param[["sig.b"]])
# list of beta repeating for num of years of observations
dat.sim$b.var <- rep(variety.slopes, yr_per_var)

# generate observations
for(i in 1:length(dat.sim$variety)){
  mu <- dat.sim$a.var[i] + dat.sim$b.var[i]*dat.sim$year[i]
  gdd <- rnorm(n = 1, mean = mu, sd = param[["gdd.sigma"]])
  dat.sim$gdd[i] <- gdd
}


# plotz
library(ggplot2)
ggplot(dat.sim, aes(x=year, y=gdd, group=variety, color = variety)) + 
  geom_line()



# STAN model

library(rstan)
library(rethinking)

# using simulated dataset
x <- dat.sim$year
y <- dat.sim$gdd
variety <- dat.sim$variety
N <- nrow(dat.sim)
Nv <- nvarieties

# using real dataset

source("../analyses/cleaning/mergeclimpheno.R")
# sourcing it from another file changes the wd - how can I avoid this
if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/model/")
} else setwd("~/Documents/git/vassalphen/model/")

x <- mini$Group.1
y <- mini$vrmt
variety <- as.integer(as.factor(as.character(mini$Group.2)))
N <- nrow(mini)
Nv <- length(unique(mini$Group.2))

# data to stan
stan.data <- list(y=y, x=x, variety=variety, N=N, Nv=Nv)

modone <- stan("gddvassal.stan", data = stan.data, iter = 3000, warmup = 2000)

summo <- summary(modone)$summary
#summo[grep("mu_", rownames(summo)),] 
#summo[grep("s\\_", rownames(summo)),] 
#summo[grep("sigma_y", rownames(summo)),] 
precis(modone)

shinystan::launch_shinystan(modone)
