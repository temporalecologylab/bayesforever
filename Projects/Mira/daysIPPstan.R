## stan model for bayesian class. Vassal interphenophases

# housekeeping
#rm(list = ls())
options(stringsAsFactors = FALSE)

library(rstan)
library(shinystan)
library(ggplot2)
library(bayesplot)

#set working directory
if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/model/")
} else setwd("~/Documents/git/vassalphen/model/")


# -------------- real dataset setup for stan ---------------- #
source("../analyses/cleaning/mergeclimpheno.R")
# sourcing it from another file changes the wd - how can I avoid this
if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/model/")
} else setwd("~/Documents/git/vassalphen/model/")

# STAN model****only choose one event at a time!!!

# budburst to flowering
x <- bbfl$yr1980
y <- bbfl$days_bbfl
variety <- as.integer(as.factor(as.character(bbfl$variety)))
N <- nrow(bbfl)
Nv <- length(unique(bbfl$variety))

# flowering to veraison
x <- flvr$yr1980
y <- flvr$days_flvr
variety <- as.integer(as.factor(as.character(flvr$variety)))
N <- nrow(flvr)
Nv <- length(unique(flvr$variety))

# veraison to maturity
x <- vrmt$yr1980
y <- vrmt$days_vrmt
variety <- as.integer(as.factor(as.character(vrmt$variety)))
N <- nrow(vrmt)
Nv <- length(unique(vrmt$variety))

# budburst to maturity
x <- bbmt$yr1980
y <- bbmt$days_bbmt
variety <- as.integer(as.factor(as.character(bbmt$variety)))
N <- nrow(bbmt)
Nv <- length(unique(bbmt$variety))

# ------------------ data to stan ------------------ #
stan.data <- list(x=x, y=y, variety=variety, N=N, Nv=Nv)

daysDuration <- stan("daysIPP.stan", data = stan.data, iter = 8000, warmup = 6000)

summy <- summary(daysDuration)$summary
summy[grep("mu_", rownames(summy)),] 
summy[grep("s\\_", rownames(summy)),] 
summy[grep("sigma_y", rownames(summy)),] 
#precis(bbfl)

pairs(daysDuration, pars = c("mu_a", "mu_b", "s_avar", "s_bvar", "sigma_y"))

shinystan::launch_shinystan(daysDuration)



# --------------- read in simulated data ------------------ #
#sim <- read.csv("y3.dat.csv", header=T)
#head(sim)

# for simulated data # STAN model setup
#x <- sim$year
#y <- sim$fin3.dat
#variety <- sim$variety
#N <- nrow(sim)
#Nv <- length(unique(sim$variety))

#end
