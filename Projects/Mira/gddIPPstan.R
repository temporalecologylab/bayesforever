## vassal interphenophases with GDD simulated data. Started 8 Oct 2020 by Mira Garner.

# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

library(rstan)
library(rethinking)

if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/model/")
} else setwd("~/Documents/git/vassalphen/model/")

# setwd for class
#setwd("~/Documents/git/bayes2020/Projects/Mira/")

# what does the model look like in math?
# gdd = avar + bvar(year) + error
# gdd ~ N(mu, sigma)
# mu = avar + bvar(year)
# avar ~ N(mu.a, sig.a)
# bvar ~ N(mu.b, sig.b)
# sigma ~ N()

# --------------- real dataset set up for stan ----------------#

source("../analyses/cleaning/mergeclimpheno.R")
# sourcing it from another file changes the wd - how can I avoid this
if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/model/")
} else setwd("~/Documents/git/vassalphen/model/")

# STAN model****only choose one event at a time!!!

# budburst to flowering
x <- bbfl$yr1980
y <- bbfl$gdd_bbfl
variety <- as.integer(as.factor(as.character(bbfl$variety)))
N <- nrow(bbfl)
Nv <- length(unique(bbfl$variety))

# flowering to veraison
x <- flvr$yr1980
y <- flvr$gdd_flvr
variety <- as.integer(as.factor(as.character(flvr$variety)))
N <- nrow(flvr)
Nv <- length(unique(flvr$variety))

# veraison to maturity
x <- vrmt$yr1980
y <- vrmt$gdd_vrmt
variety <- as.integer(as.factor(as.character(vrmt$variety)))
N <- nrow(vrmt)
Nv <- length(unique(vrmt$variety))

# budburst to maturity
x <- bbmt$yr1980
y <- bbmt$gdd_bbmt
variety <- as.integer(as.factor(as.character(bbmt$variety)))
N <- nrow(bbmt)
Nv <- length(unique(bbmt$variety))


#  ---------------- data to stan ----------------- #
gdd.stan.data <- list(y=y, x=x, variety=variety, N=N, Nv=Nv)

gddDuration <- stan("gddIPP.stan", data = gdd.stan.data, iter = 8000, warmup = 7000)

#precis(gddDuration)
summo <- summary(gddDuration)$summary
summo[grep("mu_", rownames(summo)),] 
summo[grep("s\\_", rownames(summo)),] 
summo[grep("sigma_y", rownames(summo)),]

modslopes <- summo[grep("b_", row.names(summo)), c(1,5:7)]
dfslopes <- as.data.frame(modslopes)
#dfslopes <- cbind(variety.names, dfslopes) #will this give the correct order?

shinystan::launch_shinystan(gddDuration)

# ----------------- Graphs ------------------ #
library(ggplot2)
library(bayesplot)
library(rstanarm)

quartz()

pairs(gddDuration, pars = c("mu_a", "mu_b", "s_avar", "s_bvar", "sigma_y"))

posterior <- as.matrix(gddDuration)
mcmc_areas(posterior, pars = "mu_a")
mcmc_areas(posterior, pars = "mu_b")
mcmc_areas(posterior, pars = "s_avar")
mcmc_areas(posterior, pars = "s_bvar")
mcmc_areas(posterior, pars = "sigma_y")

# prior distributions
hist(rnorm(500,700,50)) #mu_a
hist(rnorm(500,0,0.5)) #mu_b
hist(rnorm(500,80,20)) #s_avar
hist(rnorm(500,0,0.5)) #s_bvar
hist(rnorm(500,0,50)) #sigma_y

ggplot(dfslopes, aes(x=row.names(dfslopes), y=mean)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# --------------- using simulated dataset ----------------- #
#x <- dat.sim$year
#y <- dat.sim$gdd
#variety <- dat.sim$variety
#N <- nrow(dat.sim)
#Nv <- nvarieties

# ---------- cut varieties in half ------------- #
# cutting number of varieties in half to test if variation among varieties is changing parameter estimates
#variety.names <- unique(mini$Group.2)
#halfv <- variety.names[1:45]
#half.mini <- mini[which(mini$Group.2 %in% halfv),]
