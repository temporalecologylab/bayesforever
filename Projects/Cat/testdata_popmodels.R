## Started 18 September 2020 ##
## By Cat, help from many ##
# Let's make some test data for our rstan pop models


library(rstan)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

set.seed(12221)


# setwd
# Set working directory: 
setwd("~/Documents/git/bayes2020/Projects/Cat")

#--------------------------------------
# Set up the range model first - we can change to whatever cue or cues we choose to use in the future
# resp ~ force[sp] + force[pop] + sigma_y 
# force[sp] and force[pop] are standard hierarhical parameters to try and compare inter- vs intraspecific variation in the forcing cue


#### New attempt at building fake data: based on https://rpubs.com/kaz_yos/stan-multi-2
## Data generation
set.seed(12221)

nsp = 10
npop = 10
nobs = 10
ntot = nsp * npop * nobs

## Budburst intercept
intercept   <- 50
sigma_y <- 3
## Forcing slope
b_force    <- -10
sigma_b <- 3
e_bforce <- rnorm(n = ntot, mean = 0, sd = sigma_b)

## Level-1 errors
sigma_a   <- 3
e_int <- rnorm(n = ntot, mean = 0, sd = sigma_a)

## Level-2 errors (species)
sigma_pop <- 10
e_pop <- rnorm(n = npop * nsp, mean = 0, sd = sigma_pop)
sigma_bpop <- 5
e_bpop <- rnorm(n = npop * nsp, mean = 0, sd = sigma_bpop)

## Level-3 errors (population)
sigma_sp <- 3
e_sp <- rnorm(n = nsp, mean = 0, sd = sigma_sp)
sigma_bsp <- 2
e_bsp <- rnorm(n = nsp, mean = 0, sd = sigma_bsp)

## Varying intercepts
## Level 2 (repeat for level 3)
a_pop   <- rep(intercept + e_pop, each = npop)
## Level 3
a_sppop  <- rep(a_sp + e_sp, each = npop)

## Varying slopes
## Level 2 (repeat for level 3)
b_forcesp   <- rep(b_force + e_bpop, nsp)
## Level 3
b_forcesppop  <- rep(b_forcesp + e_bsp, nsp)

## Predictor
force   <- rnorm(n = ntot, 5, 2)

## Outcome
resp    <- a_sppop + b_forcesppop*force + rnorm(ntot, 0, sigma_y)

simpheno <- data.frame(species=rep(1:nsp, each=nobs), pop=rep(1:npop, each=nobs*nsp),
                          force=force, resp=resp)

#library(lme4)
#modtest <- lmer(resp ~ force + (force|species/pop), data=simpheno) ## Quick look looks good!

#write.csv(simpheno, file="~/Desktop/testing123.csv", row.names=FALSE)

N <- length(simpheno$resp)
forcepop <- list(y = simpheno$resp,
                 N = N, 
                 n_sp = nsp, 
                 n_pop = npop,
                 sp = simpheno$species,
                 pop = simpheno$pop,
                 N = N, 
                 force = simpheno$force)



# Try to run the Stan model 
#library(brms)
#check <- brm(resp ~ force + (force|species/pop), data=simpheno, warmup = 4000, iter = 5000, 
 #            control = list(max_treedepth = 15, adapt_delta = 0.99))

forcepopfit <- stan(file = "stan/nointer_3levelwpop_classroomexamp.stan", data = forcepop, warmup = 4000, iter = 5000,
                    chains = 4,  control=list(max_treedepth = 15, adapt_delta=0.99)) 


modelhere <- forcepopfit 
mod.sum <- summary(modelhere)$summary
mod.sum[grep("b_force", rownames(mod.sum)),]
mod.sum[grep("sigma", rownames(mod.sum)),] 

launch_shinystan(forcepopfit)

#save(jointfit, file="output/stan/jointlatphoto.Rda")

if(FALSE){
### Let's just look at the data a bit to make sure it looks okay...
library(ggplot2)
library(egg)

spp <- ggplot(simpheno, aes(y=resp, x=force, col=as.factor(species), group=as.factor(species))) + 
  geom_point() + geom_smooth(method="lm") + theme_classic()

pop <- ggplot(simpheno, aes(y=resp, x=force, col=as.factor(pop), group=as.factor(pop))) + 
  geom_point() + geom_smooth(method="lm") + theme_classic()

quartz()
ggarrange(spp, pop)


anova(lm(resp~force + species + pop, data=simpheno))

real <- bb.stan.here

library(brms)
modpop3.force.real <- brm(formula = resp ~ force + ( force |latbi/pophere), 
                           data = real,iter=1000,warmup=500,chains=4, 
                     control=list(adapt_delta = 0.95, max_treedepth=12))

save(modpop3.force.real, file="~/Desktop/m3l_ni_brms_realdata.Rdata")
}
