### Started 17 April 2020 by Cat
## New fake data and prior predictive checks
## Based off of Rethinking Chapter 4
## Using workflow from https://rpubs.com/adrbart/random_slope_simulation

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Do some flagging to try all versions of the simulations

# Load Libraries
library(lme4)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")

mean(ws$gdd_bb, na.rm=TRUE) ## 300
mean(hobo$gdd_bb, na.rm=TRUE) ## 250

set.seed(12221)


use.hobo = FALSE ### We expect less species variation using weather station data, so if use.hobo=TRUE, then sigma will be loaded on overall error not on species
use.urban = FALSE
use.provenance = TRUE

if(use.urban==TRUE & use.provenance==TRUE){
  print("Error was made in flags!! Adjust accordingly!")
}

if(use.provenance==FALSE & use.urban==FALSE){
  print("Error was made in flags!! Adjust accordingly!")
}

intercept = 300 ## We want to keep this the same between methods for now and just look into where the sigma is going

nsp = 20
ntot = 200
nsamples = 1000


####################################################################################################
if (use.hobo==FALSE & use.urban==TRUE & use.provenance==FALSE){

#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = nsp # number of species
ntot = ntot # numbers of obs per species. 

sample_a <- list(site.env = rbinom(nsamples, 1, 0.5))

model.parameters <- list(intercept = intercept,
                         urban.coef = -150)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})
mm <- model.matrix(~env.samples)
#mm <- mm[,-2]

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef"), collapse = "|"), x = names(model.parameters))
# Generate random parameters (by species)

parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 100), ntot)}) ## change for interspecific variation, load sigma on species on intercept
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 100), ntot)}) ## change for interspecific variation, load sigma on species on urban paramter
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 200)}) ## high individual variation, load sigma on sigma, less interspecific variation

fakedata_ws_urb <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                    gdd = response, urban = env.samples[,1]))

write.csv(fakedata_ws_urb, file="output/fakedata_ws_urb.csv", row.names = FALSE)

#  5) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ urban + (urban|species), data=fakedata_ws_urb) ## Quick look looks good!
}


####################################################################################################
if (use.hobo==TRUE & use.urban==TRUE & use.provenance==FALSE ){
  
  #  1) Let's make the observations much higher than the actual data to build a good model.
  nsp = nsp # number of species
  ntot = ntot # numbers of obs per species. 
  
  sample_a <- list(site.env = rbinom(nsamples, 1, 0.5))
  
  model.parameters <- list(intercept = intercept,
                           urban.coef = -150)
  
  #  2) Now, we will make varying intercepts
  env.samples <- sapply(sample_a, FUN = function(x){
    sample(x, size = nsp * ntot, replace = TRUE)})
  mm <- model.matrix(~env.samples)
  #mm <- mm[,-2]
  
  #  4) We need to make a random intercept model for each species
  parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
  # Which parameters are random?
  random.regex <- grep(pattern = paste(c("intercept", "urban.coef"), collapse = "|"), x = names(model.parameters))
  # Generate random parameters (by species)
  
  parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 200), ntot)}) ## change for interspecific variation, load sigma on species on intercept
  parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 100), ntot)}) ## change for interspecific variation, load sigma on species on urban paramter
  # Calculate response
  response <- sapply(1:nrow(env.samples), FUN = function(x){
    rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 100)}) ## high individual variation, load sigma on sigma, less interspecific variation
  
  fakedata_hl_urb <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                                   gdd = response, urban = env.samples[,1]))
  
  write.csv(fakedata_hl_urb, file="output/fakedata_hl_urb.csv", row.names = FALSE)
  
  #  5) Let's do a quick lmer model to test the fake data
  modtest <- lmer(gdd ~ urban + (urban|species), data=fakedata_hl_urb) ## Quick look looks good!
}


####################################################################################################
if (use.hobo==FALSE & use.urban==FALSE & use.provenance==TRUE ){
  
  #  1) Let's make the observations much higher than the actual data to build a good model.
  nsp = nsp # number of species
  ntot = ntot # numbers of obs per species. 
  
  sample_a <- list(prov.env = rnorm(nsamples, 45, 10))
  
  model.parameters <- list(intercept = intercept,
                           prov.coef = -100)
  
  #  2) Now, we will make varying intercepts
  env.samples <- sapply(sample_a, FUN = function(x){
    sample(x, size = nsp * ntot, replace = TRUE)})
  mm <- model.matrix(~env.samples)
  #mm <- mm[,-2]
  
  #  4) We need to make a random intercept model for each species
  parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
  # Which parameters are random?
  random.regex <- grep(pattern = paste(c("intercept", "prov.coef"), collapse = "|"), x = names(model.parameters))
  # Generate random parameters (by species)
  
  parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 100), ntot)}) ## change for interspecific variation, load sigma on species on intercept
  parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 100), ntot)}) ## change for interspecific variation, load sigma on species on urban paramter
  # Calculate response
  response <- sapply(1:nrow(env.samples), FUN = function(x){
    rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 200)}) ## high individual variation, load sigma on sigma, less interspecific variation
  
  fakedata_ws_prov <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                                   gdd = response, provenance = env.samples[,1]))
  
  write.csv(fakedata_ws_prov, file="output/fakedata_ws_prov.csv", row.names = FALSE)
  
  #  5) Let's do a quick lmer model to test the fake data
  modtest <- lmer(gdd ~ provenance + (provenance|species), data=fakedata_ws_prov) ## Quick look looks good!
}


####################################################################################################
if (use.hobo==TRUE & use.urban==FALSE & use.provenance==TRUE ){
  
  #  1) Let's make the observations much higher than the actual data to build a good model.
  nsp = nsp # number of species
  ntot = ntot # numbers of obs per species. 
  
  sample_a <- list(prov.env = rnorm(nsamples, 45, 10))
  
  model.parameters <- list(intercept = intercept,
                           prov.coef = -100)
  
  #  2) Now, we will make varying intercepts
  env.samples <- sapply(sample_a, FUN = function(x){
    sample(x, size = nsp * ntot, replace = TRUE)})
  mm <- model.matrix(~env.samples)
  #mm <- mm[,-2]
  
  #  4) We need to make a random intercept model for each species
  parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)
  # Which parameters are random?
  random.regex <- grep(pattern = paste(c("intercept", "prov.coef"), collapse = "|"), x = names(model.parameters))
  # Generate random parameters (by species)
  
  parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 200), ntot)}) ## change for interspecific variation, load sigma on species on intercept
  parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 100), ntot)}) ## change for interspecific variation, load sigma on species on urban paramter
  # Calculate response
  response <- sapply(1:nrow(env.samples), FUN = function(x){
    rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 100)}) ## high individual variation, load sigma on sigma, less interspecific variation
  
  fakedata_hl_prov <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                                        gdd = response, provenance = env.samples[,1]))
  
  write.csv(fakedata_hl_prov, file="output/fakedata_hl_prov.csv", row.names = FALSE)
  
  #  5) Let's do a quick lmer model to test the fake data
  modtest <- lmer(gdd ~ provenance + (provenance|species), data=fakedata_hl_prov) ## Quick look looks good!
}
