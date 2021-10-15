### Started 9 March 2020 by Cat
## New fake data and prior predictive checks
## Based off of Rethinking Chapter 4
## Using workflow from https://rpubs.com/adrbart/random_slope_simulation

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(lme4)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

#ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
#hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")

# We will focus on Harvard Forest to start
#hf.ws <- ws[(ws$type=="Harvard Forest"),]
#hf.hobo <- hobo[(hobo$type=="Harvard Forest"),]

#mean(hf.ws$gdd_lo, na.rm=TRUE) ## 877.41
#mean(hf.hobo$gdd_lo, na.rm=TRUE) ## 541.2
## Big difference between the two means!! Without looking at the data too much, I will assume the hobo logger data
# has less variance than the weather station data

set.seed(12221)


### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(site.env = rbinom(1000, 1, 0.5),
                 method.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 400,
                         urban.coef = -50,
                         method.coef = -100)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})
mm <- model.matrix(~env.samples)
#mm <- mm[,-2]

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)

# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef", "method.coef"), collapse = "|"), x = names(model.parameters))

# Generate random parameters (by species)
parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 50), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 10), ntot)})
parameters.temp[, 3] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[3]]], sd = 20), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 20)})

testdata_urbmethod <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                    gdd = response, urban = env.samples[,1], method = env.samples[,2]))

write.csv(testdata_urbmethod, file="output/testdata_urbmethod.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ urban + method + (urban + method|species), data=testdata_urbmethod) ## Quick look looks good!



####################### NOW LET'S ADD AN INTERACTION!! #####################
### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(urban.env = rbinom(1000, 1, 0.5),
                 method.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 400,
                         urban.coef = -50,
                         method.coef = -100,
                         urbanxmethod = 10)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})

# Determine which environmental variables interact
intrxnname <- names(model.parameters)[4] # interaction terms
names.temp <- gsub("x", "|", intrxnname) # remove text to align with colnames
env.pairs <- sapply(1:length(names.temp), FUN = function(X){
  grep(pattern = names.temp[X], x = colnames(env.samples))
})
# Add these interactions (product) to env.samples        
env.interactions <- sapply(1:ncol(env.pairs), FUN = function(X){
  apply(env.samples[, env.pairs[, X]], MARGIN = 1, FUN = prod)
})
env.samples2 <- cbind(env.samples, env.interactions)
# Create model matrix
mm <- model.matrix(~env.samples2)

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)

# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef", "method.coef", "urbanxmethod"), collapse = "|"), x = names(model.parameters))

# Generate random parameters (by species)
parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 50), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 10), ntot)})
parameters.temp[, 3] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[3]]], sd = 20), ntot)})
parameters.temp[, 4] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[4]]], sd = 3), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 20)})

testdata_urbmethod_intrxn <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                       gdd = response, urban = env.samples[,1], method = env.samples[,2]))

write.csv(testdata_urbmethod_intrxn, file="output/testdata_urbmethod_intrxn.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ urban + method + urban*method + (urban + method + urban*method|species), data=testdata_urbmethod_intrxn) ## Quick look looks good!


############################################################################
####################### CHILL TIME WITH AN INTERACTION!! ###################
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(urban.env = rbinom(1000, 1, 0.5),
                 method.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 650,
                         urban.coef = 800,
                         method.coef = -150,
                         urbanxmethod = -30)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})

# Determine which environmental variables interact
intrxnname <- names(model.parameters)[4] # interaction terms
names.temp <- gsub("x", "|", intrxnname) # remove text to align with colnames
env.pairs <- sapply(1:length(names.temp), FUN = function(X){
  grep(pattern = names.temp[X], x = colnames(env.samples))
})
# Add these interactions (product) to env.samples        
env.interactions <- sapply(1:ncol(env.pairs), FUN = function(X){
  apply(env.samples[, env.pairs[, X]], MARGIN = 1, FUN = prod)
})
env.samples2 <- cbind(env.samples, env.interactions)
# Create model matrix
mm <- model.matrix(~env.samples2)

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)

# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "urban.coef", "method.coef", "urbanxmethod"), collapse = "|"), x = names(model.parameters))

# Generate random parameters (by species)
parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 150), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 50), ntot)})
parameters.temp[, 3] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[3]]], sd = 150), ntot)})
parameters.temp[, 4] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[4]]], sd = 50), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 600)})

testdata_chill_intrxn <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                              chill = response, urban = env.samples[,1], method = env.samples[,2]))

write.csv(testdata_chill_intrxn, file="output/testdata_chill_intrxn.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(chill ~ urban + method + urban*method + (urban + method + urban*method|species), data=testdata_chill_intrxn) ## Quick look looks good!


#################################################################################################
#################################    Now for PROVENANCE!   ########################################
#################################################################################################

#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 200 # numbers of obs per species. 

sample_a <- list(prov.env = rnorm(1000, 42.5, 5),
                 method.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 400,
                         prov.coef = -10,
                         method.coef = -100,
                         urbanxmethod = 5)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})

# Determine which environmental variables interact
intrxnname <- names(model.parameters)[4] # interaction terms
names.temp <- gsub("x", "|", intrxnname) # remove text to align with colnames
env.pairs <- sapply(1:length(names.temp), FUN = function(X){
  grep(pattern = names.temp[X], x = colnames(env.samples))
})
# Add these interactions (product) to env.samples        
env.interactions <- sapply(1:ncol(env.pairs), FUN = function(X){
  apply(env.samples[, env.pairs[, X]], MARGIN = 1, FUN = prod)
})
env.samples2 <- cbind(env.samples, env.interactions)
# Create model matrix
mm <- model.matrix(~env.samples2)

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nsp * ntot, byrow = TRUE)

# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "prov.coef", "method.coef", "provxmethod"), collapse = "|"), x = names(model.parameters))

# Generate random parameters (by species)
parameters.temp[, 1] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[1]]], sd = 50), ntot)})
parameters.temp[, 2] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[2]]], sd = 5), ntot)})
parameters.temp[, 3] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[3]]], sd = 20), ntot)})
parameters.temp[, 4] <- sapply(1:nsp, FUN = function(x){
  rep(rnorm(n = 1, mean = model.parameters[[random.regex[4]]], sd = 2), ntot)})
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 20)})

testdata_provmethod_intrxn <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                              gdd = response, provenance = env.samples[,1], method = env.samples[,2]))

write.csv(testdata_provmethod_intrxn, file="output/testdata_provmethod_intrxn.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ provenance + method + provenance*method + (provenance + method + provenance*method|species), data=testdata_provmethod_intrxn) ## Quick look looks good!

