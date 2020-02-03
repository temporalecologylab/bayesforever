### Started 29 January 2020 by Cat
## New fake data and prior predictive checks
## Based off of Rethinking Chapter 4

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
## Compare GDDs between hobo loggers and weather station data
# 1) GDDlo ~ 1 + (1|species) - do once for HF weather station, once for hobo logger and repeat for Arboretum
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

## Let's start with Question 1 first...
#library(rethinking)
library(RColorBrewer)
library(lme4)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

ws <- read.csv("output/clean_gdd_bbanddvr.csv")
hobo <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")

# We will focus on Harvard Forest to start
hf.ws <- ws[(ws$type=="Harvard Forest"),]
hf.hobo <- hobo[(hobo$type=="Harvard Forest"),]

mean(hf.ws$gdd_lo, na.rm=TRUE) ## 877.41
mean(hf.hobo$gdd_lo, na.rm=TRUE) ## 541.2
## Big difference between the two means!! Without looking at the data too much, I will assume the hobo logger data
# has less variance than the weather station data

### WEATHER STATION DATA MODEL SIMULATION
### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 50 # numbers of obs per species. 

sample_a <- list(int.env = rnorm(1000, 700, 200))

#  2) Now, we will make varying intercepts
int.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})

#  4) We need to make a random intercept model for each species
baseinter <- list(intercept = mean(int.samples))
baseinter.mat <- matrix(unlist(baseinter), ncol = length(baseinter), nrow = nsp * ntot, byrow = TRUE)
  ## From Geoff's simulate-linear.R code in OSPREE: Which parameters are random?
random.inter <- grep(pattern = paste("intercept", collapse = "|"), x = names(baseinter))
# Generate random intercepts (by species) 
for(i in 1:length(random.inter)){
  baseinter.mat[, i] <- sapply(1:nsp, FUN = function(X){
    rep(rnorm(n = 1, mean = baseinter[[random.inter[i]]], sd = 200), ntot)})}

#  5) Calculate response
response <- sapply(1:nrow(int.samples), FUN = function(x){
    rnorm(n = 1, mean = baseinter.mat[x, ], sd = 200)})
#  6) Make a dataframe of fake data
fakedata_ws <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                 gdd = response))

write.csv(fakedata_ws, file="output/fakedata_ws.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ 1 + (1|species), data=fakedata_ws) ## Quick look looks good!

my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 2)
my.pch <- rep(15:16, each=10)
plot(gdd ~ species, col=my.pal[species], pch=my.pch[species], data = fakedata_ws)
abline(h=mean(fakedata_ws$gdd), lwd=3)

plot(density(fakedata_ws$gdd))
abline(v = mean(fakedata_ws$gdd), lwd = 2, col = "blue")

## PRIOR PREDICTIVE CHECK time!!
# Now I will follow the workflow from the Gabry et al., 2019 paper
## Using vague priors
nsims <- length(fakedata_ws$species)
alpha <- rnorm(20, 700, 200)
sigma <- runif(20, 0, 200)

data1 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim = alpha[fakedata_ws$species] +
    rnorm(nsims, mean = 0, sd = sigma)
)

xysim_labs <- labs(
  x = "Observed GDD",
  y = "Simulated data"
)

theme_set(bayesplot::theme_default(base_size = 18))
theme_update(axis.text = element_text(size = 20))

ggplot(data1, aes(x = gdd, y = sim)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(0, 1500), ylim=c(0,1500)) + geom_abline(intercept=0, slope=1)

## Using weakly informative priors
alpha2 <- rnorm(20, 0, 1)
sigma2 <- runif(20, 0, 1)

data2 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim2 = alpha2[fakedata_ws$species] +
    rnorm(nsims, mean = 0, sd = sigma)
)

ggplot(data2, aes(x = gdd, y = sim2)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(0, 1500), ylim=c(0,1500)) + geom_abline(intercept=0, slope=1)

data3 <- data.frame(
  gdd = fakedata_ws$gdd, 
  wip = data2$sim, 
  vague = data1$sim
)
ggplot(data3, aes(x=gdd, y=wip)) + 
  geom_point(alpha = 0.1) + 
  geom_point(
    aes(y = vague), 
    color = "red", 
    alpha = 0.1
  ) + 
  xysim_labs + coord_cartesian(xlim=c(0, 1500), ylim=c(0,1500)) + geom_abline(intercept=0, slope=1)


##### NOW FOR HOBO LOGGER FAKE DATA!
### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 50 # numbers of obs per species. 

sample_a <- list(int.env = rnorm(1000, 600, 50))

#  2) Now, we will make varying intercepts
int.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nsp * ntot, replace = TRUE)})

#  4) We need to make a random intercept model for each species
baseinter <- list(intercept = mean(int.samples))
baseinter.mat <- matrix(unlist(baseinter), ncol = length(baseinter), nrow = nsp * ntot, byrow = TRUE)
## From Geoff's simulate-linear.R code in OSPREE: Which parameters are random?
random.inter <- grep(pattern = paste("intercept", collapse = "|"), x = names(baseinter))
# Generate random intercepts (by species) 
for(i in 1:length(random.inter)){
  baseinter.mat[, i] <- sapply(1:nsp, FUN = function(X){
    rep(rnorm(n = 1, mean = baseinter[[random.inter[i]]], sd = 50), ntot)})}

#  5) Calculate response
response <- sapply(1:nrow(int.samples), FUN = function(x){
  rnorm(n = 1, mean = baseinter.mat[x, ], sd = 50)})
#  6) Make a dataframe of fake data
fakedata_hl <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                gdd = response))

write.csv(fakedata_hl, file="output/fakedata_hl.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ 1 + (1|species), data=fakedata_ws) ## Quick look looks good!

my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 2)
my.pch <- rep(15:16, each=10)
plot(gdd ~ species, col=my.pal[species], pch=my.pch[species], data = fakedata_ws)
abline(h=mean(fakedata_ws$gdd), lwd=3)

plot(density(fakedata_ws$gdd))
abline(v = mean(fakedata_ws$gdd), lwd = 2, col = "blue")

## PRIOR PREDICTIVE CHECK time!!
# Now I will follow the workflow from the Gabry et al., 2019 paper
## Using vague priors
nsims <- length(fakedata_ws$species)
alpha <- rnorm(20, 600, 50)
sigma <- runif(20, 0, 50)

data1 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim = alpha[fakedata_ws$species] +
    rnorm(nsims, mean = 0, sd = sigma)
)

ggplot(data1, aes(x = gdd, y = sim)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(250, 1000), ylim=c(250,1000)) + geom_abline(intercept=0, slope=1)

## Using weakly informative priors
alpha2 <- rnorm(20, 0, 1)
sigma2 <- runif(20, 0, 1)

data2 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim2 = alpha2[fakedata_ws$species] +
    rnorm(nsims, mean = 0, sd = sigma)
)

ggplot(data2, aes(x = gdd, y = sim2)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(0, 1000), ylim=c(0,1000)) + geom_abline(intercept=0, slope=1)

data3 <- data.frame(
  gdd = fakedata_ws$gdd, 
  wip = data2$sim, 
  vague = data1$sim
)
ggplot(data3, aes(x=gdd, y=wip)) + 
  geom_point(alpha = 0.1) + 
  geom_point(
    aes(y = vague), 
    color = "red", 
    alpha = 0.1
  ) + 
  xysim_labs + coord_cartesian(xlim=c(0, 1000), ylim=c(0,1000)) + geom_abline(intercept=0, slope=1)


####################### NOW LET'S TRY TO ADDRESS QUESTION #2
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 100 # numbers of obs per species. 

sample_a <- list(site.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 700,
                         urban.coef = 50)

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
for(i in 1:length(random.regex)){
  parameters.temp[, i] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[i]]], sd = 200), ntot)})}
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
    rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 200)})

fakedata_ws_urb <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                gdd = response, urban = env.samples[,1]))

write.csv(fakedata_ws_urb, file="output/fakedata_ws_urb.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ urban + (urban|species), data=fakedata_ws) ## Quick look looks good!

my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 2)
my.pch <- rep(15:16, each=10)
plot(gdd ~ species, col=my.pal[species], pch=my.pch[species], data = fakedata_ws)
abline(h=mean(fakedata_ws$gdd), lwd=3)

plot(density(fakedata_ws$gdd))
abline(v = mean(fakedata_ws$gdd), lwd = 2, col = "blue")

## PRIOR PREDICTIVE CHECK time!!
# Now I will follow the workflow from the Gabry et al., 2019 paper
## Using vague priors
nsims <- length(fakedata_ws$species)
alpha <- rnorm(20, 700, 200)
beta <- rnorm(20, 50, 10)
sigma <- runif(20, 0, 50)

data1 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim = alpha[fakedata_ws$species] + (beta + beta[fakedata_ws$species]) +
    rnorm(nsims, mean = 0, sd = sigma)
)

ggplot(data1, aes(x = gdd, y = sim)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(250, 1000), ylim=c(250,1000)) + geom_abline(intercept=0, slope=1)

## Using weakly informative priors
nsims <- length(fakedata_ws$species)
alpha <- rnorm(20, 0, 1)
beta <- rnorm(20, 0, 1)
sigma <- runif(20, 0, 1)

data2 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim = alpha[fakedata_ws$species] + (beta + beta[fakedata_ws$species]) +
    rnorm(nsims, mean = 0, sd = sigma)
)

ggplot(data2, aes(x = gdd, y = sim2)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(0, 1000), ylim=c(0,1000)) + geom_abline(intercept=0, slope=1)

data3 <- data.frame(
  gdd = fakedata_ws$gdd, 
  wip = data2$sim, 
  vague = data1$sim
)
ggplot(data3, aes(x=gdd, y=wip)) + 
  geom_point(alpha = 0.1) + 
  geom_point(
    aes(y = vague), 
    color = "red", 
    alpha = 0.1
  ) + 
  xysim_labs + coord_cartesian(xlim=c(0, 1500), ylim=c(0,1500)) + geom_abline(intercept=0, slope=1)


######################################################################
# Compare urban effect using weather station data and NOW HOBO LOGGER DATA
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nsp = 20 # number of species
ntot = 100 # numbers of obs per species. 

sample_a <- list(site.env = rbinom(1000, 1, 0.5))

model.parameters <- list(intercept = 600,
                         urban.coef = 50)

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
for(i in 1:length(random.regex)){
  parameters.temp[, i] <- sapply(1:nsp, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[i]]], sd = 50), ntot)})}
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 50)})

fakedata_hl_urb <- cbind(data.frame(species = as.vector(sapply(1:nsp, FUN = function(x) rep(x, ntot))),
                                gdd = response, urban = env.samples[,1]))

write.csv(fakedata_hl_urb, file="output/fakedata_hl_urb.csv", row.names = FALSE)

#  7) Let's do a quick lmer model to test the fake data
modtest <- lmer(gdd ~ urban + (urban|species), data=fakedata_ws) ## Quick look looks good!

my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 2)
my.pch <- rep(15:16, each=10)
plot(gdd ~ species, col=my.pal[species], pch=my.pch[species], data = fakedata_ws)
abline(h=mean(fakedata_ws$gdd), lwd=3)

plot(density(fakedata_ws$gdd))
abline(v = mean(fakedata_ws$gdd), lwd = 2, col = "blue")

## PRIOR PREDICTIVE CHECK time!!
# Now I will follow the workflow from the Gabry et al., 2019 paper
## Using vague priors
nsims <- length(fakedata_ws$species)
alpha <- rnorm(20, 600, 50)
beta <- rnorm(20, 50, 10)
sigma <- runif(20, 0, 50)

data1 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim = alpha[fakedata_ws$species] + (beta + beta[fakedata_ws$species]) +
    rnorm(nsims, mean = 0, sd = sigma)
)

ggplot(data1, aes(x = gdd, y = sim)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(250, 1000), ylim=c(250,1000)) + geom_abline(intercept=0, slope=1)

## Using weakly informative priors
alpha <- rnorm(20, 0, 1)
beta <- rnorm(20, 0, 1)
sigma <- runif(20, 0, 1)

data2 <- data.frame(
  gdd = fakedata_ws$gdd,
  sim = alpha[fakedata_ws$species] + (beta + beta[fakedata_ws$species]) +
    rnorm(nsims, mean = 0, sd = sigma)
)

ggplot(data2, aes(x = gdd, y = sim2)) + 
  geom_point(alpha = 0.1, color = "red") + 
  xysim_labs + coord_cartesian(xlim=c(0, 1000), ylim=c(0,1000)) + geom_abline(intercept=0, slope=1)

data3 <- data.frame(
  gdd = fakedata_ws$gdd, 
  wip = data2$sim, 
  vague = data1$sim
)
ggplot(data3, aes(x=gdd, y=wip)) + 
  geom_point(alpha = 0.1) + 
  geom_point(
    aes(y = vague), 
    color = "red", 
    alpha = 0.1
  ) + 
  xysim_labs + coord_cartesian(xlim=c(0, 1000), ylim=c(0,1000)) + geom_abline(intercept=0, slope=1)
