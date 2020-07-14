## stan model for bayesian class. Vassal interphenophases

# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

library(rstan)
library(rstanarm)
library(shinystan)
library(ggplot2)
library(dplyr)
library(bayesplot)

#setting working directory
if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/bayes2020/Projects/Mira/")
} else setwd("~/Documents/git/bayes2020/Projects/Mira")


# read in simulated data
sim <- read.csv("y3.dat.csv", header=T)
head(sim)

# STAN model

x <- sim$year
y <- sim$fin3.dat
variety <- sim$variety
N <- nrow(sim)
Nv <- length(unique(sim$variety))

# data to stan
stan.data <- list(x=x, y=y, variety=variety, N=N, Nv=Nv)

# stan model starts here
write("//
      // stan linear regression model for duration of vassal interphenophases

      data{
        int<lower=0> N;       //all the samples
        vector[N] x;          // year predictor
        vector[N} y;          // predicted duration of interphenophase
        int<lower=1> Nv;      // number of varieties
        int<lower=1, upper=Nv> variety[N]; // what does this do?
      }
      
      parameters{
        real a_var;               // mu, one per var
        real b_var;               // mu, one per var
        real<lower=0> s_avar;     // for alpha, one per var
        real<lower=0> s_avar;     // for beta, one per var
        real<lower=0> sigma_y;    // big sig
        
        //priors
        a_var ~ normal(65,5);
        b_var ~ normal(0,1);
        s_avar ~ normal(0,10);
        s_bvar ~ normal(0,0.2);
        sigma_y ~ normal(0,10);
    
      }
      
      model{
        // likelihood
        for (n in 1:N)
        y[n] ~ normal(a_var + b_var * x[n], sigma);
      }",
      
      "vassal_bb_fl.stan")
#stan model end


# if we had stan
#bbfl <- stan("vassal_bb_fl.stan", data = stan.data, iter = 2000, warmup = 1000)
#sum.bbfl <- summary(bbfl)
sum.bbfl <- summary(bbfl)$summary
sum.bbfl[grep("mu_", rownames(sum.bbfl)),] 
sum.bbfl[grep("s\\_", rownames(sum.bbfl)),] 
sum.bbfl[grep("sigma_y", rownames(sum.bbfl)),] 
#precis(bbfl)

#shinystan::launch_shinystan(bbfl)

# CmdStanR - also not working
#library(cmdstanr)
#bbfl.file <- file.path("~/Documents/git/bayes2020/Projects/Mira/vassal_bb_fl.stan")
#bbfl.mod <- cmdstan_model(bbfl.file)
