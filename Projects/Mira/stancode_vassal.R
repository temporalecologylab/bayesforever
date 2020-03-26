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

setwd("~/Documents/git/bayes2020/Projects/Mira/")

# read in simulated data
sim <- 
head(sim)

# STAN model

x <- sim$year
y <- sim$fin2.dat
variety <- sim$var_yr
N <- #number of observations

# data to stan
stan.data <- list(x=x, y=y, variety=variety)

# stan model starts here
write("//
      // stan linear regression model for duration of vassal interphenophases
      
      data{
      int<lower=0> N;
      vector[N] x;
      vector[N} y;
      }
      parameters{
      real alpha
      real beta
      real<lower=0> sigma
      }
      transformed parameters{
      a.var
      }
      model{
      for (n in 1:N)
      y[n] ~ normal(alpha + beta * x[n], sigma);
      }
      generated quantities{
      
      }",
      "vassal_bb_fl.stan")


#stan model end
