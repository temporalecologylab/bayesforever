### Started 30 January 2020 by Cat
## Let's try and build a model in stan!!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
## Compare GDDs between hobo loggers and weather station data
# 1) GDDlo ~ 1 + (1|species) - do once for HF weather station, once for hobo logger and repeat for Arboretum
# Compare urban effect using weather station data and then hobo logger data
# 2) GDDlo ~ urban + (urban|species) - do once with weather station data and once with hobo logger data

## Let's start with Question 1 first...
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstanarm)
library(rstan)
library(brms)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

figpath <- "figures"
figpathmore <- "wsall_urb"

source("source/microurban_muplot.R")

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4


# Load fitted stan model: no interactions
#load("stan/hobo_urban_mod.Rdata") 
#load("stan/ws_urban_mod.Rda") 
load("stan/wsall_urban_mod.Rda")

sumer.ws <- summary(wsall_urb_mod)$summary
sumer.ws[grep("mu_", rownames(sumer.ws)),]

unique(ws_urb.stan$spp) # numbers are alphabetical
sort(unique(ws_urb.stan$spp))

# m1.bb <- m2l.ni
modelhere <- wsall_urb_mod
muplotfx(modelhere, "", 7, 8, c(0,2), c(-400, 700) , 750, 2)


