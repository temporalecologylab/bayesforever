### Started 23 April 2020 by Cat
## Building new dataframe with fake data to try and better understand hobo logger data versus microclimate data

# Maybe I should use estimates for fstar from real models?

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
# GDDlo ~ urban + (urban|species) 

## Let's start with Question 1 first...
#library(rethinking)
library(RColorBrewer)
library(viridis)
library(lme4)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(rstan)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
mean(ws$gdd_bb, na.rm=TRUE) ## 292
sd(ws$gdd_bb, na.rm = TRUE) ## 116


use.urban = TRUE
use.provenance = FALSE

if(use.urban == TRUE & use.provenance == TRUE){
  print("Error has occurred. Can't have both urban and provenance equal TRUE!")
}

if(use.urban == FALSE & use.provenance == FALSE){
  print("Error has occurred. Can't have both urban and provenance equal TRUE!")
}


# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 250 #### just to make sure we don't get any NAs
nspps <- 12 
ninds <- 10 
nobs <- nspps*ninds
nsites <- 2
nmicros <- 10

if(use.urban==TRUE){
urbeffect <- -75
#urbsd <- 20 ### only used when using provenance
}

if(use.provenance==TRUE){
proveffect <- -10
}

fstar <- 250
fstarspeciessd <- 20
fstarindsd <- 10
  
dayz <- rep(1:daysperyr, nobs)
cc.arb <- 11 ## based off weather station data
mean.microarb <- 2
sigma.arb <- 8 
sigma.microarb <- 1

cc.hf <- 9  ## based off weather station data
mean.microhf <- 2
sigma.hf <- 8  
sigma.microhf <- 1

source("simulations/micro_databuildfx.R") ### warning messages are okay - outdated package warning but still works

cols <-viridis_pal(option="viridis")(3)
## Just a quick check on GDDs
quartz()
ggplot(df.fstar, aes(x=fstar.new)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$site)))


### Okay, first let's check on site level varition in temperature
#### Before moving on, let's look at the data a bit
ws <- ggplot(df, aes(x=tmean.ws)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$site))) + ggtitle("Weather Station") +
  coord_cartesian(xlim=c(-25, 42))

hl <- ggplot(df, aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
  scale_fill_manual(name="Site", values=cols, labels=sort(unique(df$site))) + ggtitle("Hobo Logger") +
  coord_cartesian(xlim=c(-25, 42))

quartz()
grid.arrange(ws, hl, ncol=2)


### Now let's look at GDD differences between methods
quartz()
par(mfrow=c(1,2))
my.pal <- viridis_pal(option="magma")(12)
my.pch <- c(15:16)
plot(bbws.gdd ~ species, col=my.pal[as.factor(bball$species)], pch=my.pch[as.factor(bball$site)], data = bball, main="Weather Station",
     ylab="GDD")
abline(h=mean(bball$bbws.gdd), lwd=3)

plot(bbhl.gdd ~ species, col=my.pal[as.factor(bball$species)], pch=my.pch[as.factor(bball$site)], data = bball, main="Hobo Logger",
     ylab="GDD")
abline(h=mean(bball$bbhl.gdd), lwd=3)


### Next, we can take a quick glimpse at results
if(use.urban==TRUE){
bball$urban <- ifelse(bball$site=="arb", 1, 0)
modtest <- lmer(bbws.gdd ~ urban + (urban|species), data=bball)
arm::display(modtest)

modtest.hl <- lmer(bbhl.gdd ~ urban + (urban|species), data=bball)
arm::display(modtest.hl)
}

if(use.provenance==TRUE){
  bball$provenance <- as.numeric(bball$provenance)
  modtest <- lmer(bbws.gdd ~ provenance + (provenance|species), data=bball)
  arm::display(modtest)
  
  modtest.hl <- lmer(bbhl.gdd ~ provenance + (provenance|species), data=bball)
  arm::display(modtest.hl)
}

if(FALSE){
library(sjPlot)
quartz()
sjPlot::tab_model(modtest, modtest.hl)
}
 

#####And finally... it's modelling time!
if(use.urban==TRUE){
bball$urban <- ifelse(bball$site=="arb", 1, 0)

datalist.gdd <- with(bball, 
                     list(y = bbws.gdd, 
                          tx = urban,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(bball),
                          n_sp = length(unique(bball$species))
                     )
)


ws_urb_buildfake = stan('stan/urbanmodel_stan_normal_ncp.stan', data = datalist.gdd,
                   iter = 4000, warmup=2000, control=list(adapt_delta = 0.99)) ### 

#check_all_diagnostics(ws_urb_buildfake)

ws_urb_fake.sum <- summary(ws_urb_buildfake)$summary
ws_urb_fake.sum[grep("mu_", rownames(ws_urb_fake.sum)),]
ws_urb_fake.sum[grep("sigma_", rownames(ws_urb_fake.sum)),]

#save(ws_urb_buildfake, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan_builtsims_ncp.Rdata")




datalist.gdd <- with(bball, 
                     list(y = bbhl.gdd, 
                          tx = urban,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(bball),
                          n_sp = length(unique(bball$species))
                     )
)


hl_urb_buildfake = stan('stan/urbanmodel_stan_normal_ncp.stan', data = datalist.gdd,
                        iter = 4000, warmup=2000, control=list(adapt_delta = 0.99)) ### 

#check_all_diagnostics(hl_urb_buildfake)

hl_urb_fake.sum <- summary(hl_urb_buildfake)$summary
hl_urb_fake.sum[grep("mu_", rownames(hl_urb_fake.sum)),]
hl_urb_fake.sum[grep("sigma_", rownames(hl_urb_fake.sum)),]

#save(hl_urb_buildfake, file="~/Documents/git/microclimates/analyses/stan/hl_urban_stan_builtsims_ncp.Rdata")

### Compare side by side
ws_urb_fake.sum[grep("mu_", rownames(ws_urb_fake.sum)),]
hl_urb_fake.sum[grep("mu_", rownames(hl_urb_fake.sum)),]

ws_urb_fake.sum[grep("sigma_", rownames(ws_urb_fake.sum)),]
hl_urb_fake.sum[grep("sigma_", rownames(hl_urb_fake.sum)),]
}


##### Provenance Model!
if(use.provenance==TRUE){
  bball$provenance <- as.numeric(bball$provenance)
  
  datalist.gdd <- with(bball, 
                       list(y = bbws.gdd, 
                            prov = provenance,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(bball),
                            n_sp = length(unique(bball$species))
                       )
  )
  
  
  ws_prov_buildfake = stan('stan/provmodel_stan_normal_ncp.stan', data = datalist.gdd,
                          iter = 4000, warmup=2000, control=list(adapt_delta = 0.99)) ### BAD MODEL!!!
  
  #check_all_diagnostics(ws_prov_buildfake)
  
  ws_prov_fake.sum <- summary(ws_prov_buildfake)$summary
  ws_prov_fake.sum[grep("mu_", rownames(ws_prov_fake.sum)),]
  ws_prov_fake.sum[grep("sigma_", rownames(ws_prov_fake.sum)),]
  
  #save(ws_prov_buildfake, file="~/Documents/git/microclimates/analyses/stan/ws_prov_stan_builtsims_ncp.Rdata")
  
  

  datalist.gdd <- with(bball, 
                       list(y = bbhl.gdd, 
                            prov = provenance,
                            sp = as.numeric(as.factor(species)),
                            N = nrow(bball),
                            n_sp = length(unique(bball$species))
                       )
  )
  
  
  hl_prov_buildfake = stan('stan/provmodel_stan_normal_ncp.stan', data = datalist.gdd,
                          iter = 4000, warmup=2000, control=list(adapt_delta = 0.99)) ### Better but still bad
  
  #check_all_diagnostics(hl_prov_buildfake)
  
  hl_prov_fake.sum <- summary(hl_prov_buildfake)$summary
  hl_prov_fake.sum[grep("mu_", rownames(hl_prov_fake.sum)),]
  hl_prov_fake.sum[grep("sigma_", rownames(hl_prov_fake.sum)),]
  
  #save(hl_prov_buildfake, file="~/Documents/git/microclimates/analyses/stan/hl_prov_stan_builtsims_ncp.Rdata")
  
  ### Compare side by side
  ws_prov_fake.sum[grep("mu_", rownames(ws_prov_fake.sum)),]
  hl_prov_fake.sum[grep("mu_", rownames(hl_prov_fake.sum)),]
  
  ws_prov_fake.sum[grep("sigma_", rownames(ws_prov_fake.sum)),]
  hl_prov_fake.sum[grep("sigma_", rownames(hl_prov_fake.sum)),]
}
