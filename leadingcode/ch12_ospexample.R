## Cat decides she doesn't like the questions from Chapter 12 so we're going to make up a new question

# Links for class:
# http://mfviz.com/hierarchical-models/
# https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load libraries
library(ggplot2)
library(rethinking)

set.seed(1221)

### Let's use OSPREE data
setwd("~/Documents/git/bayes2020")
osp <- read.csv("leadingcode/data/bbstan_mainmodel_utah_allsppwcrops_allfp_allchill.csv")

osp <- subset(osp, select=c("resp", "force", "photo", "chill", "complex.wname", "datasetID"))
osp <- osp[complete.cases(osp),]

## Let's look at days to budburst for all species
# We will start with no pooling to compare to partial pooling
m_nopool <- map2stan(
    alist(
      resp ~ dnorm(mu, sigma),
      mu <- a + bf*force,
      a ~ dnorm(0, 50),
      bf ~ dnorm(0,50),
      sigma ~ dnorm(0,10)
    ),
    data=osp,
    chains=2, iter=2000, warmup=1500, 
    control=list(max_treedepth = 15,adapt_delta = 0.99)
  )

precis(m_nopool)


osp$species <- as.integer(as.factor(osp$complex.wname))

m_partpool <- map2stan(
  alist(
    resp ~ dnorm(mu, sigma),
    mu <- a + a_species[species] + bf*force,
    a_species[species] ~ dnorm(0, sigma_species),
    a ~ dnorm(0, 50),
    bf ~ dnorm(0,50),
    sigma ~ dnorm(0,10),
    sigma_species ~ dnorm(0,10)
  ),
  data=osp,
  chains=2, iter=2000, warmup=1500, 
  control=list(max_treedepth = 15,adapt_delta = 0.99)
)


precis(m_partpool, depth=2)
precis(m_nopool)

par(mfrow=c(1,2))
plot(precis(m_nopool))
plot(precis(m_partpool, depth=2))


### alright, let's add in one more random intercept using study id
osp$study <- as.numeric(as.factor(osp$datasetID))

m_partpool_wstudy <- map2stan(
  alist(
    resp ~ dnorm(mu, sigma),
    mu <- a + a_study[study] + a_species[species] + bf*force,
    a_study[study] ~ dnorm(0, sigma_study),
    a_species[species] ~ dnorm(0, sigma_species),
    a ~ dnorm(0, 50),
    bf ~ dnorm(0, 50),
    sigma ~ dnorm(0,10),
    sigma_study ~ dnorm(0,10),
    sigma_species ~ dnorm(0,10)
  ),
  data=osp,
  chains=2, iter=2000, warmup=1500, 
  control=list(max_treedepth = 15,adapt_delta = 0.99)
)



dev.new()
postpartpool <- extract.samples(m_partpool)
postpartpool_study <- extract.samples(m_partpool_wstudy)
dens(postpartpool_study$sigma, xlab="sigma", xlim=c(13, 22))
dens(postpartpool$sigma, col="blue", lwd=2, add=TRUE)

dev.new()
par(mfrow=c(1,2))
plot(precis(m_partpool, depth=2))
plot(precis(m_partpool_wstudy, depth=2))


if(FALSE){
###### For full model, see here:
m_nopool_full <- map2stan(
  alist(
    resp ~ dnorm(mu, sigma),
    mu <- a + bf*force + bp*photo + bc*chill,
    a ~ dnorm(0, 50),
    bf ~ dnorm(0,50),
    bp ~ dnorm(0,50),
    bc ~ dnorm(0,50),
    sigma ~ dnorm(0,10)
  ),
  data=osp,
  chains=4, iter=4000, warmup=2000, 
  control=list(max_treedepth = 15,adapt_delta = 0.99)
)




m_partpool_full <- map2stan(
  alist(
    resp ~ dnorm(mu, sigma),
    mu <- a + a_species[species] + bf*force + bp*photo + bc*chill,
    a_species[species] ~ dnorm(0, sigma_species),
    a ~ dnorm(0, 50),
    bf ~ dnorm(0,50),
    bp ~ dnorm(0,50),
    bc ~ dnorm(0,50),
    sigma ~ dnorm(0,10),
    sigma_species ~ dnorm(0,10)
  ),
  data=osp,
  chains=4, iter=4000, warmup=2000, 
  control=list(max_treedepth = 15,adapt_delta = 0.99)
)

par(mfrow=c(1,2))
plot(precis(m_nopool_full))
plot(precis(m_partpool_full789epth=2))

m_partpool_wstudy_full <- map2stan(
  alist(
    resp ~ dnorm(mu, sigma),
    mu <- a + a_species[species] + a_study[study] + bf*force + bp*photo + bc*chill,
    a_species[species] ~ dnorm(0, sigma_species),
    a_study[study] ~ dnorm(0, sigma_study),
    c(a, bf, bp, bc) ~ dnorm(0, 50),
    c(sigma, sigma_species, sigma_study) ~ dnorm(0,10)
  ),
  data=osp,
  chains=4, iter=4000, warmup=2000, 
  control=list(max_treedepth = 15,adapt_delta = 0.99)
)

}