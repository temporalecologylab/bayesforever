## Started 4 June 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##
## Converted by Cat for the cluster

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#--------------------------------------
# Now simulate the phenology side
# photodat ~ a_photo[sp] + bphoto[sp]*P + sigma_y_photo
# bphoto[sp] ~ aphoto[sp] + betaMinLatxPhoto*mua_sp + betaMaxLatxPhoto*mua_sp

# Parameters for pheno
sigma_yphoto <- 3
sigma_y <- 3 ## for latitude model

### Okay do we actually want to know what the influence of "more northern than normal" 
## lats is rather than the actual latitude?
# So maybe we should center the data?
# Parameters
a_min <- 0
#a_max <- 0

sigma_bphotomin <- 5
#sigma_bphotomax <- 2

n <- 10 # number of replicates per sp x study (may eventually want to draw this from a distribution to make data more realistic)
nsp <- 30 # number of species

### This will be a fixed effects model but I think we need some mua_sp to create some variation around our species estimates
## And now let's add a greater sigma since our data is centered
sigma_asp <- 10
mua_sp <- rnorm(nsp, 0, sigma_asp)

# Set up the data ...
simlat <- data.frame(sp=rep(1:nsp, each=10), mua_sp=rep(mua_sp, each=10))

simlat$minlat <- a_min + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
#simlat$maxlat <- a_max*simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)


nsp # Same as above (you could vary it but would be a little trickier) 
mua_sp # this is the effect of species trait differences from the trait model (above)

sigma_aphoto <- 5

a_photo <- rnorm(nsp, -2, sigma_aphoto)
beta_photomin <- rnorm(nsp, 0, sigma_bphotomin)
##beta_photomax <- rnorm(nsp, 0.5, sigma_bphotomax)

mua_photo_sp <- rnorm(nsp, 0, 5)

#sigmab_grand <- 2
#mub_grand <- rnorm(nsp, 0, sigmab_grand)

Pmean <- 6
Psigma <- 2

simpheno <- data.frame(sp=numeric(), a_photo=numeric(), P=numeric())

nph <- 50 # number of observations per species/phenological combination 
Nph <- nsp * nph # obervations per species for phenological event and photoperiod

for (sp in 1:nsp){
  Phere <- rnorm(nph, Pmean, Psigma)
  simphenoadd <- data.frame(sp=rep(sp, nph), a_photo=rep(a_photo[sp], nph), P=Phere, mua_photo_sp=rep(mua_photo_sp[sp], each=nph))
  simpheno <- rbind(simpheno, simphenoadd)
}


#bphoto <- beta_photomin * simlat$minlat + beta_photomax * simlat$maxlat

#simpheno$photodat <- simpheno$a_photo + bphoto*simpheno$P + rnorm(nrow(simpheno), 0, sigma_yphoto)

simpheno$photodat <- (simpheno$a_photo + simpheno$mua_photo_sp)*simpheno$P + #mub_grand * simpheno$P + 
  (beta_photomin * simlat$minlat) + rnorm(nrow(simpheno), 0, sigma_yphoto) ## + (beta_photomax * simlat$maxlat)*simpheno$P

N <- length(simlat$minlat)

Npheno <- length(simpheno$photodat)
latstanpheno <- list(mindat = simlat$minlat, #maxdat = simlat$maxlat,
                     photodat = simpheno$photodat,
                     N = N, nsp = nsp, species = simlat$sp, 
                     Npheno = Npheno, nsppheno = nsp,
                     speciespheno = simpheno$sp, photoperiod = simpheno$P, 
                     latmins = simlat$a_min, latmaxs = simlat$a_max)
#/n/wolkovich_lab/Lab/Cat/
jointfit <- stan(file = "~/Documents/git/bayes2020/Projects/Cat/stan/joint_photolat.stan", data = latstanpheno, warmup = 1000, iter = 2000,
                 chains = 4, cores = 4,  control=list(max_treedepth = 15)) 

save(jointfit, file="~/Desktop/jointphotolat.Rda")

if(FALSE){
if(!runfullmodel){
  load("~/Desktop/jointphotolat.Rda")
}

# Checking against sim data
bigfitpost <- rstan::extract(jointfit)
bigfitsum <- rstan::summary(jointfit)$summary

sd(simpheno$photo) ## 22.6
mean(bigfitsum[grep("sigma_yphoto", rownames(bigfitsum)),"mean"]) ### 22.4

mean(simpheno$a_photo) ## -2.13
mean(bigfitsum[grep("a_photo", rownames(bigfitsum)),"mean"]) ## -1.39

mean(simlat$minlat) ## 0.16
mean(bigfitsum[grep("a_mins_sp", rownames(bigfitsum)),"mean"]) # 0.09
mean(simlat$maxlat) ## 0.07
mean(bigfitsum[grep("a_maxs_sp", rownames(bigfitsum)),"mean"]) # 0.05


bigfitsum[grep("a_mins_sp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_mins_sp\\[", rownames(bigfitsum)),"mean"]~unique(ave(simlat$minlat, simlat$sp))) # great

bigfitsum[grep("a_maxs_sp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_maxs_sp\\[", rownames(bigfitsum)),"mean"]~unique(ave(simlat$maxlat, simlat$sp))) # great

# Trait model
sigma_y  ## 2
mean(bigfitpost[["sigma_y"]]) ## 2.01
a_min ## 0
mean(bigfitpost[["a_mins_sp"]]) ## 0.09
a_max ## 0
mean(bigfitpost[["a_maxs_sp"]]) ## 0.05

# Hyperparameters
mub_grand
bigfitsum[grep("agrand\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("agrand\\[", rownames(bigfitsum)),"mean"]~mub_grand) ### this okay...

a_photo
bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]~a_photo) ### this okay...

beta_photomin
bigfitsum[grep("a_photomin\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_photomin\\[", rownames(bigfitsum)),"mean"]~beta_photomin) ### bad!!!!
beta_photomax
bigfitsum[grep("mu_bphotomax\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mu_bphotomax\\[", rownames(bigfitsum)),"mean"]~beta_photomax) ### bad


yphotos <- simpheno$photodat
ypp_photos <- bigfitsum[grep("y_ppphoto\\[", rownames(bigfitsum)),"mean"]

plot(ypp_photos~yphotos)

}

shinystan::launch_shinystan(jointfit)
