# Stats class March 16th - WHat Faith learned on her external stats class with mike Betacourt 
#------------------------------------------------------------------------------------------

#This code will go over some of the useful things I learned over the last few weeks in reation to hierarchical modeling. 
#Specifically when to use centred vs non centred parametreisation, and a few extras relating to our previous discussions. 

#housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)


setwd("/home/faith/Documents/github/bayes2020/Projects/Faith/HierarchicalTips")


util <- new.env()
source('stan_utility.R', local=util)#Code writen by michael Betancourt, I'm not sure I shoudl share it...

#Libraries
library(ggplot2)
library(rstan)


#Michael's favourite colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

c_light_trans <- c("#DCBCBC80")
c_dark_trans <- c("#8F272780")
c_green_trans <- c("#00FF0080")


#1. A simple intercept model working well witj centred parameratization 
#---------------------------------------------------------------------------


#simulate ideal data for an intercept only mixed model measuring height of different trees
#We have different tree species amoung these trees

nTreeObs <- 60 # 50 replicates per species 
nSpecies <- 20 # fewer than 10 species makes it difficult to estimate teh sigma of the ditribution  
N <- nTreeObs*nSpecies #Oveall nmber of observation 


meanHeight <- 25
sigmaGrand <- 10
sigmaSpecies <- 10

#simulate observations using grand mean and grand sigma (general observation error) 
obsDataRaw <- rnorm(N, meanHeight,sigmaGrand)

#simulate effect of species, then repeat so observations within a species share the same species effect  
SpecEffect <- rnorm(nSpecies, 0,sigmaSpecies)
SpecEffectRep <- rep(SpecEffect, each = nTreeObs)
SpeciesName <-  rep(1:nSpecies, each = nTreeObs)

simTreeHeight <- data.frame(cbind(obsDataRaw,SpecEffectRep, SpeciesName))
simTreeHeight$simObs  <- simTreeHeight$obsDataRaw + simTreeHeight$SpecEffectRep

hist(simTreeHeight$simObs )

#Run Cetred Model

stanDataCP <- list(N = N,  y = simTreeHeight$simObs, nSpec = nSpecies, 
	SpecID = simTreeHeight$SpeciesName)

cp_fit <- stan(file='Hierarchical_centred.stan', data=stanDataCP, seed=4938483,
               iter=1000, warmup=500)

posterior_cp <- extract(cp_fit)

str(posterior_cp)
pairs(cp_fit)#too many parameters to easily plot pairs plot 
pairs(cp_fit, pars = c("mu","sigma_g","sigma_sp", "lp__"))

#When plotting parameters that can't go below 0, you should plot the parameter on teh log scale
# so all sigma values in thsi case shoudl eb plotted on teh log scale
#it's also important to plot the sigma_sp value against teh sigma value for their distribution (sigma_sp)
# becauese the relationship between these is often what breaks down in miced models

posterior_cp$speciesHeight[,1]

partition <- util$partition_div(cp_fit) # thsi is a tool of Mchael's 
div_samples <- partition[[1]]
nondiv_samples <- partition[[2]]
str(nondiv_samples)

par(mfrow=c(3, 4))

for (i in 1:nSpecies) {
  name_x <- paste("speciesHeight[", i, "]", sep='')

  plot(nondiv_samples[name_x][,1], log(nondiv_samples$sigma_sp),
       col=c_dark_trans, pch=16, main="",
       xlab=name_x, xlim=c(1, 50), ylab="log(sigma_sp)", ylim=c(0.5, 3))
  points(div_samples[name_x][,1], log(div_samples$sigma_sp),
         col=c_green_trans, pch=16)
}

par(mfrow=c(1, 1))


#The model ran well, and the pairs plots all look great. Crucially, there is no evidence for any funnel shapes
# between sigma_sp and teh individual species effects

# We COULD run an ncp modle on thsi data too, but it actually wouldn't fit as well. The model would take longer. 

# When to use an ncp----------------------
#----------------------------------------


#generally we use an ncp when there is less data within each group/context/species. 


nTreeObs <- 4 # 3 replicates per species. The model is going to struggle! 
nSpecies <- 9 # fewer than 10 species makes it difficult to estimate teh siga of the ditribution  
N <- nTreeObs*nSpecies #Oveall nmber of observation 


meanHeight <- 30
sigmaGrand <- 10
sigmaSpecies <- 10

#simulate observations using grand mean and grand sigma (general observation error) 
obsDataRaw <- rnorm(N, meanHeight,sigmaGrand)

#simulate effect of species, then repeat so observations within a species share the same species effect  
SpecEffect <- rnorm(nSpecies, 0,sigmaSpecies)
SpecEffectRep <- rep(SpecEffect, each = nTreeObs)
SpeciesName <-  rep(1:nSpecies, each = nTreeObs)

simTreeHeight <- data.frame(cbind(obsDataRaw,SpecEffectRep, SpeciesName))
simTreeHeight$simObs <- simTreeHeight$obsDataRaw + simTreeHeight$SpecEffectRep

hist(simTreeHeight$simObs )

stanDataCP2 <- list(N = N,  y = simTreeHeight$simObs, nSpec = nSpecies, 
	SpecID = simTreeHeight$SpeciesName)


#Run Cetred Model---------------------------------------

cp_fit2 <- stan(file='Hierarchical_centred.stan', data=stanDataCP2, seed=4938483,
               iter=2000, warmup=500)

posterior_cp2 <- extract(cp_fit2)

str(posterior_cp2)
pairs(cp_fit2, pars = c("mu","sigma_g","sigma_sp", "lp__")) # the problem isnt imediately obvious from this pairs plot 

posterior_cp2$speciesHeight[,1]

partition2 <- util$partition_div(cp_fit2) # thsi is a tool of Mchael's 
div_samples2 <- partition2[[1]]
nondiv_samples2 <- partition2[[2]]

par(mfrow=c(3, 4))

for (i in 1:nSpecies) { # instead lets plot log sigma_sp against each species prediction 
  name_x <- paste("speciesHeight[", i, "]", sep='')

  plot(nondiv_samples2[name_x][,1], log(nondiv_samples2$sigma_sp),
       col=c_dark_trans, pch=16, main="",
       xlab=name_x, xlim=c(1, 50), ylab="log(sigma_sp)", ylim=c(-0.5, 4)) 
  points(div_samples2[name_x][,1], log(div_samples2$sigma_sp),
         col=c_green_trans, pch=16)
}

#Note green divergent transitions are clustering in the mouth of the funnels because thsi is where the model gets stuck. 

par(mfrow=c(1, 1))


#Run ncp model - this shoulf fit better ------------------

nTreeObs <- 4 # I'm cheating here to show you my point. Just beleive me I guess? 
nSpecies <- 9 # fewer than 10 species makes it difficult to estimate teh siga of the ditribution  
N <- nTreeObs*nSpecies #Oveall nmber of observation 


meanHeight <- 30
sigmaGrand <- 10
sigmaSpecies <- 10

#simulate observations using grand mean and grand sigma (general observation error) 
obsDataRaw <- rnorm(N, meanHeight,sigmaGrand)

#simulate effect of species, then repeat so observations within a species share the same species effect  
SpecEffect <- rnorm(nSpecies, 0,sigmaSpecies)
SpecEffectRep <- rep(SpecEffect, each = nTreeObs)
SpeciesName <-  rep(1:nSpecies, each = nTreeObs)

simTreeHeight <- data.frame(cbind(obsDataRaw,SpecEffectRep, SpeciesName))
simTreeHeight$simObs <- simTreeHeight$obsDataRaw + simTreeHeight$SpecEffectRep

hist(simTreeHeight$simObs )

stanDataNCP <- list(N = N,  y = simTreeHeight$simObs, nSpec = nSpecies, 
	SpecID = simTreeHeight$SpeciesName)


ncp_fit <- stan(file='Hierarchical_ncp.stan', data=stanDataNCP, seed=4938483,
               iter=2000, warmup=500)

posterior_ncp <- extract(ncp_fit)


pairs(ncp_fit, pars = c("mu","sigma_g","sigma_sp", "lp__"))

posterior_ncp$speciesHeight[,1]

partition3 <- util$partition_div(ncp_fit) # thsi is a tool of Mchael's 
div_samples3 <- partition3[[1]]
nondiv_samples3 <- partition3[[2]]


par(mfrow=c(3, 4))

for (i in 1:nSpecies) {
  name_x <- paste("speciesHeight[", i, "]", sep='')

  plot(nondiv_samples3[name_x][,1], log(nondiv_samples3$sigma_sp),
       col=c_dark_trans, pch=16, main="",
       xlab=name_x, xlim=c(1, 50), ylab="log(sigma_sp)", ylim=c(0.5, 4)) # also note this model is much less sure
       # what each species shoudl be than the one with more observations per species 
  points(div_samples3[name_x][,1], log(div_samples3$sigma_sp),
         col=c_green_trans, pch=16)
}

par(mfrow=c(1, 1))

#You shoudl not just ncp everything thuogh, because if you have lots of obserrvations per species teh model will struggle to 
#fit an ncp model. 

#But in real life, things are rately so simple. This is the really cool bit - you can use both cp and ncp to estimate the species
#effects if you have uneven distribution of samples accross species. Mike is a big fan of doing this, he said it's teh real trick 
#to getting mixed models to work. 

#Simulate data with uneven sampling effort 





nSpecies <- 10 # fewer than 10 species makes it difficult to estimate teh siga of the ditribution  
nTreeObs <- c(3,2,1000,2,200,2000,250, 300,1,1) # assign different numbers of observations for each species
N <- sum(nTreeObs)



meanHeight <- 40
sigmaGrand <- 5
sigmaSpecies <- 10

#simulate observations using grand mean and grand sigma (general observation error) 
obsDataRaw <- rnorm(N, meanHeight,sigmaGrand)

#simulate effect of species, then repeat so observations within a species share the same species effect  
SpecEffect <- rnorm(nSpecies, 0,sigmaSpecies)
SpecEffectReplist <- list()
SpeciesNamelist <- list()

for (i in 1:nSpecies){
	SpecEffectReplist[[i]] <- rep(SpecEffect[i], each = nTreeObs[i])
	SpeciesNamelist[[i]] <- rep(i, each = nTreeObs[i])
}

SpecEffectRep <- unlist(SpecEffectReplist)
SpeciesName <-  unlist(SpeciesNamelist)

length(obsDataRaw)
length(SpecEffectRep)
length(SpeciesName)

simTreeHeightOdd <- data.frame(cbind(obsDataRaw,SpecEffectRep, SpeciesName))
simTreeHeightOdd$simObs <- simTreeHeightOdd$obsDataRaw + simTreeHeightOdd$SpecEffectRep

hist(simTreeHeightOdd$simObs )

stanDataOdd <- list(N = N,  y = simTreeHeightOdd$simObs, nSpec = nSpecies, 
	SpecID = simTreeHeightOdd$SpeciesName)

cp_fit3 <- stan(file='Hierarchical_centred.stan', data=stanDataOdd, seed=4938483,
               iter=3000, warmup=500)

# I havent managed to recreate the problem in teh model, but I will show you the solution anyway in case you ever need it. 

posterior_cp3 <- extract(cp_fit3)


pairs(cp_fit3, pars = c("mu","sigma_g","sigma_sp", "lp__"))

posterior_cp3$speciesHeight[,1]

partition4 <- util$partition_div(cp_fit3) # thsi is a tool of Mchael's 
div_samples4 <- partition4[[1]]
nondiv_samples4<- partition4[[2]]


par(mfrow=c(3, 4))

for (i in 1:nSpecies) {
  name_x <- paste("speciesHeight[", i, "]", sep='')

  plot(nondiv_samples4[name_x][,1], log(nondiv_samples4$sigma_sp),
       col=c_dark_trans, pch=16, main="",
       xlab=name_x, xlim=c(20, 70), ylab="log(sigma_sp)", ylim=c(0.5, 4)) # also note this model is much less sure
       # what each species shoudl be than the one with more observations per species 
  points(div_samples4[name_x][,1], log(div_samples4$sigma_sp),
         col=c_green_trans, pch=16)
}

par(mfrow=c(1, 1))


#Make a column saying which species to centre and wich to ncp 


as.data.frame(nTreeObs)

# Let's set a threshold that separates out these two contexts from the others.
# you pick thsi by eyeballing the pairs plots and seeing which species 
#are having trouble and which are not. I will assume here that when we have fewer 
#than 25 observations per species then ncp will work better.

thresh <- 25

# We can then model the two well-informed contexts with centered 
# parameterizations and the two sparsely-informed contexts with 
# non-centered parameterizations.

ncp_idx <- unique(simTreeHeightOdd$SpeciesName)[nTreeObs <= thresh]
cp_idx <- unique(simTreeHeightOdd$SpeciesName)[nTreeObs > thresh]



stanDataMixed<- list(N = N,  y = simTreeHeightOdd$simObs, nSpec = nSpecies, 
	SpecID = simTreeHeightOdd$SpeciesName, nSpec_ncp = length(ncp_idx),
	ncp_idx = ncp_idx, nSpec_cp = length(cp_idx), cp_idx = cp_idx)


cp_fit4 <- stan(file='Hierarchical_mixed.stan', data=stanDataMixed, seed=4938483,
               iter=3000, warmup=500)


posterior_cp4 <- extract(cp_fit4)


pairs(cp_fit4, pars = c("mu","sigma_g","sigma_sp", "lp__"))


partition4 <- util$partition_div(cp_fit4) # thsi is a tool of Mchael's 
div_samples4 <- partition4[[1]]
nondiv_samples4<- partition4[[2]]


par(mfrow=c(3, 4))

for (i in 1:nSpecies) {
  name_x <- paste("speciesHeight[", i, "]", sep='')

  plot(nondiv_samples4[name_x][,1], log(nondiv_samples4$sigma_sp),
       col=c_dark_trans, pch=16, main="",
       xlab=name_x, xlim=c(20, 70), ylab="log(sigma_sp)", ylim=c(0.5, 4)) # also note this model is much less sure
       # what each species shoudl be than the one with more observations per species 
  points(div_samples4[name_x][,1], log(div_samples4$sigma_sp),
         col=c_green_trans, pch=16)
}

par(mfrow=c(1, 1))

