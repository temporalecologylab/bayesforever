rm(list = ls())
#script started by Faith Jones on the 5th March 2020, with year removed April 8th 
#this code will not run without access to the BCvin repo. It is for reference only. 

#running a mixed linear model of bud winter hardiness regressed against air temperature. There is grouping 
#on the intercept and slope for variety  

#Overview:
#----------------

#the best model for simulated data is slopeVarietyCov.stan (fit 6), which has a covarience structure and
#no non centred parameterisation 
#there are three more models of simulated data in this script as well with either no covarience structure or non-centred parameterisation (or both)

#for real data, I had to remove the covariance structure and non-centre 
setwd("/home/faith/Documents/github/bcvin/hardiness/analyses/")


#libraries
#install.packages("reshape2")
library(reshape2)
library(ggplot2)
library(rstan)
library(lme4)
library(rstanarm)
library(truncnorm) # truncated normal distribution 
library(fitdistrplus) # fitting a gamm adsitribution 
library(brms)
library(rethinking) # for HPDI function 
library(scales) # alpha making plotting points translucent 
library(bayesplot)# nice posterior check plots 
library(tidyr)
library(dplyr)
library(shinystan)
library(MASS)

#climate data
clim <- read.delim("input/envcanada_penticton.csv", skip=25, sep=",", header=TRUE)
clim$date <- as.Date(clim$Date.Time, format="%m/%d/%y")
clim$month <- format(clim$date, "%b")
clim$day<- format(clim$date,"%d")
head(clim)
climsm <- subset(clim, select=c("Year", "month","day", "Mean.Temp..C.", "Mean.Temp.Flag", "date", "Date.Time"))
names(climsm) <- c("Year", "month","day", "meanC", "meanC.flag", "date", "Date.Time")

# hardiness data
#--------------------------
budhardiness2012to13 <- read.csv("input/budhardiness2012to13.csv", header=TRUE)
budhardiness2013to14 <- read.csv("input/budhardiness2013to14.csv", header=TRUE)
budhardiness2014to15 <- read.csv("input/budhardiness2014to15.csv", header=TRUE)
budhardiness2015to16 <- read.csv("input/budhardiness2015to16.csv", header=TRUE)
budhardiness2016to17 <- read.csv("input/budhardiness2016to17.csv", header=TRUE)
budhardiness2017to18 <- read.csv("input/budhardiness2017to18.csv", header=TRUE) 
budhardiness2018to19 <- read.csv("input/budhardiness2018to19.csv", header=TRUE) 

bh12 <- melt(budhardiness2012to13, id.var=c("X2012...2013", "Variety"))
bh13 <- melt(budhardiness2013to14, id.var=c("X2013...2014", "Variety"))
bh14 <- melt(budhardiness2014to15, id.var=c("X2014...2015", "Variety"))
bh15 <- melt(budhardiness2015to16, id.var=c("X2015...2016", "Variety"))
bh16 <- melt(budhardiness2016to17, id.var=c("site", "Variety")) 
bh17 <- melt(budhardiness2017to18, id.var=c("site", "X2017...2018")) 
bh18 <- melt(budhardiness2018to19, id.var=c("site", "Variety")) 

nameshere <- c("site", "variety", "Date", "lte")
names(bh12) <- nameshere
names(bh13) <- nameshere
names(bh14) <- nameshere
names(bh15) <- nameshere
names(bh16) <- nameshere
names(bh17) <- nameshere
names(bh18) <- nameshere
bh12$years <- "2012to2013"
bh13$years <- "2013to2014"
bh14$years <- "2014to2015"
bh15$years <- "2015to2016"
bh16$years <- "2016to2017"
bh17$years <- "2017to2018"
bh18$years <- "2018to2019"

bhall.rbind <- rbind(bh12, bh13, bh14, bh15, bh16, bh17, bh18)

# remove the averages....
bhall <- subset(bhall.rbind, site!="Average Bud Hardiness (all sites, all varieties)")

# cleaning names
sort(unique(bhall$site))
bhall$site[bhall$site=="Naramata bench"] <- "Naramata Bench"
sort(unique(bhall$variety))
sort(unique(bhall$Date))
# cleaning dates
breakbyperiod <- strsplit(as.character(bhall$Date), ".", fixed=TRUE) 
bhall$Day <- unlist(lapply(breakbyperiod, function(x) x[1]))
bhall$month <- unlist(lapply(breakbyperiod, function(x) x[2]))
bhall$day <- unlist(lapply(strsplit(as.character(bhall$Day), "X", fixed=TRUE), function(x) x[2]))

# right, so now, we need to fix year!
bhall$year <- NA
bhall$year[bhall$years=="2012to2013" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2012
bhall$year[bhall$years=="2012to2013" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2013
bhall$year[bhall$years=="2013to2014" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2013
bhall$year[bhall$years=="2013to2014" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2014
bhall$year[bhall$years=="2014to2015" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2014
bhall$year[bhall$years=="2014to2015" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2015
bhall$year[bhall$years=="2015to2016" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2015
bhall$year[bhall$years=="2015to2016" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2016
bhall$year[bhall$years=="2016to2017" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2016
bhall$year[bhall$years=="2016to2017" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2017
bhall$year[bhall$years=="2017to2018" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2017
bhall$year[bhall$years=="2017to2018" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2018
bhall$year[bhall$years=="2018to2019" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2018
bhall$year[bhall$years=="2018to2019" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2019

# and make a useful df
bh <- subset(bhall, select=c("year", "month", "day", "variety", "lte", "site"))
head(bh)

#make a date column 
bh$Date <- paste(bh$month, bh$day, bh$year, sep = "/")
bh$Datestrptime <- as.POSIXct(strptime(bh$Date ,format="%b/%d/%Y"))
climsm$Datestrptime <- as.POSIXct(strptime(climsm$Date.Time,format="%m/%d/%y"))
#note, dates that are in strptime format rather than as,POSIXct cannot be used to merge 

#combine datasets
bhclim <- merge(bh, climsm, by.x = "Datestrptime", by.y = "Datestrptime") 
bhclim$Month_num <- format(as.Date(bhclim$Datestrptime), "%m")
bhclim$month_day <- as.numeric(paste(bhclim$Month_num, bhclim$day.x, sep = "."))

#set columns as factors
bhclim$year <- as.factor(bhclim$year)
bhclim$variety <- as.factor(bhclim$variety)

#explore a bit 
head(bhclim)
str(bhclim)
plot(bhclim$lte ~ bhclim$meanC)
climatehardPlot <- ggplot(aes(x = meanC, y = lte), data = bhclim)
climatehardPlot + geom_point(aes(colour = factor(Year))) +	
	theme_classic() + ylab("LTE50")

climatePlot <- ggplot(aes(x = Datestrptime, y = lte), data = bhclim)
climatePlot + geom_point() +
  xlab("Date") + ylab("LTE50")	+
	theme_classic()


plot(bhclim$lte ~bhclim $Datestrptime)
plot(bhclim$lte ~bhclim $month_day )
plot(bhclim$lte ~bhclim $meanC)


plot(bhclim$lte ~ bhclim$meanC, type = "p", pch = 16, cex.lab=1.5, cex.axis=1.5,
	xlab = "Mean daily air temperature (C)", ylab = "Observed LTE50 (C)")

abline(lmFit, col = "red")

yearPlot <- ggplot(aes(x = year, y = lte), data = bhclim)
yearPlot + geom_boxplot()+theme_classic()

varietyPlot <- ggplot(aes(x = variety, y = lte), data = bhclim)
varietyPlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

sitePlot <- ggplot(aes(x = site, y = lte), data = bhclim)
sitePlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))


#try and simulate a multi level model of hardiness against temp partially pooled by variety 
#-------------------------------------------------------------------------------------
set.seed(16)
#model should be:

# y ~ Normal((alpha + alphaSite) + beta * x, sigma/eps)

#parameters 
#LTE50sim (y ) is simulated using parameters

#inputs
nrep <- 40 # number of reps of each variety and year (days sampled in a year and for a variety )
meanTemp <- 2
sigmaTemp <- 5
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)

nvariety <- 40
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"


#parameters (mostly taken from the lmer model)

nObs <- nvariety*nrep # the number of observations  for each year and each variety combined 

#make a multivariate distribution of slopes and intercepts for each grouping effect
alpha <- -20 # overall gran mean alpha
betag <- 0.50 # overall grand mean beta

muAB <- c(alpha, betag)#combine two gran effects

#variety
sigma_vara <- 0.2 # standard deviation in intercepts for vatiety 
sigma_varb <- 0.3 # standard deviation in slopes for vatiety 
rho_var <- -0.7 # correlation between intercept and slope. Lower intercepts (more cold hardy) should have steeper slopes

sigmas_var <- c(sigma_vara, sigma_varb) # combine sigma values into a vector
Rho_var <- matrix(c(1, rho_var, rho_var, 1), nrow = 2) # correlation matrix
Sigma_var <- diag(sigmas_var) %*% Rho_var %*% diag(sigmas_var) # this does some matrix black magic
varEffects <- mvrnorm(nvariety, muAB, Sigma_var)#get overall slopes and intercepts for each year 

alphaVarObs <- rep(varEffects[,1], each = nrep) # replicate the value so i can a variety specific mean for each observation

betaVarObs <- rep(varEffects[,2], each = nrep) # replicate the value so i can a variety specific slope for each observation

#other model parameters (no grouing)
sigma <-  1
eps <- rnorm(nObs , 0, sigma)

#make columns for teh name of the year, variety and day of the year 
varNamesRep <- rep(varNames, each = nrep)


#sigma <- sqrt(sigma2)


simLTEVar <- alphaVarObs  + betaVarObs * simTemps + eps

#combine into a single data table

simVarData <- data.frame(cbind(simTemps, varNamesRep, simLTEVar))
str(simVarData)
simVarData[order(simVarData$varNames),]
simVarData$varNamesRep <- as.factor(simVarData$varNamesRep )


#plot simulated data 
#------------------------------

simVarData$varNamesCh <- as.factor(simVarData$varNames) # make a column of variety "names" for plotting 
simulatedDataPlot <- ggplot(data = simVarData, aes(x = simTemps, y = simLTEVar))
simulatedDataPlot + geom_point() +
	geom_smooth(aes(group = varNamesCh, colour = varNamesCh), method = "lm", se=F) +  
	theme_classic() +
	theme(legend.position = "none")+
	theme(text = element_text(size=20)) +
	xlab("simulated mean daily temp (C)") +
	ylab("simulated lte50 (C)")


#prior predictive checks 
#---------------------------------------------


#range of temperatures to simulate over
Ni <- 10 #number of repeat runs of the model 
nObs <- 30 # number fo temperature observations per simulation 
preTemps <- rnorm(30, -15, 20) # i think this is a sensible range of winter temps

#how do i chose a prior for the covarience structure???? I guess that it needs to be a negative number. Normal as well?
R <- rlkjcorr(1e4, K = 2, eta = 2)
plot(density(rlkjcorr(Ni, K = 2, eta = 2)[,1,2]))#what this prior looks like 

#1st level parameters 
alpha_g <-  rnorm(Ni, -15, 12)
beta_g <- rlnorm(Ni, 0 , 1)
sigma_g <- rnorm(Ni, 0, 5)#i wont need thsi to simulate mu values 

#second level parameters 
Rho_varNi <- rlkjcorr(Ni, K = 2, eta = 2)[,1,2]#prior for correlation matrix 
Sigma_vara <- rnorm(Ni, 0, 1) # standard deviation in intercepts for variety  
Sigma_varb <- rnorm(Ni, 0, 1) # standard deviation in slopes for variety

#make a dataframe for the outputs of random effects 
n_vars  <- 20 # number of random effects 
varietyEffecSim <- rep(c(1:n_vars), times = Ni)

repetition <- rep(1:Ni, each = n_vars)
randomEffectsPre <- data.frame(varietyEffecSim)
randomEffectsPre$alpha_v <- NA # this will be the alpha values for each varity, not teh grand alpha 
randomEffectsPre$rep <- repetition
#randomEffectsPre$alpha_g <- rep(alpha_g , each = n_vars)
#randomEffectsPre$beta <- rep(beta, each = n_vars) Thsi will come from teh below loop so each variety has a different slope
#randomEffectsPre$x <- rep(preTemps, times = n_vars)


#loop for making 100 different repetitions of the model, where there are 20 varieties and 20 years 
#i then repeat the values so that there is a combination of variety and year for each value 
for (counter in 1:Ni){

	#counter <- 1

	#Variety effect on alpha and beta  
	muABs <- c(alpha_g[counter], beta_g[counter])#combine two grand effects

	sigma_varas <- Sigma_vara[counter]# standard deviation in intercepts for year 
	sigma_varbs <- Sigma_varb[counter] # standard deviation in slopes for year 
	rho_vars <- Rho_varNi[counter] # correlation between intercept and slope. Lower intercepts (more cold hardy) should have steeper slopes

	sigmas_vars <- c(sigma_varas, sigma_varbs) # combine sigma values into a vector
	Rho_vars <- matrix(c(1, rho_vars, rho_vars, 1), nrow = 2) # correlation matrix
	Sigma_vars <- diag(sigmas_vars) %*% Rho_vars %*% diag(sigmas_vars) # this does some matrix black magic
	varEffectss <- mvrnorm(n_vars, muABs, Sigma_vars)#get overall slopes and intercepts for each year 

	# alpha values for each variety 
	randomEffectsPre$alpha_v[randomEffectsPre$rep == counter] <- varEffectss[,1]

	# beta values for each variety 
	randomEffectsPre$beta_v[randomEffectsPre$rep == counter] <- varEffectss[,2]

}

plot(simVarData$ simLTEVar ~ simVarData$simTemps, col = "grey74")

for (i in 1:length(randomEffectsPre$alpha_v)){
	abline(a = randomEffectsPre$alpha_v[i], b = randomEffectsPre$beta_v[i], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.8))
}

plot(density(randomEffectsPre$alpha_v))
plot(density(randomEffectsPre$beta_v))

#it is trying a few vrazy values, but generally ok i think?

#Run Stan Model on simulated data
#------------------------------------------


head(simVarData)

x <- I(simVarData$simTemps)
y <- simVarData$simLTEVar
N <- length(simVarData$simTemps)
variety <- as.integer(as.factor(simVarData$varNames ))
n_vars <- length(unique(varNamesRep))


#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame

stan_data4 <- list(N = N, x = x, y = y, n_vars = n_vars,  variety = variety )


#model with a full covarience structure and no non-centred parameterisation 
#-----------------------------------------------------------------------------

#this model is (i think?) exactly the same as the process i used to simulate data. 
#It has a covariance structure for teh partial pooling of variety on slope and 
#intercept. There is no non-centred parameterisation. 

#This model ddoesnt give fitting warnings (it used to when i included year) but has to have a high adapt_delta
#


fit6 <- stan(file = "slopeVarietyCov.stan", data = stan_data4, warmup = 3000, 
	iter = 8000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.97))

launch_shinystan(fit6)

post <- extract.samples(fit6)

str(post)
names(post)

plot(density(post$alpha_g))#-20 - ok estimate

plot(density(post$beta_g))#0.5 - underestimating this a bit, but maybe ok?

plot(density(data.frame(post$Rho)[,2]))#doing a good job i think? Should be -0.7

plot(density(data.frame(post$var_sigma)[,1])) #Partial pooling on intercept. should be 0.2. Doing a good job i think. 

plot(density(data.frame(post$var_sigma)[,2])) #Partial pooling on slope. should be 0.3. Doing a good job i think. 

plot(density(post$sigma_y))#should be 1. Maybe underestimating a bit, but I think its ok. 

#Predicted values
meanRealY <- colMeans(data.frame(post$realY))
SDRealY <- apply(data.frame(post$realY), 2, HPDI) 
str(SDRealY)

realYs <- data.frame(meanRealY)
head(realYs)
realYs$upperHPDI <- SDRealY[1,]
realYs$lowerHPDI <- SDRealY[2,]

#plot predicted valuea against temp
#black lines are the mean slopes, coloured ones are the HPDI varience 

plot(realYs$lowerHPDI ~ simVarData$simTemps,col="green", type = "l")
lines(realYs$upperHPDI ~ simVarData$simTemps,col="green")
lines(realYs$meanRealY ~ simVarData$simTemps,col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs$lowerHPDI, simVarData$simLTEVar,col="purple", type = "p", pch = 16)
points(realYs$upperHPDI, simVarData$simLTEVar,col="purple", pch = 16)
points(realYs$meanRealY, simVarData$simLTEVar,col="black", pch = 16)

#variety effects
varietyAlphas <- data.frame(post$a_b_variety[,,1])
meanVarietyAlpha <- colMeans(varietyAlphas)
varietyBetas <- data.frame(post$a_b_variety[,,2])
meanVarietyBeta <- colMeans(varietyBetas)

plot(meanVarietyAlpha ~ meanVarietyBeta)

#predictions for each variety 
mcmc_intervals(varietyAlphas) + geom_vline(xintercept = alpha, linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(varietyBetas)+ geom_vline(xintercept = betag, linetype="dotted", color = "grey") #intercepts 





#Other models:
#-----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------




#a model with a less complicated structure (no covarience) 
#------------------------------------------------------

#Thid model has non non-cented parameters and no covarience. Just partial pooling on 
#the intercept and slope for variety

#It is not giving me any fitting warnings at 8000 itterations, but not predicting 
#the values as well as I would hope. 


fit8 <- stan(file = "slope_varietySimple.stan", data = stan_data4, warmup = 2000, 
	iter = 8000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.95))

launch_shinystan(fit8)

post8 <- extract.samples(fit8)

str(post8)

plot(density(post8$alpha_g))#-20 - overestimating this 

plot(density(post8$beta_g))#0.5 - overestimating this 

plot(density(post8$sigma_alpha_v))#0.3 - estimating ok 

plot(density(post8$varbeta))#

plot(density(post8$sigma_beta_v))#0.2 - estimating ok

#attemp at non centred parametrisation to get model working better
#---------------------------------------------------------------------

#this model is very similar to model slope_varietySimple.stan, exept I added some
#non-parameterisation on the partial pooling around the intercept. I did this because
#I was getting transition errors and there was a banana shape without teh non-centred parameterisation

#This model gives no fitting warnings, and is doing an ok job of estimating parameters but not as good 
#as I would like 


fit10 <- stan(file = "noncentred_slope_varietySimple.stan", data = stan_data4, warmup = 2000, 
	iter = 4000, chains = 4, cores = 4, thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.80))

launch_shinystan(fit10)

post10 <- extract.samples(fit10)
str(post7)


plot(density(post10$alpha_g))#-20 - ok estimate

plot(density(post10$beta_g))#0.5 -underestimating this 

plot(density(post10$sigma_alpha_v))#0.3 - estimating ok 

plot(density(post10$varbeta))#

plot(density(post10$sigma_beta_v))#0.2 - estimating ok



#model with no correlation matrix and non-centred parameterisation on the sigma alpha var
#------------------------------------------------------------------------------------

#this is teh model i made woth partial pooling and a covariance structure for variety 
#on intercept and slope, and non-centred parameterisation. I dont know if it is needed
#because at the momment teh model without non-centred parameterisation is doing fine 

#this model has no divergent transitions warning or other warnings
#
fit9 <- stan(file = "nonCentre_slopeVarietyCov.stan", data = stan_data4, warmup = 2000, 
	iter = 4000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))

launch_shinystan(fit9)

fit9


post9 <- extract.samples(fit9)

str(post9)

plot(density(post9$alpha_g))#-20 - ok estimate

plot(density(post9$beta_g))#0.5 - underestimating this a bit, but maybe ok?

plot(density(data.frame(post9$Rho)[,2]))#dOverestimating this.  Should be -0.7

plot(density(data.frame(post9$var_sigma)[,1])) #Partial pooling on intercept. should be 0.2. Doing a good job i think. 

plot(density(data.frame(post9$var_sigma)[,2])) #Partial pooling on slope. should be 0.3. Overestimating a lot. 

plot(density(post9$sigma_y))#should be 1. Maybe underestimating a bit, but I think its ok. 


#Try with the real Data
#----------------------------
#----------------------------------------


#using the model with a covarience structure and no non-centred parameterisation 
#------------------------------------------------------------------------------

#this model now doesnt converge. There is a problem with teh partial pooling on slopes for varieties. (banana plot)
head(bhclim)

#remove na rows
bhclimClean2 <- bhclim[!is.na(bhclim$lte),]

#remove rows where no variety data given 
bhclimClean <- bhclimClean2[!bhclimClean2$variety == "",]

x2 <- I(bhclimClean$meanC)
y2 <- bhclimClean$lte
N2 <- length(bhclimClean$meanC)
variety2 <- as.integer(as.factor(as.character(bhclimClean$variety)))
n_vars2 <- length(unique(bhclimClean$variety))


#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame

stan_data_real <- list(N = N2, x = x2, y = y2, n_vars = n_vars2,  variety = variety2 )
str(stan_data_real)

fitReal_cov <- stan(file = "slopeVarietyCov.stan", data = stan_data_real, warmup = 2000, 
	iter = 6000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.97))

launch_shinystan(fitReal_cov)

#try without the covariance structure 
#------------------------------------------
#more divergent transitions, again with sigma around partial pooling of variety around slope

realFit_nocov <- stan(file = "slope_varietySimple.stan", data = stan_data_real, warmup = 2000, 
	iter = 8000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.95))

launch_shinystan(realFit_nocov)

#try with partial pooling, no covariance, and non-centred parameterisation for sigma_beta_var 
#--------------------------------------------------------------------------------------------

#this one fitted ok
#I think teh probelm is that there is no real variation between varieties, which is making it 
#really hard for the model

#teh estimated different alphas for varieties have no changed with the inclusion of patial pooling on slope 
realFit_nocov_bncp <- stan(file = "nonCentre_slopeVariety_beta.stan", data = stan_data_real, warmup = 2000, 
	iter = 8000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.95))

launch_shinystan(realFit_nocov_bncp)



postReal <- extract.samples(realFit_nocov_bncp)

str(postReal)
names(postReal)

plot(density(postReal$alpha_g))#-20 - ok estimate

plot(density(postReal$beta_g))#0.5 - underestimating this a bit, but maybe ok?

plot(density(postReal$sigma_alpha_v)) #Partial pooling on intercept. should be 0.2. Doing a good job i think. 

plot(density(postReal$sigma_beta_v)) #Partial pooling on slope. should be 0.3. Doing a good job i think. 

plot(density(postReal$sigma_y))#should be 1. Maybe underestimating a bit, but I think its ok. 

#Predicted values
meanRealY2 <- colMeans(data.frame(postReal$realY))
SDRealY2<- apply(data.frame(postReal$realY), 2, HPDI) 
str(postReal)

realYs2 <- data.frame(meanRealY2)
head(realYs)
realYs2$upperHPDI <- SDRealY2[1,]
realYs2$lowerHPDI <- SDRealY2[2,]

#plot predicted valuea against temp
#black lines are the mean slopes, coloured ones are the HPDI varience 

plot(realYs2$lowerHPDI ~ bhclimClean$meanC,col="green")
points(realYs2$upperHPDI ~ bhclimClean$meanC,col="green")
points(realYs2$meanRealY ~ bhclimClean$meanC,col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs2$lowerHPDI, bhclimClean$lte,col="purple", type = "p", pch = 16, cex.lab=1.5, cex.axis=1.5,
	xlab = "Predicted LTE50 (C)", ylab = "Observed LTE50 (C)")
points(realYs2$upperHPDI, bhclimClean$lte,col="purple", pch = 16)
points(realYs2$meanRealY, bhclimClean$lte,col="black", pch = 16)

#variety effects
meanVarietyAlpha2 <- colMeans(postReal$alpha_var)
meanBetaVar <- data.frame(postReal$zb_variety * mean(postReal$sigma_beta_v))
meanVarietyBetaZ <- colMeans(meanBetaVar)
str(meanVarietyBetaZ)
plot(meanVarietyAlpha2 ~ meanVarietyBetaZ)

#predictions for each variety 
SpeciesAlphas <- data.frame(postReal$alpha_var)
names(SpeciesAlphas) <- levels(as.factor(as.character(bhclimClean$variety)))
names(meanBetaVar) <- levels(as.factor(as.character(bhclimClean$variety)))
mcmc_intervals(SpeciesAlphas) + geom_vline(xintercept = mean(postReal$alpha_g), linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(meanBetaVar ) + geom_vline(xintercept = mean(postReal$alpha_b), linetype="dotted", color = "grey") #intercepts 

