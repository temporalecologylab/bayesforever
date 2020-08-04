# Started July 24, 2020

#Trying to write a Stan model to test for differences across individual speices phenology 


# Model: 
#   doy_i = change in phenology*year + base doy
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

if(length(grep("deirdreloughnan", getwd())>0)) { 
  setwd("~/Documents/github/Synchrony") 
} else
  setwd("~/Documents/github/Synchrony")


rm(list=ls()) 
options(stringsAsFactors = FALSE)

require(rstan)
#require(brms)
require(lme4)
require(truncnorm)
require(shinystan)

set.seed(1234)

#########################################################
source("Rcode/generate_data.R")

# #############################################################
#Had an issue with the indexing, initially called species from a pool of 100, so some species were named sp 76, but the stan model was looking for species with values less than 62, so to fix this using the as.numeric(as.factor)

stor3$Speciesfactor<-as.numeric(as.factor(stor3$Species))
test<-stor3
head(test)
datalist<-with(test,
               list( N=nrow(test),
                     Nspp =length(unique(Species)),
                     ypred = DOY,
                    species =as.numeric(as.factor(Species)),
                    year = Year
                    ))
length(unique(test$Species))

head(test)
###################################################
#Having some issues with species, so checking a few things to try and understand why, see explanation above.
#datalist["species"]

#These two things are not the same
# tail(as.numeric(as.factor(stor3$Species)))
# tail(stor3$Species)
###################################################
# running stan model, model still a work in progress, but currently it has random slopes and intercepts for species
mdl<-stan("Stan/singlesp_randslopes_goo.stan",
          data= datalist
          ,iter=8000, chains=4, seed=1235)

print(mdl, pars = c("mu_a_sp","sigma_a_sp","mu_b_sp","sigma_b_sp", "sigma_y", "a", "b"))

g# quick based on sync model randomization (15 Oct 2018)
sm.sum <- summary(mdl)$summary

ssm<- as.shinystan(mdl)
launch_shinystan(ssm)


# Saving the stan output
#saveRDS(mdl, "singlesp_randslope.rds")

#Quick histogram of rhat values for the slopes
#stan_rhat(mdl, pars = "b")

# Thinking about the plots I would want to make and the different, useful, ways to visualize everything
#Most interested in how the slopes (changes in synchrony) are changing
mdl_slopes<-sm.sum[grep("b\\[", rownames(sm.sum)),] 

species<-sort(unique(test$Speciesfactor))
test<-cbind(mdl_slopes, species)
colnames(test)[colnames(test)=="species"] <- "Speciesfactor"

data1<-merge(test,stor3, by="Speciesfactor")
head(data1)

ggplot(data=data1, aes(x=Event, y=mean))+
  geom_bar()

mdl_slopes<-as.data.frame(mdl_slopes)
Speciesfactor<-seq(1:64)
mdl_slopes$Speciesfactor<-Speciesfactor
# creating a data frame that links these mean slope values with the species and the event types
data1<-data.frame(bind(mdl_slopes, Speciesfactor))


########################################
##### GOOOOO ###########################
########################################

head(test)

data<-data.frame(
  change = test$DOY, 
  sim = mu_b
)

ggplot(test, aes(Year, DOY)) + 
  geom_point(aes(colour = factor(Species)))

#read in the real data
obs<-read.csv("Input/Cohen_Kharouba_Thackeray_RIS.csv")
head(obs)
names(obs)
dat<-obs[,c(2,3,5)]
obsdata <- dat[complete.cases(dat), ] #loose 129 rows of data
datalist<-with(obsdata,
               list( N=nrow(obsdata),
                     Nspp =length(unique(species)),
                     ypred = doy,
                     species =as.numeric(as.factor(species)),
                     year = year
               ))
length(unique(test$Species))
mdl<-stan("Stan/singlesp_randslopes_goo.stan",
          data= datalist
          ,iter=8000, chains=4, seed=1235)
#
nsim<-length(test$Species)
alpha<-smpl.int
beta<-rslopes
year<-test$Year
sigma<-runif(100, 0, 2)

data<-data.frame(
  doy = test$DOY,
  sim = alpha[test$Species[i]]+beta[i]*year+ rnorm(nsim, mean=0, sd=sigma)
)

ggplot(data, aes(x=doy, y=sim)) +
  geom_point(alpha=0.1, color="green")
