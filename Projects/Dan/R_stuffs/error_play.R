####model cross talk
### Carrying uncertaintly across multiple models


#### Section I: When this matters ##############
#1) Using published data (meta-analyses, databases)
#2) Using prior model estimates for ``data" in subsequent models
#####################################################################

####Section  II: Why this matters ################
#1) Not accounting for uncertainty in estimate may result 
#in downstream over estimating effects
# especially in a hypothesis-testing (``statistical significance") framework.

#2) Alternatively, not accounting for this latent variation 
#may mask the true effect of  parameter of interest 

##########################################################################

#### Section III: Simulate a data frame
# goal you want to test a) effect of a treatment and b) how responses differ based another factor
# example a common garden testing phenological sensitivity, and how phenological sensitivity correlates with climate at home site

# To prove a point simulate data with lots of within population variability so means
#alone will have strong inference but error should blunt the spear.
# Below is my attempt

#### Section III: Solutions
# I'd like to generate a litst and example of options that vary based on coding ability and work
# I don't think the solution should be that everyone should need to code joint model in stan

#1) Extract posteriors from model 1, append them to data for model 2
#2) Measurement error or latent variable modeling (available in brms)
#3) Joint modeling
#4) Maybe categorize/factorize based on overlapping credible intervals?
#5) Other



rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
library(brms)
library(tidyverse)
library(tidybayes)
library(rstan)


nSpecies<-20 # number of populations


nph <- 200 # number of observations per species/phenological combination 
#Nph <- nSpecies * nForcing * nph * nPheno #overall number of observations
Nph <- nSpecies * nph # 20 obervations per species for phenological event and forcing 

#make a dataframe to keep things organised
phenoData <- data.frame(matrix(NA, Nph, 2))
names(phenoData) <- c("obs", "species")
phenoData$obs <- rep(1:20,nSpecies)
phenoData$species <- rep(c(1:nSpecies), each = nph)

beta<-seq(-4, -10, length = nSpecies)


df<-data.frame(realbeta=numeric())

for (j in c(1:length(beta))){
  realbeta<-rnorm(nph,beta[j],6)
  dathere<-data.frame(realbeta=realbeta)
  df <- rbind(df, dathere)
}
phenoData$beta<-df$realbeta


temperature<-rnorm(nph, 20, 1)
phenoData$temperature<-temperature

muPhenoSp <- 150# day 100 is mean phenological date
sigmaPhenoSp <- 5 # species generally vary around 2 days from mean 150

alphaPhenoSp <- rnorm(nSpecies, muPhenoSp, sigmaPhenoSp) 
phenoData$alphaPhenoSp <- rep(alphaPhenoSp, each = nph)

ePhenoSigma <- 2
ePheno <- rnorm(Nph, 0, ePhenoSigma) 
phenoData$ePheno <- ePheno

#"run" the full model to simulate data 
phenoData$yPhenoi <- phenoData$alphaPhenoSp + phenoData$beta * phenoData$temperature + phenoData$ePheno

mod1<-brm(yPhenoi~temperature+(temperature|species),data=phenoData)
summary(mod1)
coef(mod1)
fixef(mod1)

get_variables(mod1)
goober2<-mod1%>%
  spread_draws(r_species[species,term])%>%
  tidyr::spread(term,r_species) 
colnames(goober2)

ggplot(goober2,aes(temperature,as.factor(species)))+stat_eye()


dat2<-goober2 %>% dplyr::group_by(species) %>% dplyr::summarise(sensitivity=mean(temperature),sd=sd(temperature))

dat2$homesite<-rnorm(length(dat2$species),dat2$species,7)

ggplot(dat2,aes(species,homesite))+geom_point()

ggplot(dat2,aes(sensitivity,homesite))+geom_point()+geom_smooth(method="lm")


mod2<-brm(sensitivity~homesite,data=dat2)

mod3<-brm(sensitivity|mi(sd)~homesite,data=dat2,iter=4000)
dat2$homesitesd<-rnorm(length(dat2$homesite),.5,0.1)

mod3a<-brm(sensitivity|mi(sd)~me(homesite,homesitesd),data=dat2,iter=4000)

goober3<-left_join(goober2,dat2, by="species")

mod4<-brm(temperature~homesite,data=goober3)

fixef(mod2)
fixef(mod3)
fixef(mod3a)
fixef(mod4)

