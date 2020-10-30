# Started July 24, 2020

#Writting a Stan model to test for differences across individual speices phenology 


# Model: 
#   doy_i = change in phenology*year + base doy
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Use 4 cores
options(mc.cores = 4)

if(length(grep("deirdreloughnan", getwd())>0)) { 
  setwd("~/Documents/github/Synchrony") 
} else if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/others/deirdre/Synchrony") 
} else{
  setwd("~/deirdre/Synchrony") # for midge
}

# Get the cleaned data
source("Rcode/combiningallphenodata.R")

# Get the cleaned data (relative path version)
#source("combiningallphenodata.R")


require(rstan)
#require(brms)
require(lme4)
require(truncnorm)
require(shinystan)
require(reshape2)

set.seed(1234)

#########################################################

dat<-allnew

#allnew$studyid

#removing all the extra columns for now, may want this additional information later
names(dat)
dat1 <- dat[,c("year", "doy", "species", "Trophic.level", "environment", "phenophase", "studyid", "datasource",
    "intid",  "spp", "type.of.action")]
head(dat1)


################################################################################
#Now we need to add the hinge part so the years are all relative to the start of climate change, 1980 or to be comparable to Kharouba et al. 1981
####################################################
# This is code taken directly from syncmodels.R that Lizzie and Heather wrote, it is removing duplicated species
# These species exist within the hk dataset still, but there are no additional duplicates it would seem

specieschar.wdups <- aggregate(dat1["doy"],
                               dat1[c("studyid", "species", "spp")], FUN=length)
specieschar <- aggregate(specieschar.wdups["doy"],
                         specieschar.wdups[c("studyid", "species")], FUN=length)
dupspp <- subset(specieschar, doy>1) # two species
specieschar.wdups[which(specieschar.wdups$species %in% dupspp$species),]
# delete duplicate species as spp1 (generally the shorter timeseries)
dat.nodups <- dat1[-(which(dat$species %in% dupspp$species &
                                    dat1$spp=="spp1")),]
# and order it!
dat.nodups <- dat.nodups[with(dat.nodups, order(species, year)),]
specieschar.formodel <- aggregate(dat.nodups["doy"],
                                  dat.nodups[c("studyid", "species", "type.of.action")], FUN=length)
head(dat.nodups)

#####################################################################################

#Studies that do have a hinge ie start before 1981, just want and idea for how many there actually are
# This is a very round about way, I am sure there is a better way of doing this!
pre1980studies<-subset(dat.nodups, year<1981) 
vlong<-list(unique(pre1980studies$studyid)) # There are apparently 100 studies that has data before 1980, meaning there are 27 that do not

#####################################################################################
#Using code from Kharouba et al. 2018 and Lizzie to calculate the hinge
dat.nodups$newyear<-dat.nodups$year

dat.nodups$newyear[which(dat.nodups$newyear<=1981)]<-1981

dat.nodups$yr1981 <- dat.nodups$newyear-1981

# Ok, the data is good to go! 

##########################################################################################
# BUT I am only going to start by working with the Kharouba data, so I will subset it out 
hk<-subset(dat.nodups, datasource=="kharouba")
names(hk)

specieschar.hin<- aggregate(hk["doy"], hk[c("studyid", "species", "intid")], FUN=length) #this is just creating a list with each species for each study, type.of.action and species level

hk <- hk[,c("year", "doy", "species", "phenophase", "studyid", "datasource", "yr1981","intid")]
unique(hk$intid)
hkintid<-hk[order(hk$intid),]

hk<-hk[complete.cases(hk),]


##########################################################################################
# I have to change species to a number 
# since there is so much data,I am only starting with the kharouba data, some of this code is take right from Lizzie and Heather's code (synchmodel.R)

hk$species.fact<-as.numeric(as.factor(hk$species))

hk
specieschar.hin<- aggregate(hk["doy"], hk[c("studyid", "species","intid")], FUN=length) #this is just creating a list with each species for each study, type.of.action and species level

# specieschar.hin<- aggregate(hk["doy"], hk[c("studyid", "species", "type.of.action", "spp")], FUN=length) #this is just creating a list with each species for each study, type.of.action and species level
hk <- hk[complete.cases(hk), ]
 datalist<-with(hk,
               list( N=nrow(hk),
                     Nspp =nrow(specieschar.hin),
                     ypred = doy,
                    species =species.fact,
                    year = yr1981
                    ))
 

###################################################

# running stan model, model still a work in progress, but currently it has random slopes and intercepts for species
 mdl<-stan("Stan/singlesp_randslopes_goo.stan",
           data= datalist
           ,iter=2000, chains=1, seed=1235, control = list(max_treedepth = 11))
 
 mdlcov<-stan("Stan/singlesp_randslopesint_goo.stan",
              data= datalist
              ,iter=2000, chains=1, seed=1235, control = list(max_treedepth = 11))
 
mdlcov<-stan("Stan/singlesp_randslopes_goo_wcov.stan",
          data= datalist
          ,iter=2000, chains=4, seed=1235, control = list(max_treedepth = 11))

#I don't know why this doesn't work or what these errors mean
#mdl_cov<- stan("Stan/synchrony1_notype_randslops_wcovar.stan", data=c("N","J","y","species","year","nVars","Imat"), iter=2000, chains=1)


print(mdlcov, pars = c("mu_b","sigma_b", "sigma_y", "a","b","ypred_new"
)) 

print(mdlcov, pars = c("mu_a","sigma_a","mu_b","sigma_b", "sigma_y", "a","b","ypred_new"
                    )) 

sm.sum <- summary(mdlcov)$summary

ssm<- as.shinystan(mdlcov)
launch_shinystan(ssm)

length(unique(hk$species))

# trying to make a pairs plot with just a subset of the variables
pairs(mdl, pars=c("mu_a","mu_b","sigma_a","sigma_b","sigma_y", "a[1]","b[1]", "a[50]","b[50]"))

# Saving the stan output
saveRDS(mdl , "singlesp_randslope.rds")
saveRDS(mdlcov, "singlesp_randslope_cov.rds")


########################################################################################
# Plotting predicted vs observed 
extracted<-rstan::extract(mdl)

#head(extracted$a) # this matrix is the esti intercepts for each species
#extracted$a[,86] #this should give you 4000 intercepts for species 86
#if did the same thing for the slopes, would get 4000

#extracted$ypred_new[,86]
# # Working with estimates for species 1
# pred<-vector()
# 
# for (i in 1:10){
#   int<-sample(extracted$a[,1],1)
#   slope<-sample(extracted$b[,1],1)
#   sigma<-sample(extracted$sigma_y,1)
#   year<-sample(1:5,1) # Should this be the actual lenght of this study?
# 
#   mu_y<-int+slope*year+sigma
#   pred<-rbind(pred,mu_y)
# }
# 
# colnames(pred)[1] <- "mdl_pred"
# pred<-as.data.frame(pred)
# head(pred)


# PPC based on the vingette from https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html 

library("bayesplot")
library("ggplot2")


y<-hk$doy

yrep<-extracted$ypred_new # I want this to be a matrix, which it is, with one element for each data point in y

ppc_dens_overlay(y, yrep[1:50, ])
# oh, it doesn't look great. The model is under predicting

####################################################
# Thinking about whether to add covariance matrix, statistical rethinking suggests there should be a correlation between slopes and intercept that you might be able to see in a scatterplot
mdl_slopes<-sm.sum[grep("b\\[", rownames(sm.sum)),1] 
mdl_int<-sm.sum[grep("a\\[", rownames(sm.sum)),1] 

plot(mdl_int, mdl_slopes, xlab="sp intercepts", ylab="sp slopes")

##### New code - thanks Geoff!
##### Compare slopes for non-interacting and interacting species
#####

## Create table of known pairwise interactions
ids.list <- unique(hk$intid)
interact.stor <- matrix(NA, ncol = 2, nrow = length(ids.list))
for(i in 1:length(ids.list)){
    temp <- subset(hk, intid == ids.list[i]) #creates new subset for every pair
    temp2 <- unique(temp$species.fact) # no. sp share the same id number
    if(length(temp2) == 2){ #if= 2, then add the species names to i row in storage, creating col 1 = sp 1, col 2- sp2
        interact.stor[i, ] <- unique(temp$species.fact)
    } else{
        interact.stor[i, ] <- NA # if only one species has the id or 3+ have the id it gets skipped, can add to statment to see what Ids and how to include 
        #make a new loop for multiple int, could add coln to the matrix --> creates a ragged array, vect with diff sizes, could make empty list, for eac element give it all interacting pairs, work w/ this below in place of the matrix, for i in 1:length(list) -- then sample based on these elements
    }
}
interact.stor <- interact.stor[complete.cases(interact.stor), ]
## Create table of random interactions
random.stor <- matrix(NA, ncol = 2, nrow = nrow(interact.stor)) # if a list - change from matrix, syntax the same
for(i in 1:nrow(interact.stor)){
    random.stor[i, ] <-  sample(x = unique(hk$species.fact), size = 2, replace = FALSE) # fills in stor, want same number of random int, fill row with a sample of species ids, x= pool sampled from, samples not the same
    # adding unique so it is not weighted by the number of entries, this removes the effects of longer studies
}

## Extract posteriors from stan object
fit.post <- rstan::extract(mdl)

## Response to climate change is parameter b, so we subtract b's for random and interacting pair of species
n.postsamples <- 1000 # do 1000 subtractions for each pairing - from each pairing from the posterior
random.responses <- dim(nrow(interact.stor) * n.postsamples) # just creating an empty vector of this size
interact.responses <- dim(nrow(interact.stor) * n.postsamples)
for(i in 1:nrow(interact.stor)){
    temp.random1 <- sample(x = fit.post$b[, random.stor[i, 1]], size = n.postsamples, replace = TRUE) # for each row getting sampples, this first sample = sampled from the extracted slopes, fit.post$b = rows are iteractions, columns are levels, coln we want is the coln in the random.stor - slopes here are organized by sp id, i = first entry, sample from this coln, want 1000 sampels (ie n.postsampeles), repalce=T bc want a fresh sample each time
    temp.random2 <- sample(x = fit.post$b[, random.stor[i, 2]], size = n.postsamples, replace = TRUE)
    temp.interact1 <- sample(x = fit.post$b[, interact.stor[i, 1]], size = n.postsamples, replace = TRUE)
    temp.interact2 <- sample(x = fit.post$b[, interact.stor[i, 2]], size = n.postsamples, replace = TRUE)
    random.responses[(((i - 1) * n.postsamples) + 1):(i * n.postsamples)] <- temp.random1 - temp.random2
    interact.responses[(((i - 1) * n.postsamples) + 1):(i * n.postsamples)] <- temp.interact1 - temp.interact2
}

#[(((i - 1) * n.postsamples) + 1):(i * n.postsamples)] here we are indexing -- 255 and 256 - created a huge vector, now segmenting the very long vectors and ensuring the first entries corresp to first of interact.stor
# eg. first run i=1, so this becomes i-1=0, so assigning the first element all the way to 1000, assigning all these to temp 1-temp 2, if i=2, get 2-1 get 1001, 2*1000 so 1001 to 2000 get assigned a new 
#alt. could also make a matrix and populate the different columns of the matrix, with each column being a unique species pair
#note segmentation is need for certain stan models, an advantage of one big vect = can deal with different sized data, could have a different number for different pairings. 
print(random.stor[i,1])
print(random.stor[i,2])
# can mannually check if the columns are what you expect 

## Plotting - the range is the range of values and esti the density, fitting line to hist of data from xmin to xmax
plot.param <- list(
    xmin = -5, 
    xmax = 5,
    ymin = 0,
    ymax = 0.5,
    n = 1024)
plot(density(random.responses, from = plot.param[["xmin"]], to = plot.param[["xmax"]], n = plot.param[["n"]]), main = "Random vs interacting pairs", col = "blue", lwd = 2, xlab = "Difference in slopes", ylim= c(plot.param[["ymin"]], plot.param[["ymax"]]))
points(density(interact.responses, from = plot.param[["xmin"]], to = plot.param[["xmax"]], n = plot.param[["n"]]), col = "red", type = "l", lwd = 2)
abline(v = mean(random.responses), col = "blue")
abline(v = mean(interact.responses), col = "red")
legend("topright", c("Random", "Interacting"), col = c( "blue","red"), lty = "solid", lwd = 2)


##########################################################
#Need to extract the posteriors for different groups for comparisons
tot.sim<-extract(mdl)

dim(tot.sim$b) # I want to just extract the slopes but for every species
tot.sim$b[1500,] # if we just look at one iteration, see that there are 88 values, one for each species

smpl.spslopes <- melt(tot.sim$b)
names(smpl.spslopes) <- c("iter", "species.fact","b")

#remove warm up, say first 1000
smpl.spslopes<- subset(smpl.spslopes, iter>1000)

# Now i want to recombine these samples with the relevant grouping factors (ie interaction type, phenophase, environment etc)
spch.fm<-subset(specieschar.hin, select = c("studyid","species"))

groups<-subset(hk, select = c("studyid","species","species.fact","spp","intid","phenophase","type.of.action","environment"))
groups.nodups <- groups[!duplicated(groups),] # simplify so that there are only one for each combination of factors

final<-merge(groups.nodups, smpl.spslopes, by = c("species.fact"), all.x=TRUE)

head(final)

# Saving the output
#write.csv(final, "Output/single.hinge.output.csv")

# Calculating the mean change in species phenologies per decade
mean.sp<-aggregate(final["b"], final[c("studyid", "species")], FUN=mean)
mean.sp<-merge(groups.nodups,mean.sp)
mean.sp$b.decade<-mean.sp$b*10

# What is the mean across all datasets
mean(smpl.spslopes$b)*10
# -3.77 ie advancing approximately 4 days, same as found in the Kharouba code

hist(mean.sp$b.decade, main="", xlab="change in phenology (days/decade)", breaks=20)

library(ggplot2)
ggplot(mean.sp, aes(x= b.decade, fill=type.of.action)) +
  geom_histogram(binwidth=0.5, alpha = 0.5, position="identity")

pred<-subset(mean.sp, type.of.action=="predation")
comps<-subset(mean.sp, type.of.action=="competition")
polln<-subset(mean.sp, type.of.action=="pollination")
herb<-subset(mean.sp, type.of.action=="herbivory")
para<-subset(mean.sp, type.of.action=="parasitism") #hmm no data..
mutul<-subset(mean.sp, type.of.action=="mutualism") # none here either, what happened to it?

par(mfrow=c(2,3))
xlim <- c(-17,17)
hist(pred$b.decade, main="predators", xlab="change in phenology (days/decade)", breaks=20, xlim=xlim)
hist(comps$b.decade, main="competitors", xlab="change in phenology (days/decade)", breaks=20, xlim=xlim)
hist(polln$b.decade, main="pollination", xlab="change in phenology (days/decade)", breaks=20, xlim=xlim)
hist(herb$b.decade, main="herbivory", xlab="change in phenology (days/decade)", breaks=20, xlim=xlim)
# hist(para$b.decade, main="parasitism", xlab="change in phenology (days/decade)", breaks=20, xlim=xlim) 
# hist(mutul$b.decade, main="mutualism", xlab="change in phenology (days/decade)", breaks=20, xlim=xlim)


#################################################################################################
#Lets try running the model with different data
co<-subset(dat.nodups, datasource=="cohen")

co <- co[,c("year", "doy", "species", "phenophase", "studyid", "datasource", "yr1981")]
co<-co[complete.cases(co),]
##########################################################################################

co$species.fact<-as.numeric(as.factor(co$species))
co<-co[1:1000,]

#For cohen
specieschar.hin.cohen<- aggregate(co["doy"], co[c("studyid", "species")], FUN=length) #this is just creating a list with each species for each study, type.of.action and species level

datalistcohen<-with(co,
               list( N=nrow(co),
                     Nspp =nrow(specieschar.hin.cohen),
                     ypred = doy,
                     species =species.fact,
                     year = yr1981
               ))

N <- nrow(co)
y <- co$doy
J <- nrow(specieschar.hin.cohen)
species <- co$species.fact
year <- co$yr1981
nVars <-1
Imat <- diag(1, nVars)

###################################################

# running stan model, model still a work in progress, but currently it has random slopes and intercepts for species
mdlco<-stan("Stan/singlesp_randslopes_goo.stan",
          data= datalistcohen
          ,iter=2000, chains=1, seed=1235)

print(mdlco, pars = c("mu_b","sigma_b", "sigma_y", "mu_a", "b", "sigma_a")) 

pairs(mdlco, pars=c("mu_a","mu_b_sp","sigma_a","sigma_b_sp","sigma_y", "a[1]","b[1]"))

co_sm_sum <- summary(mdlco)$summary

ssmco<- as.shinystan(mdlco)
launch_shinystan(ssmco)


# Saving the stan output
#saveRDS(mdlco, "singlesp_randslope.rds")



## 
mdlco_slopes<-sm.sum[grep("b\\[", rownames(co_sm_sum)),1] 
mdl_intco<-sm.sum[grep("a\\[", rownames(co_sm_sum)),1] 

plot(mdl_intco, mdlco_slopes, xlab="sp intercepts", ylab="sp slopes")

###### Plotting the data #########################################################################################################

head(dat.nodups)

hist(dat.nodups$doy)

hist(co$doy)
############ GOOOOOOOOOOO #######################################################################
#below are random bits of code I tried using that I might want to return to later for inspiration

mdl_slopes<-sm.sum[grep("b\\[", rownames(sm.sum)),] 

#why are there 91 if there are only 88 species, is it simply because some species are in multiple studies? 
# Now need to create groups based on spp

dx <- gc[match(levels(gc$lab), gc$lab),] # with twig id in same order as the loop above

spp.one<-subset(co, spp =="spp1")
head(spp.one)
names(spp.one)

head(mdl_slopes)

p.samples<-matrix(0, ncol= 1000, nrow =107)

for(i in 0:1000){
  spch.fm$model<-smpl.spslopes$b[i,]
  final<-merge(groups.nodups, spch.fm, by.x= c("studyid","spp"), by.y = c("studyid","species"), all.x=TRUE)
  it1000[,(i-1000)] <- spch.fm$model
}

unique(smpl.spslopes)
