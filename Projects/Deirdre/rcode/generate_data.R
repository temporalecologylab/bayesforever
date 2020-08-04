# Started July11, 2020

#First attempt at simple model for single species data trends ie understanding if we find the same trends at Cohen and Thackeray with this much larger dataset.


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
require(brms)
require(lme4)
require(truncnorm)

set.seed(1234)
##################################################
# Making test data
#this code is a frankenstein's monster of bits taken from FRST 507, D. Flynn, OSPREE and Geoff

para<-list(int = 100,
           sigma = 2,
           nstudy = 50,
           nspp = 100)

###################################################

# the length of the dataset will be determined by the number of years of observations per study:
#want to generate a random number of years for each data set
noyr<-sample(5:30,50,replace=T) #here I a generate a sequence of numbers from 5 (the min) to 30 (a long time series)
noyr


####################################################

# Generating a vector of random slopes for each unique species:
rslopes<-rnorm(100, 5, 2)
#this is the change in phenology per decade for each species, in each study, averg slope =5, dist of slopes=2 

#Next need to make varying intercepts
smpl.int<-rtruncnorm(100, 0, 365, 186, 20)
#using truncated norm bc they need to be between 0 and 365


##################################################

# now need to generate unique studies that vary in the lengths of their years of observation

nspp<-100
stor<-data.frame()
for (i in 1:para[["nstudy"]]){
  nspp<-sample(1:para[["nspp"]], 2, replace=T) #this is better bc now, then make the temp data.frame as follows
  nevent<-sample(1:5,50, replace=T)
  temp1 <- data.frame(Study = i,Species = nspp[1],Intercept = smpl.int[nspp[1]],Slope = rslopes[nspp[1]], Event = nevent[i])
  temp2 <- data.frame(Study = i,Species = nspp[2],Intercept = smpl.int[nspp[2]],Slope = rslopes[nspp[2]], Event = nevent[i])
  #Year = noyr[i]
  stor <- rbind(stor, rbind(temp1, temp2))
}
head(stor)
tail(stor)

smpl.int[1]
rslopes[1]

noyr

stor3<-data.frame()
for (i in 1:nrow(stor)){
  year<-seq(noyr[stor[i,"Study"]])
  temp<-dim(noyr[stor[i,"Study"]])
  for (j in 1:length(year)){
    temp[j]<-rnorm(n = 1, mean = stor[i, "Intercept"] + stor[i,"Slope"] * year[j], sd=para[["sigma"]]) #overall sigma/error
  } 
  temp3 <- data.frame(Study = stor[i,"Study"],Species = stor[i,"Species"],Event = stor[i,"Event"], Year = year, DOY=temp) # stor[1,"Study] and sp are single values, but year and doy have multiple, could include rep() 
  stor3<-rbind(stor3,temp3)
}
stor3

head(stor3)
tail(stor3)

stor4<-subset(stor3, Study==1 & Species ==51)
head(stor4)

summary(lm(DOY~Year, data=stor4)) #true int= 154.77, slope = 1.85, summary esti values of 153.46 and slope of 2.12, not bad considering how little data 
#to test a stan model with this, build one with random slopes and intercepts and 

head(stor)

#write.csv(stor3, "test_data.csv")
