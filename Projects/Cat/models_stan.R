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
library(shinystan)
library(rstan)

## Let's load some real data to check out.
setwd("~/Documents/git/bayes2020/Projects/Cat")

#source("source/stan_utility.R")

urb <- read.csv("output/testdata_urbmethod_intrxn.csv")

#yreal <- urb$gdd

datalist.gdd <- with(urb, 
                     list(y = gdd, 
                          urban = urban,
                          method = method,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(urb),
                          n_sp = length(unique(species))
                     )
)


#urbmethod_fake = stan('stan/urbanmethod_normal_ncp.stan', data = datalist.gdd,
 #                       iter = 2000, warmup=1000)#, control=list(adapt_delta=0.99)) ### 

urbmethod_fake_intrxn = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                      iter = 2000, warmup=1000)#, control=list(adapt_delta=0.99)) ### 
  
#check_all_diagnostics(ws_urb_buildfake)
  
urbmethod_fakesum <- summary(urbmethod_fake_intrxn)$summary
urbmethod_fakesum[grep("mu_", rownames(urbmethod_fakesum)),]
urbmethod_fakesum[grep("sigma_", rownames(urbmethod_fakesum)),]

yraw <- urb$gdd

launch_shinystan(urbmethod_fake_intrxn)  

############################################################################
######################## Now with real data ################################
############################################################################

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
ws$hobo <- 0

ws_urb <- subset(ws, select=c("id", "type", "gdd_bb", "hobo", "year", "genus", "species", "utah"))
ws_urb <- ws_urb[(ws_urb$type!="Common Garden"),]

hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
hobo$hobo <- 1

hobo_urb <- subset(hobo, select=c("id", "type", "gdd_bb", "hobo", "year", "genus", "species", "utah"))
hobo_urb <- hobo_urb[(hobo_urb$type!="Common Garden"),]

bball <- dplyr::full_join(ws_urb, hobo_urb)

bball$urban <- NA
bball$urban <- ifelse(bball$type=="Harvard Forest", 0, bball$urban)
bball$urban <- ifelse(bball$type=="Treespotters", 1, bball$urban)

bball.stan <- bball[(bball$year=="2019"),]
bball.stan <- subset(bball, select=c(gdd_bb, urban, hobo, genus, species))
bball.stan <- bball.stan[(complete.cases(bball.stan)),]
bball.stan <- bball.stan[!is.na(bball.stan$gdd_bb),]
bball.stan$spp <- paste(bball.stan$genus, bball.stan$species, sep="_")

bball.stan <- bball.stan[(bball.stan$gdd_bb<=1000),]

yraw <- bball$gdd_bb
yraw <- na.omit(yraw)

datalist.gdd <- with(bball.stan, 
                       list(y = gdd_bb, 
                            urban = urban, 
                            hobo = hobo,
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(bball.stan),
                            n_sp = length(unique(bball.stan$spp))
                       )
)


urbmethod = stan('stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                      iter = 1000, warmup=500, control=list(adapt_delta=0.90)) ### 

#check_all_diagnostics(ws_urb_buildfake)

urbmethod_sum <- summary(urbmethod)$summary
urbmethod_sum[grep("mu_", rownames(urbmethod_sum)),]
urbmethod_sum[grep("sigma_", rownames(urbmethod_sum)),]

yraw <- na.omit(bball$gdd_bb)

launch_shinystan(urbmethod)



############################################################################
######################## Now let's check out chilling ######################
############################################################################

chilling <- read.csv("output/testdata_chill_intrxn.csv")


datalist.chill <- with(chilling, 
                     list(y = chill, 
                          urban = urban,
                          method = method,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(chilling),
                          n_sp = length(unique(species))
                     )
)


chill_fake_intrxn = stan('stan/chill_normal_ncp_inter.stan', data = datalist.chill,
                             iter = 2000, warmup=1000)#, control=list(adapt_delta=0.99)) ### 

#check_all_diagnostics(ws_urb_buildfake)

chill_fakesum <- summary(chill_fake_intrxn)$summary
chill_fakesum[grep("mu_", rownames(chill_fakesum)),]
chill_fakesum[grep("sigma_", rownames(chill_fakesum)),]

yraw <- chill$chill

launch_shinystan(chill_fake_intrxn)  

############################################################################
########################## Real data chilling ##############################
############################################################################
### THIS MODEL IS NOT WORKING!!! FAR TOO MUCH VARIATION!!!!

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
ws$method <- 1

ws_urb <- subset(ws, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species", "utah"))
ws_urb <- ws_urb[(ws_urb$type!="Common Garden"),]

hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
hobo$method <- 0

hobo_urb <- subset(hobo, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species", "utah"))
hobo_urb <- hobo_urb[(hobo_urb$type!="Common Garden"),]

bball <- dplyr::full_join(ws_urb, hobo_urb)

bball$urban <- NA
bball$urban <- ifelse(bball$type=="Harvard Forest", 0, bball$urban)
bball$urban <- ifelse(bball$type=="Treespotters", 1, bball$urban)

bball.stan <- bball[(bball$year=="2019"),]
bball.stan <- subset(bball, select=c(utah, urban, method, genus, species))
bball.stan <- bball.stan[(complete.cases(bball.stan)),]
bball.stan <- bball.stan[!is.na(bball.stan$utah),]
bball.stan$spp <- paste(bball.stan$genus, bball.stan$species, sep="_")

yraw <- bball$utah
yraw <- na.omit(yraw)


datalist.chill <- with(bball.stan, 
                     list(y = utah, 
                          urban = urban, 
                          method = method,
                          sp = as.numeric(as.factor(spp)),
                          N = nrow(bball.stan),
                          n_sp = length(unique(bball.stan$spp))
                     )
)


chillmod = stan('stan/chill_normal_ncp_inter.stan', data = datalist.chill,
                 iter = 4000, warmup=2500, control=list(adapt_delta=0.90)) ### 

#check_all_diagnostics(ws_urb_buildfake)

chillmod_sum <- summary(chillmod)$summary
chillmod_sum[grep("mu_", rownames(chillmod_sum)),]
chillmod_sum[grep("sigma_", rownames(chillmod_sum)),]

yraw <- na.omit(bball.stan$utah)

launch_shinystan(chillmod)


############################################################################
## Quick BRMS model checks:

# 1) Is there more chilling at the Arboretum?
chillurb <- brm(utah ~ urban + (urban | species), data=bball)

# 2) Does more chilling mean less forcing is needed ?
bballchill <- bball[(complete.cases(bball)),]

chillgdd <- brm(gdd_bb ~ utah + (utah | species), data=bballchill)



############################################################################
######################## Finally, provenance model #########################
############################################################################

provdata <- read.csv("output/testdata_provmethod_intrxn.csv")

datalist.gdd <- with(provdata, 
                     list(y = gdd, 
                          prov = provenance,
                          method = method,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(provdata),
                          n_sp = length(unique(species))
                     )
)


provmethod_fake_intrxn = stan('stan/provmethod_normal_ncp_inter.stan', data = datalist.gdd,
                             iter = 2000, warmup=1000)#, control=list(adapt_delta=0.99)) ### 

#check_all_diagnostics(ws_urb_buildfake)

provmethod_fakesum <- summary(provmethod_fake_intrxn)$summary
provmethod_fakesum[grep("mu_", rownames(provmethod_fakesum)),]
provmethod_fakesum[grep("sigma_", rownames(provmethod_fakesum)),]

yraw <- provdata$gdd

launch_shinystan(provmethod_fake_intrxn)  

############################################################################
######################## Now with real data ################################
############################################################################

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
ws$method <- 1

ws_urb <- subset(ws, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species", "utah", "provenance"))
ws_urb <- ws_urb[(ws_urb$type!="Common Garden"),]

hobo <- read.csv("output/clean_gdd_chill_bbanddvr_hobo.csv")
hobo$method <- 0

hobo_urb <- subset(hobo, select=c("id", "type", "gdd_bb", "method", "year", "genus", "species", "utah", "provenance"))
hobo_urb <- hobo_urb[(hobo_urb$type!="Common Garden"),]

bball <- dplyr::full_join(ws_urb, hobo_urb)

bball$provenance <- as.numeric(bball$provenance)

bball.stan <- bball[(bball$year=="2019"),]
bball.stan <- subset(bball, select=c(gdd_bb, provenance, method, genus, species))
bball.stan <- bball.stan[(complete.cases(bball.stan)),]
bball.stan <- bball.stan[!is.na(bball.stan$gdd_bb),]
bball.stan$spp <- paste(bball.stan$genus, bball.stan$species, sep="_")

bball.stan <- bball.stan[(bball.stan$gdd_bb<=1000),]

yraw <- bball.stan$gdd_bb

datalist.gdd <- with(bball.stan, 
                     list(y = gdd_bb, 
                          prov = provenance, 
                          method = method,
                          sp = as.numeric(as.factor(spp)),
                          N = nrow(bball.stan),
                          n_sp = length(unique(bball.stan$spp))
                     )
)


provmethod = stan('stan/provmethod_normal_ncp_inter.stan', data = datalist.gdd,
                 iter = 4000, warmup=2500, control=list(adapt_delta=0.90)) ### 

#check_all_diagnostics(ws_urb_buildfake)

provmethod_sum <- summary(provmethod)$summary
provmethod_sum[grep("mu_", rownames(provmethod_sum)),]
provmethod_sum[grep("sigma_", rownames(provmethod_sum)),]

launch_shinystan(provmethod)
