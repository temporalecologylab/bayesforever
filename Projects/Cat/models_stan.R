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
library(bayesplot) ## for plotting
library(egg) ## for plotting
library(shinystan)
library(rstanarm)
library(rstan)
library(brms)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

source("source/stan_utility.R")

ws <- read.csv("output/fakedata_ws.csv")
hobo <- read.csv("output/fakedata_hl.csv")


datalist.ws <- with(ws, 
                       list(y = gdd, 
                            sp = as.numeric(as.factor(species)),
                            N = nrow(ws),
                            n_sp = length(unique(ws$species))
                       )
)



ws_fake = stan('stan/weather_stan_normal.stan', data = datalist.ws,
                   iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 

if(FALSE){
get_prior(gdd ~ 1 + (1|species), data=ws)

ws_fake_brms <- brm(gdd ~ 1 + (1|species),  
               data=ws, chains=4,family=gaussian(), iter = 4000, warmup=2500,
               prior = prior(normal(700, 200), class = "Intercept") + prior(normal(0, 200), class = "sd"))

mcmc_areas(ws_fake_brms)
pp_check(ws_fake_brms)
}

check_all_diagnostics(ws_fake)

ws_fake.sum <- summary(ws_fake)$summary
ws_fake.sum[grep("mu_", rownames(ws_fake.sum)),]
ws_fake.sum[grep("sigma_", rownames(ws_fake.sum)),]

launch_shinystan(ws_fake)

datalist.hl <- with(hl, 
                    list(y = gdd, 
                         sp = as.numeric(as.factor(species)),
                         N = nrow(hl),
                         n_sp = length(unique(hl$species))
                    )
)



hl_fake = stan('stan/hobo_stan_normal.stan', data = datalist.hl,
               iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 



check_all_diagnostics(hl_fake)

hl_fake.sum <- summary(hl_fake)$summary
hl_fake.sum[grep("mu_", rownames(hl_fake.sum)),]
hl_fake.sum[grep("sigma_", rownames(hl_fake.sum)),]

launch_shinystan(hl_fake)




################################################################################################
################################ URBAN MODELS NOW!! ############################################
################################################################################################
ws_urb <- read.csv("output/fakedata_ws_urb.csv")
hobo_urb <- read.csv("output/fakedata_hl_urb.csv")

datalist.wsurb <- with(ws_urb, 
                         list(y = gdd, 
                              tx = urban, 
                              sp = as.numeric(as.factor(species)),
                              N = nrow(ws_urb),
                              n_sp = length(unique(ws_urb$species))
                         )
  )


#ws_urb_priorcheck = stan('stan/urbanmodel_stan_normal_weather.stan',
 #                  iter = 2000, warmup=1000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 

ws_urb_fake = stan('stan/urbanmodel_stan_normal_weather.stan', data = datalist.wsurb,
                       iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 


check_all_diagnostics(ws_urb_fake)

ws_urb_fake.sum <- summary(ws_urb_fake)$summary
ws_urb_fake.sum[grep("mu_", rownames(ws_urb_fake.sum)),]
ws_urb_fake.sum[grep("sigma_", rownames(ws_urb_fake.sum)),]

save(ws_urb_fake, file="~/Documents/git/microclimates/analyses/stan/ws_urban_stan.Rdata")

launch_shinystan(ws_urb_fake)


if(FALSE){
library(brms)
get_prior(gdd ~ urban + (urban | species), data=ws_urb)
ws_urb_brm <- brm(gdd ~ urban + (urban | species), data=ws_urb,
                  iter=2000, warmup=1500, chains=2, 
                  control=list(max_treedepth=15, adapt_delta=0.99),
                  prior = prior(normal(700, 200), class=Intercept) +
                                prior(normal(75, 25), class=b))
}



datalist.hlurb <- with(hobo_urb, 
                       list(y = gdd, 
                            tx = urban, 
                            sp = as.numeric(as.factor(species)),
                            N = nrow(hobo_urb),
                            n_sp = length(unique(hobo_urb$species))
                       )
)


#ws_urb_priorcheck = stan('stan/urbanmodel_stan_normal_weather.stan',
#                  iter = 2000, warmup=1000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 

hl_urb_fake = stan('stan/urbanmodel_stan_normal_hobo.stan', data = datalist.hlurb,
                   iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 



check_all_diagnostics(hl_urb_fake)

hl_urb_fake.sum <- summary(hl_urb_fake)$summary
hl_urb_fake.sum[grep("mu_", rownames(hl_urb_fake.sum)),]
hl_urb_fake.sum[grep("sigma_", rownames(hl_urb_fake.sum)),]

save(hl_urb_fake, file="~/Documents/git/microclimates/analyses/stan/hl_urban_stan.Rdata")

launch_shinystan(hl_urb_fake)


###############################################################################################################
################################ URBAN MODELS with REAL data NOW!! ############################################
###############################################################################################################

ws_urb <- read.csv("output/clean_gdd_bbanddvr.csv")
hobo_urb <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")

ws_urb$urban <- ifelse(ws_urb$type=="Harvard Forest", 0, 1)
#ws_urb <- ws_urb[(ws_urb$year>=2019),]

ws_urb.stan <- subset(ws_urb, select=c(gdd_bb, urban, genus, species))
ws_urb.stan <- ws_urb.stan[(complete.cases(ws_urb.stan)),]
ws_urb.stan$spp <- paste(ws_urb.stan$genus, ws_urb.stan$species, sep="_")

ws_urb.stan <- ws_urb.stan[(ws_urb.stan$gdd_bb<=1000),]

datalist.wsurb <- with(ws_urb.stan, 
                       list(y = gdd_bb, 
                            tx = urban, 
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(ws_urb.stan),
                            n_sp = length(unique(ws_urb.stan$spp))
                       )
)


#ws_urb_priorcheck = stan('stan/urbanmodel_stan_normal_weather.stan',
#                  iter = 2000, warmup=1000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 

wsall_urb_mod = stan('stan/urbanmodel_stan_normal_weather.stan', data = datalist.wsurb,
                   iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 


#check_all_diagnostics(ws_urb_mod)
check_all_diagnostics(wsall_urb_mod)

#ws_urb_mod.sum <- summary(ws_urb_mod)$summary
#ws_urb_mod.sum[grep("mu_", rownames(ws_urb_mod.sum)),]
#ws_urb_mod.sum[grep("sigma_", rownames(ws_urb_mod.sum)),]

wsall_urb_mod.sum <- summary(wsall_urb_mod)$summary
wsall_urb_mod.sum[grep("mu_", rownames(wsall_urb_mod.sum)),]
wsall_urb_mod.sum[grep("sigma_", rownames(wsall_urb_mod.sum)),]


my.pal <- rep(brewer.pal(n = 10, name = "Paired"), 4)
my.pch <- rep(15:18, each=10)
plot(gdd_bb ~ as.numeric(as.factor(spp)), col=my.pal[as.factor(spp)], pch=my.pch[as.factor(spp)], data = ws_urb.stan)
abline(h=mean(ws_urb.stan$gdd), lwd=3)

plot(density(ws_urb.stan$gdd))
abline(v = mean(ws_urb.stan$gdd), lwd = 2, col = "blue")


save(wsall_urb_mod, file="~/Documents/git/microclimates/analyses/stan/wsall_urban_mod.Rdata")

#launch_shinystan(ws_urb_fake)


hobo_urb <- read.csv("output/clean_gdd_bbanddvr_hobo.csv")

hobo_urb$urban <- ifelse(hobo_urb$type=="Harvard Forest", 0, 1)

hobo_urb.stan <- subset(hobo_urb, select=c(gdd_bb, urban, genus, species))
hobo_urb.stan <- hobo_urb.stan[(complete.cases(hobo_urb.stan)),]
hobo_urb.stan$spp <- paste(hobo_urb.stan$genus, hobo_urb.stan$species, sep="_")
hobo_urb.stan <- hobo_urb.stan[(hobo_urb.stan$gdd_bb<=1000),]

datalist.hobourb <- with(hobo_urb.stan, 
                       list(y = gdd_bb, 
                            tx = urban, 
                            sp = as.numeric(as.factor(spp)),
                            N = nrow(hobo_urb.stan),
                            n_sp = length(unique(hobo_urb.stan$spp))
                       )
)


#hobo_urb_priorcheck = stan('stan/urbanmodel_stan_normal_weather.stan',
#                  iter = 2000, warmup=1000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 

hobo_urb_mod = stan('stan/urbanmodel_stan_normal_hobo.stan', data = datalist.hobourb,
                  iter = 5000, warmup=2000, control=list(max_treedepth = 15,adapt_delta = 0.99)) ### 



check_all_diagnostics(hobo_urb_mod)

hobo_urb_mod.sum <- summary(hobo_urb_mod)$summary
hobo_urb_mod.sum[grep("mu_", rownames(hobo_urb_mod.sum)),]
hobo_urb_mod.sum[grep("sigma_", rownames(hobo_urb_mod.sum)),]

save(hobo_urb_mod, file="~/Documents/git/microclimates/analyses/stan/hobo_urban_mod.Rdata")




