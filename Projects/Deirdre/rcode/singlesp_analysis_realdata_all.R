# Started July 24, 2020

#Writting a Stan model to test for differences across individual speices phenology 


# Model: 
#   doy_i = change in phenology*year + base doy

# Update Nov 22, Lizzie found an issue with my model code and suggested doing more PPC plots and comparisons
#see line ~143
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
require(shinystan)
require(reshape2)
require(bayesplot)
require(ggplot2)


#########################################################
names(dat1)
dat <- allnew

#allnew$studyid

#removing all the extra columns for now, may want this additional information later

##########################################################################################
# BUT I am only going to start by working with the Kharouba data, so I will subset it out 
##########################################################################################
# BUT I am only going to start by working with the Kharouba data, so I will subset it out 
most <- subset(dat.nodups, datasource != "thackeray")

specieschar.hin <- aggregate(most["doy"], most[c("studyid", "species")], FUN=length) #this is just creating a list with each species for each study, type.of.action and species level - 322

most <- most[,c("year", "doy", "species", "phenophase", "studyid", "datasource", "yr1980","species.name")]

#moved the line where I added species as a factor out of the cleaning code since I think it was doing something odd.
most$species.fact<-as.numeric(as.factor(most$species.name))
unique(most$species.fact)
#without the Thackeray data - 224 species across 118 studies, but 321 unique study/species combinations (ie some studies have multiple species)
head(most)
final <- most[complete.cases(most),] 
nrow(most)-nrow(final) # none removed! 

head(final)
##########################################################################################
# I have to change species to a number 
# since there is so much data,I am only starting with the kharouba data, some of this code is take right from Lizzie and Heather's code (synchmodel.R)

final$species.fact<-as.numeric(as.factor(final$species.name))

#specieschar.hin<- aggregate(final["doy"], final[c("studyid", "species")], FUN=length) #this is just creating a list with each species for each study, type.of.action and species level

##this is just creating a list with each species for each study, type.of.action and species level

datalist<-with(final,
               list( N=nrow(final),
                     Nspp =length(unique(final$species.name)),
                     ypred = doy,
                     species =species.fact,
                     year = yr1980)
)
datalist$Nspp 
unique(datalist$species)

length(unique(final$species.fact))
###################################################

# running stan model, model still a work in progress, but currently it has random slopes and intercepts for species
mdl<-stan("Stan/singlesp_randslopes_goo.stan",
          data= datalist,
          iter=2000, chains=4)

mdlslpint<-stan("Stan/singlesp_randslopesint_goo.stan",
                data= datalist,
                iter=2000, chains=4)

# mdlcov<-stan("Stan/singlesp_randslopes_goo_wcov.stan",
#     data= datalist,
#     iter=4000, chains=4)

print(mdlcov, pars = c("mu_a","sigma_a","mu_b","sigma_b", "sigma_y", "a","b","ypred_new","Rho")) 
print(mdlslpint,  pars = c("mu_b","sigma_b", "sigma_y", "a","b","ypred_new"))

save(mdl, file="Output/singlesp_randslopes.Rda")
save(mdlslpint, file="Output/singlesp_randslopespint.Rda")

#sm.sum <- summary(mdlcov, pars= c("Rho"))$summary
sm.sum
ssm<- as.shinystan(mdl)
launch_shinystan(ssm)

length(unique(hk$species))

# trying to make a pairs plot with just a subset of the variables
pairs(mdlcov, pars=c("mu_a","mu_b","sigma_a","sigma_b","sigma_y", "a[1]","b[1]", "a[50]","b[50]"))

# Saving the stan output
#mdl<-load("Output/singlesp_randslopes.Rda")
mdl.sm.sum <- summary(mdl)$summary

#mdlslpint<-load("Output/singlesp_randslopespint.Rda")
mdlslpint.sm.sum <- summary(mdlslpint)$summary

# extracting values
ext<-rstan::extract(mdl)
extslpint<-rstan::extract(mdlslpint)
#extcov<-rstan::extract(mdlcov)
########################################################################################
# Plotting predicted vs observed 



# PPC based on the vingette from https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html 


y<-final$doy

y.ext<-ext$ypred_new # I want this to be a matrix, which it is, with one element for each data point in y
y.slpint<-extslpint$ypred_new 
#y.cov<-extcov$ypred_new 

pdf(file="figures/mdl_densityplot_temp.pdf", width=4, height=4)
par(mfrow=c(1,2))
ppc_dens_overlay(y, y.ext[1:50, ])
ppc_dens_overlay(y, y.slpint[1:50, ])

dev.off()

pdf(file="figures/mdlslpint_densityplot_temp.pdf", width=4, height=4)
ppc_dens_overlay(y, y.slpint[1:50, ])
dev.off()
#ppc_dens_overlay(y, y.cov[1:50, ])

####################################################
# Exploring model output in greater detail with more appropriate PPC
####################################################

# extract the species-level intercept values from models B and C and plot them against each other (maybe with their 50% uncertainty intervals). Do it for the slopes too.

pdf(file="figures/comp_int_temp.pdf", width=10, height=10)
par(mfrow=c(2,1))
plot(mdl, pars="a", ci_level=0.5, outer_level=0.5,col="blue")
plot(mdlslpint, pars="a", ci_level=0.5, outer_level=0.5)
dev.off()

pdf(file="figures/comp_slp_temp.pdf", width=10, height=10)
plot(mdl, pars="b",ci_level=0.5, outer_level=0.5)
plot(mdlslpint, pars="b",ci_level=0.5, outer_level=0.5)
dev.off()

head(final)
plot(mdl.int,specieschar.hin$species)


###############################################################
mdl.slopes<-mdl.sm.sum[grep("b\\[", rownames(mdl.sm.sum)),c(1,6)] 
mdl.int<-mdl.sm.sum[grep("a\\[", rownames(mdl.sm.sum)),] 
# 
mdlslpint.slopes<-mdlslpint.sm.sum[grep("b\\[", rownames(mdlslpint.sm.sum)),c(1,6)] 
mdlslpint.int<-mdlslpint.sm.sum[grep("a\\[", rownames(mdlslpint.sm.sum)),] 
# 
mdl.a<-mdl.int[,1]
mdlslpint.a<-mdlslpint.int[,1]

#getting the intercept values to compare
mdl.a<-mdl.int[,1]
mdlslpint.a<-mdlslpint.int[,1]

#getting the slope values to compare
mdl.b<-mdl.slopes[,1]
mdlslpint.b<-mdlslpint.slopes[,1]

# Plotting these 1:1 comparisons
pdf(file="figures/comp_int_slp.pdf", width=10, height=10)
par(mfrow=c(2,1))
plot(mdl.a, mdlslpint.a)
plot(mdl.b, mdlslpint.b)
dev.off()

length(mdl.a)
length(mdlslpint.a)

# ggplot kept giving me an error message that it needed to be in dataframes
mdl.int<-as.data.frame(mdl.int)
mdlslpint.int<-as.data.frame(mdlslpint.int)

mdl.slopes<-as.data.frame(mdl.slopes)
mdlslpint.slopes<-as.data.frame(mdlslpint.slopes)

x_axis <- factor(row.names(mdl.int))
mdl.int$xmin <-mdl.int$`25%` 
mdlslpint.int$xmax <-mdlslpint.int$`75%` 


ggplot()+
  geom_point(data=mdl.int,aes(y=row.names(mdl.int), x=mean), color="salmon")+
  geom_point(data=mdlslpint.int,aes(y=row.names(mdlslpint.int), x=mean), color="darkgreen") +
  labs(x="doy", y="Species")

ggplot()+
  geom_point(data=mdl.slopes,aes(y=row.names(mdl.slopes), x=mean), color="salmon")+
  geom_point(data=mdlslpint.slopes,aes(y=row.names(mdlslpint.slopes), x=mean), color="darkgreen")+
  labs(x="doy", y="Species")


###################################################################################
#2 plot the histograms of the extracted species-level intercept values from models B and C
pdf(file="figures/comp_hist_int_temp.pdf", width=10, height=10)
par(mfrow=c(2,2))
hist(ext$a)# pooling all the iteractions for each species 
hist(ext$a[250,]) #
hist(extslpint$a)# pooling all the iteractions for each species 
hist(extslpint$a[1,]) #
dev.off()


####################################################################################
#3.  do a better PPC. I suggest the following (I would do it in R for now, you can work with Geoff later to move it to Stan):
#  - Grab the species-level intercept and slope values, then use those and the sigma_y parameter to generate new y data (and compare to real y data).
#- Run a greater diversity of PPCs, not just the y value one you have, but I also suggest some comparisons to simple linear models (one more per species x dataset) to get a feel for what models B and C are doing. I did these for Heather's model and they were helpful to look at.
studyid321<-subset(final, studyid=="Mills_2005_Ibis")$yr1980

# make 223-225 temp obj and sample once in a for loop,
int<-sample(ext$a[,224], 100) #samples from the posterior will be used to make predictions for sp 1
slp<-sample(ext$b[,224], 100)
smp_sigmay<-sample(ext$sigma_y, 100)

mu_temp<-int[1]+slp[1]*sp224 #vector of change over time, average

ypred<-rnorm(n=length(mu_temp), mu_temp,smp_sigmay[1])
#can make the plot ouside the loop and then each loop would populate it with the pts 
plot( sp321,ypred) #predicted phenology over time including uncertainty
points(mu_temp, type="l") # line is the average responses for a single species spec

####################################################################################################
sp227<-subset(final, studyid=="Beebee_1995_Nature")$yr1980

# make 223-225 temp obj and sample once in a for loop,
int<-sample(ext$a[,227], 100) #samples from the posterior will be used to make predictions for sp 1
slp<-sample(ext$b[,227], 100)
smp_sigmay<-sample(ext$sigma_y, 100)

mu_temp<-int[1]+slp[1]*sp227 #vector of change over time, average

ypred<-rnorm(n=length(mu_temp), mu_temp,smp_sigmay[1])
#can make the plot ouside the loop and then each loop would populate it with the pts 
plot( sp227,ypred) #predicted phenology over time including uncertainty
points(mu_temp, type="l") # line is the average responses for a single species spec
####################################################################################################
Nspp <-length(unique(final$species))

# extracting mean values:
sigma_y <- mean(ext$sigma_y) # from stan output = ext for Kharouba model
sigma_b <- mean(ext$sigma_b) 
mu_b <- mean(ext$mu_b) 

b <- rnorm(J, mean=mu_b, sd=sigma_b) # alert! perhaps should not set sd to sigma exactly? DL: what do you mean by this?
N=nrow(final)
species <- final$species.fact
year <- final$yr1980

ypred <- length(N) 
for (n in 1:N){
  s <- species[n]
  ypred[n] <- a[s] + b[s]*year[n]
}
y <- rnorm(N, ypred, sigma_y)

pdf(file="figures/time_series_mdl.pdf", width=10, height=10)
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01, mfrow=c(2,2))
plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Data from posterior means")
for (j in 1:5)
  lines(year[species==j], y[species==j], col=species[species==j])

plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Data from posterior means")
for (j in 1:10)
  lines(year[species==j], y[species==j], col=species[species==j])

plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Data from posterior means")
for (j in 215:224)
  lines(year[species==j], y[species==j], col=species[species==j])
hist(y, xlab="Day of year", main="Data from posterior means")
dev.off()

####################################################################

####################################################################
#4. do a mu plot as per the vingette: https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/

xlab<-"year"
ylab<-"doy"

pdf(file="figures/pltsp.pdf", width=10, height=10)
ggplot(final)+
  aes(x=yr1980, y=doy)+
  stat_smooth(method ="lm", se=FALSE)+
  facet_wrap("species.fact") +
  labs(x="year",y="doy") #labels dont work for now
dev.off()


spsub<-subset(final, species.fact<10)
ggplot(spsub)+
  aes(x=yr1980, y=doy)+
  stat_smooth(method ="lm", se=FALSE)+
  facet_wrap("species.fact") +
  labs(x="year",y="doy") #labels dont work for now

spsub<-subset(final, species.fact==20 |species.fact==41 |species.fact==75 |species.fact==116 |species.fact==184 |species.fact==200 )
ggplot(spsub)+
  aes(x=yr1980, y=doy)+
  stat_smooth(method ="lm", se=FALSE)+
  facet_wrap("species.fact") +
  labs(x="year",y="doy") #labels dont work for now

#complete and partial pooling
# start by fitting a separate line for each cluster of data

library(lme4)
library(dplyr)
library(tibble)

head(final)
final_temp<-subset(final, datasource== "kharouba")

final_temp<-subset(final, datasource== "cohen")

final_temp <- final_temp %>% 
  as_tibble() %>% 
  mutate(species.fact = as.character(species.fact))

final<- final %>% 
  as_tibble() %>% 
  mutate(species.fact = as.character(species.fact))

df_no_pooling <- lmList(doy ~ yr1980 | species.fact, final) %>% 
  coef() %>%  
  # species are stored as row-names. Make them an explicit column
  rownames_to_column("species.fact") %>% 
  rename(Intercept = `(Intercept)`, Slope_yrs = yr1980 ) %>% 
  add_column(Model = "No pooling")
head(df_no_pooling) # there are the same number of values as species, looks good

#complete pooling - a model using all the data together (pooled)
m_pooled<-lm(doy ~ yr1980, final)

# Repeat the intercept and slope terms to get one per sp
df_pooled <- tibble(
  Model = "Complete pooling",
  species.fact = unique(final$species.fact),
  Intercept = coef(m_pooled)[1], 
  Slope_yrs = coef(m_pooled)[2])
head(df_pooled)
# there are the same number as species with only one value

# Compare the two approaches in one plot
# Join the raw data so we can use plot the points and the lines.
df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
  left_join(final, by = "species.fact")


head(df_models)
spsub<-subset(df_models, species.fact<10)
spsub<-subset(df_models, species.fact==20 |species.fact==41 |species.fact==75 |species.fact==116 |species.fact==184 |species.fact==211 )
unique(df_models$species.fact)

pdf(file="figures/complete_nopooling.pdf", width=10, height=10)
 ggplot(spsub) + 
  aes(x = yr1980, y = doy) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_yrs, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("species.fact") +
  #  labs(x = xlab, y = ylab) + 
  #scale_x_continuous(breaks = 0:4 * 2) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top")
dev.off()

## Second subset ##########################
 spsub2<-subset(df_models, species.fact==1 |species.fact==45 |species.fact==175 |species.fact==19 |species.fact==65 |species.fact==201)
 
pdf(file="figures/complete_nopooling_2ndsub.pdf", width=10, height=10)
subplot<- ggplot(df_models) + 
   aes(x = yr1980, y = doy) + 
   # Set the color mapping in this layer so the points don't get a color
   geom_abline(aes(intercept = Intercept, slope = Slope_yrs, color = Model),
               size = .75) + 
   geom_point() +
   facet_wrap("species.fact") +
   #  labs(x = xlab, y = ylab) + 
   #scale_x_continuous(breaks = 0:4 * 2) + 
   # Fix the color palette 
   scale_color_brewer(palette = "Dark2") + 
   theme(legend.position = "top")
subplot
dev.off()

 pdf(file="figures/comp_pooling.pdf", width=10, height=10)
p_model_comparison <- ggplot(df_models) + 
  aes(x = yr1980, y = doy) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_yrs, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("species.fact") +
  #  labs(x = xlab, y = ylab) + 
  scale_x_continuous(breaks = 0:4 * 2) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top")

p_model_comparison
dev.off()

# fitting a simple mixed-effects model the lme4 way:partial pooling
finalk<-subset(final, datasource== "kharouba")

finalc<-subset(final, datasource== "cohen")


#m <- lmer(doy ~ yr1980 + (1 + yr1980 |species.fact), final) # model fails to converge
#m <- lmer(doy ~ yr1980 + (1 + yr1980 |species.fact), finalk) # model fails to converge
m <- lmer(doy ~ yr1980 + (1 |species.fact), final) # model fails to converge

arm::display(m)

# Make a dataframe with the fitted effects
df_partial_pooling <- coef(m)[["species.fact"]] %>% 
  rownames_to_column("species.fact") %>% 
  as_tibble() %>% 
  rename(Intercept = `(Intercept)`, Slope_yrs = yr1980) %>% 
  add_column(Model = "Partial pooling")

head(df_partial_pooling)
length(unique(df_partial_pooling$Intercept)) 

df_models <- bind_rows(df_pooled, df_no_pooling, df_partial_pooling) %>% 
  left_join(final, by = "species.fact")

plot(df_no_pooling$Intercept, df_part$Intercept)
# Plotting partial pooling
spsub2<-subset(df_models, species.fact==1 |species.fact==45 |species.fact==175 |species.fact==19 |species.fact==65 |species.fact==201)

pdf(file="figures/complete_nopooling_partial_2ndsub.pdf", width=10, height=10)
subplot<- ggplot(spsub2) + 
  aes(x = yr1980, y = doy) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_yrs, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("species.fact") +
  #  labs(x = xlab, y = ylab) + 
  #scale_x_continuous(breaks = 0:4 * 2) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top")
subplot
dev.off()

# want to visualize the shrinkage:
# Also visualize the point for the fixed effects
df_fixef <- data_frame(
  Model = "Partial pooling (average)",
  Intercept = fixef(m)[1],
  Slope_yrs = fixef(m)[2])

# Complete pooling / fixed effects are center of gravity in the plot
df_gravity <- df_pooled %>% 
  distinct(Model, Intercept, Slope_yrs) %>% 
  bind_rows(df_fixef)
df_gravity

df_pulled <- bind_rows(df_no_pooling, df_partial_pooling)
unique(df_pulled$Model)


pdf(file="figures/pulled_comp.pdf", width=10, height=10)
ggplot(df_pulled) + 
  aes(x = Intercept, y = Slope_yrs, color = Model) + 
  geom_point(size = 2) + 
  geom_point(data = df_gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = species.fact, color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = species.fact, color = NULL),
    data = df_no_pooling) +
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
dev.off()

ggplot(df_pulled) + 
  aes(x = Intercept, y = Slope_yrs, color = Model) + 
  geom_point(size = 2) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = species.fact, color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = species.fact, color = NULL),
    data = df_no_pooling) +
  geom_point(data = df_gravity, size = 5) +
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
# The plot has a few odd points, that I am curious to look into more:
# sp fact: 142, 176, 199, 57, 194
#sort(mdl.b) # sp 142 and 176 have the largest positive and negative slopes respectively ~7
sp142<-subset(final, species.fact=="142"); head(sp142) #Eurasian bullfinch
sp176<-subset(final, species.fact=="176"); head(sp176) #Common starling
sp83<-subset(final, species.fact=="83"); head(sp83) #Common starling

sp199<-subset(final, species.fact=="199"); head(sp199) #Dusky thrush
sp194<-subset(final, species.fact=="194"); head(sp194) #Alpine newt 

sp1<-subset(final, species.fact=="1"); unique(sp1$studyid) #Alpine newt 
sp175<-subset(final, species.fact=="175"); unique(sp175$studyid) #Alpine newt 

####################################################################
####################################################################

J <-length(unique(final$species))

# extracting mean values:
sigma_y_si <- mean(extslpint$sigma_y) # from stan output = ext for Kharouba model
sigma_b_si <- mean(extslpint$sigma_b) 
sigma_a_si <- mean(extslpint$sigma_a) 
mu_b_si <- mean(extslpint$mu_b) 
mu_a_si <- mean(extslpint$mu_a)

a_si <- rnorm(J, mean=mu_a_si, sd=sigma_a_si) 
b_si <- rnorm(J, mean=mu_b, sd=sigma_b) # alert! perhaps should not set sd to sigma exactly? DL: what do you mean by this?
N=nrow(final)
species <- final$species.fact
year <- final$yr1980

ypred <- length(N) 
for (n in 1:N){
  s <- species[n]
  ypred[n] <- a_si[s] + b_si[s]*year[n]
}
y_si <- rnorm(N, ypred, sigma_y)

pdf(file="figures/time_series_mdlslpint.pdf", width=10, height=10)
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01, mfrow=c(2,2))
plot(range(year), range(y_si), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Data from posterior means")
for (j in 1:5)
  lines(year[species==j], y_si[species==j], col=species[species==j])

plot(range(year), range(y_si), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Data from posterior means")
for (j in 15:20)
  lines(year[species==j], y_si[species==j], col=species[species==j])

plot(range(year), range(y_si), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Data from posterior means")
for (j in 215:224)
  lines(year[species==j], y_si[species==j], col=species[species==j])
hist(y_si, xlab="Day of year", main="Data from posterior means")
dev.off()


pdf(file="figures/time_series_comp.pdf", width=10, height=10)
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01, mfrow=c(2,2))
plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",bty="l")
lines(year[species==1], y[species==1], col=species[species==1])
lines(final$yr1980[final$species.fact==1], final$doy[final$species.fact==1], col="darkgreen")

plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",bty="l")
lines(year[species==25], y[species==25], col=species[species==25])
lines(final$yr1980[final$species.fact==25], final$doy[final$species.fact==25], col="darkgreen")

plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",bty="l")
lines(year[species==224], y[species==224], col=species[species==224])
lines(final$yr1980[final$species.fact==224], final$doy[final$species.fact==224], col="darkgreen")
dev.off()
##### Compare slopes for non-interacting and interacting species
#####
hkint<-subset(dat.nodups, datasource=="kharouba")
## Create table of known pairwise interactions
ids.list <- unique(hkint$intid)
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
fit.post <- rstan::extract(mdlcov)

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


smpl.spslopes <- melt(extslpint$b)
names(smpl.spslopes) <- c("iter", "species.fact","b")

smpl.spint <- melt(extslpint$a)
names(smpl.spint) <- c("iter", "species.fact","a")

#remove warm up, say first 1000
smpl.spint<- subset(smpl.spint, iter>1000)
smpl.spslopes<- subset(smpl.spslopes, iter>1000)

# Now i want to recombine these samples with the relevant grouping factors (ie interaction type, phenophase, environment etc)
spch.fm<-subset(specieschar.hin, select = c("studyid","species.name"))

groups<-subset(final, select = c("studyid","species","species.fact","phenophase","datasource"))
groups.nodups <- groups[!duplicated(groups),] # simplify so that there are only one for each combination of factors

final2<-merge(groups.nodups, smpl.spint, by = c("species.fact"), all.x=TRUE)

head(final2)
unique(final2$datasource)
# Saving the output
#write.csv(final, "Output/single.hinge.covmatrix.output.csv")
head(final2)
# Calculating the mean change in species phenologies per decade
mean.sp<-aggregate(final2["a"], final2[c("studyid", "species")], FUN=mean)
head(mean.sp)
plot(mean.sp$a, mean.sp$Species)

mean.sp<-merge(groups.nodups,mean.sp)
mean.sp$b.decade<-mean.sp$b*10

# What is the mean across all datasets
mean(smpl.spslopes$b)*10
# -3.37 ie advancing approximately 3 days, a little lower than the 4 days observed in Kharouba et al.

hist(mean.sp$b.decade, main="", xlab="change in phenology (days/decade)", breaks=20)

library(ggplot2)
ggplot(mean.sp, aes(x= b.decade, fill=phenophase)) +
  geom_histogram(binwidth=0.5, alpha = 0.5, position="identity")

head(mean.sp)
ggplot(mean.sp, aes(x= b.decade, y=studyid,color=phenophase)) +
  geom_point()

firstapp<-subset(mean.sp, phenophase=="first appearance")
ggplot(firstapp, aes(x= b.decade, y=studyid,color=datasource)) +
  geom_point()

repo<-subset(mean.sp, phenophase=="reproduction")
ggplot(repo, aes(x= b.decade, y=studyid,color=datasource)) +
  geom_point()

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

temp<-subst(final2, studyid=="Rubolini_et_al_2007")


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

require(dplyr)
temp<-dat.nodups %>%
  group_by("studyid","species") %>%
  summarise(mean=mean(dat.nodups$doy, na.rm=TRUE))

unique(dat.nodups$doy )                         

names(dat.nodups)

hk<-subset(dat.nodups, datasource=="kharouba");unique(hk$studyid)
unique.fact<-unique(hk[c(3,7),])

unique.fact<-unique(dat.nodups[,3:12])
hk2<-subset(unique.fact, datasource=="kharouba");unique(hk2$studyid)

write.csv(unique.fact, "trophic.environment.csv", row.names = FALSE)

library("bayesplot")
mcmc_areas(mdl, regex_pars = "b\\[[1-3]\\]",  prob = 0.5) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with medians and 80% intervals"
  )

###### Plots from synchmodel.R ######################
comppool <- lm(doy~yr1980, data=final)

nopool <- lm(doy~yr1980*species.fact, data=final)
uniquespp <- unique(final$species.name)
lmfits <- rep(NA, length(uniquespp))
for (eachsp in 1:length(uniquespp)){
  lmhere <- lm(doy~yr1980, data=subset(final, species.name==uniquespp[eachsp]))
  lmfits[eachsp] <- coef(lmhere)[2]
}


# looking at hinges
yearzero <- 1980
rawlong.nodups2<-subset(final, year <= yearzero)
rawlong.nodups2$count<-1

preccstudies <- rawlong.nodups2

hinges <- preccstudies
hinges$newyear <- hinges$year
hinges$newyear[hinges$newyear <= yearzero] <- yearzero

# loop!
# we'll do this for each intid, but since that will replicate species I will feed
# the loop the select the species


makehingeplot <- function(specieslist){
  for (i in seq_along(specieslist)){
    subby.nohinge <- subset(hinges, species==specieslist[i])
    
    # pdf(paste("figures/hingetimeseries/", "hingeat", yearzero, " ", intidhere, 
   #            specieslist[i], ".pdf", sep=""), height=4, width=6)
    par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
    plot(doy~yr1980, data=subby.nohinge, type="l")
    points(doy~yr1980, data=subby.nohinge, cex=0.6)
    abline((lm(doy~yr1980, data=subby.nohinge)))
    lines(doy~yr1980, data=subby.nohinge, col="red")
    abline((lm(doy~yr1980, data=subby.nohinge)), col="red")
   # dev.off()
  }
}
makehingeplot( c("Phytoplankton spp."))
