# Started July 24, 2020

#Writting a Stan model to test for differences across individual speices phenology 


# Model: 
#doy_i = change in phenology*year + base doy

# Update Nov 22, Lizzie found an issue with my model code and suggested doing more PPC plots and comparisons
#see line ~143

# December 30, 2020: added a few more new studies

# January 20, 2021: The partial pooling plots do appear to be pulling some species in an unexpected direction. But unfortuantley posting to discouse has not offered any additional insight, and writing the model in rstanarm resulted in the exact same issues. So moving forward for now
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls()) 
options(stringsAsFactors = FALSE)

# Use 4 cores
options(mc.cores = 4)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/Synchrony")
} else if(length(grep("Lizzie", getwd()) > 0)) {
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

unique(dat.nodups$datasource)
##############################################################################
# the thackeray data is odd and is slow to run, so I am subsetting it out for now
#most <- dat.nodups
most <- subset(dat.nodups, datasource != "thackeray")
unique(most$datasource)

specieschar.hin <- aggregate(most["doy"], most[c("studyid", "species")], FUN = length) #this is just creating a list with each species for each study, and species level - 887, 763 species

most <- most[,c("year", "doy", "species", "phenophase", "studyid", "datasource", "yr1980","species.name")]

#moved the line where I added species as a factor out of the cleaning code since I think it was doing something odd.

unique(most$species.fact)
#without the Thackeray data - 230 species across 118 studies, but 344 unique study/species combinations (ie some studies have multiple species) with Thackeray data - 716 species

final <- most[complete.cases(most), ]  # there are five rows of the seabird data that doesn't have a doy

final$species.fact <- as.numeric(as.factor(final$species.name))
final$study.fact <- as.numeric(as.factor(final$studyid))

nrow(most)-nrow(final) 


# Setting up the data for the bayesian model
datalist <- with(final,
               list( N = nrow(final),
                     Nspp = length(unique(final$species.name)),
                     Nstudy = length(unique(final$studyid)),
                     ypred = doy,
                     species = species.fact,
                     study = study.fact,
                     year = yr1980 ))

datalist$Nspp; datalist$Nstudy
unique(datalist$species)

# making a table to show how often a species is included in more than one study
# first I am making a dataset that creates a row for every species by study combination
species.study <- aggregate(dat.nodups ["doy"],
                 dat.nodups[c("studyid", "species")],
                 FUN = length)

# next I am using plyr to make a table by species that counts the number of studies that incldue that species 
dspstudy <- species.study %>%
  group_by(species) %>%
  summarise(no.studies = unique(length(studyid)))

###################################################

# running stan model, model still a work in progress, but currently it has random slopes and intercepts for species
# mdl <- stan("Stan/singlesp_randslopes_goo.stan",
#           data = datalist,
#           iter = 2000,
#           chains = 4)
# 
# mdlslpint <- stan("Stan/singlesp_randslopesint_goo.stan",
#                 data = datalist,
#                 iter = 4000, 
#                 chains = 4)

mdlstudy <- stan("Stan/singlesp_randslopesint_studyid.stan",
                  data = datalist,
                  iter = 4000, 
                  chains = 4)


save(mdl, file = "Output/singlesp_randslopes.Rda")
save(mdlslpint, file = "Output/singlesp_randslopespint_muadiffprior.Rda")

mdl.sm.sum <- summary(mdl)$summary
mdlslpint.sm.sum <- summary(mdlslpint)$summary
 
# extracting values
ext <- rstan::extract(mdl)
extslpint <- rstan::extract(mdlslpint)


# tryin with rstanarm to see if the 8000 dt also occur
require(rstanarm)

synch<-stan_lmer(formula = doy ~ yr1980 + (1 + yr1980 | species.fact) + (1 + yr1980 | study.fact), data =final)


#########################################################################################
# Plotting predicted vs observed 
# PPC based on the vingette from https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html 
# 
y <- final$doy

y.ext <- ext$ypred_new # I want this to be a matrix, which it is, with one element for each data point in y
y.slpint <- extslpint$ypred_new
#y.cov <- extcov$ypred_new 

# pdf(file = "figures/mdl_densityplot_temp.pdf", width = 4, height = 4)
# par(mfrow = c(1,2))
ppc.dens.overlay(y, y.ext[1:50, ])
ppc.dens.overlay(y, y.slpint[1:50, ])
# dev.off()

# pdf(file = "figures/mdlslpint_densityplot_temp.pdf", width = 4, height = 4)
# ppc_dens_overlay(y, y.slpint[1:50, ])
# dev.off()


