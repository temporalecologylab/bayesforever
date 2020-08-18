## vassal interphenophase with GDD simulated data. Started 12 Aug 2020 by Mira Garner.

# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/vassalphen/analyses/")
} else setwd("~/Documents/git/vassalphen/analyses/")

# setwd for class
#setwd("~/Documents/git/bayes2020/Projects/Mira/")


####---------------------------------------------------------------####

# Intercept only model - predict gdd for variety

# gdd = avar + error
# gdd ~ N(mu, sigma)
# mu = avar
# avar ~ N(mu.var, sig.var)
# sigma ~ N()


# 5 years monitoring 5 varieties

# First the climate ...
dailytemphere <- rnorm(365*5, 15, 8) # not realistic for seasonality but real N(14.75,6)
hist(dailytemphere)
yrz <- c(rep(2000, 365), rep(2001, 365), rep(2002, 365), rep(2003, 365), rep(2004, 365))
doy <- rep(1:365, 5)
climdat <- as.data.frame(cbind(yrz, dailytemphere, doy))
colnames(climdat)[colnames(climdat) == "yrz"] <- "yr"

# set up observations
V <- 5 # number of varieties
var.names <- c("Merlot", "Pinot noir", "Riesling", "Syrah", "Chardonnay")

# 5 vines observed per variety each year
obyr <- rep(c(1:5), each = 5) #observations in one year
obs <- rep(obyr, 5) #repeat the observations in each year for 5 years

# create variety column. Repeat variety names for 5 observations each year for 5 years
variety <- rep(var.names, 25)

# add dates for each observation
yr <- rep(2000:2004, each = 25)
bb.doy <- sample(80:120, 125, replace = TRUE) # budburst date
fl.doy <- sample(150:180, 125, replace = TRUE) # flowering date

bbdat <- data.frame(variety, obs, yr, bb.doy) #bb data
fldat <- data.frame(variety, obs, yr, fl.doy) #fl data

bbdat$stage <- "budburst"
fldat$stage <- "flowering"

colnames(bbdat)[colnames(bbdat) == "bb.doy"] <- "doy"
colnames(fldat)[colnames(fldat) == "fl.doy"] <- "doy"

# combine datasets??
phendat <- rbind(bbdat, fldat)

# calc gdd and add climdat, threshold at 10 deg C
climdat$gddbase <- ifelse(climdat$dailytemphere >=10, climdat$dailytemphere, 0)
climdat$gdd <- ave(climdat$gddbase, climdat$yr, FUN=cumsum)

# add gdd to phenology datasets 
bgdat <- merge(bbdat, climdat)
fgdat <- merge(fldat, climdat)

# create the interphenophase dataset
bfgdd # the gdd between budburst and flowering


intph <- data.frame(yr, variety, bfgdd)

# Predict ggd for year
# GDD by interphenophase
#end

