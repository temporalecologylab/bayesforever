## Simulating data and prior predictive check for Vassal interpheno phases.
## 10 Feb 2020 MG

#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# set working directory
setwd("~/Documents/git/bayes2020/Projects/Mira/")

# -----------------model with year and slope for each variety-------------------- #

# y.bbfl ~ N(u, sigma)
# u = a.var + B.var(year) #the model = for each variety, predict the budburst
# so y.bbfl = a.var + B.var(year) + error
# a.var ~ N(mean.var, sigma.var) #prior for variety intercept
# B.var ~ N(moo.Bvar, sig.Bvar)
# e ~ U(0,5)

#priors
mu3.var <- 65
sig3.var <- 5
mu.B <- 0
sig.B <- 0.5
e3 <- 10

# Simulating Data

#n.var <- length(unique(d2$variety)) #number of simulations/varieties #Mar2020 = 50
n.var <- 50
n.var
#n.obs <- 20 #give each variety 20 observations
#len <- n.var*n.obs

# list of years
# data in d2 starts 1956, ends 2013
year_0 <- 1980 #hinge year
sigma_yr <- 5
yr_per_var <- round(runif(n.var, 5, 57)) #number of years of data for each variety
variety <- rep(1:n.var, yr_per_var) # replicate each variety name the number of years of observations (yr_per_sp)
len.yr <- length(variety)
year <- rep(NA, len.yr)
for (v in 1:n.var){
  year[variety == v] <- rev(runif(1, 1961, 2013) - 1:(yr_per_var[v])) - year_0
}
dat.yrs <- data.frame(variety, year)


# make list of alpha for each variety - this alpha repeats for the number of years of observations
list.a3 <- round(rnorm(n.var, mu3.var, sig3.var))
a3 <- rep(list.a3, yr_per_var)
# make similar list of beta
list.b <- rnorm(n.var, mu.B, sig.B)
unique(list.b)
b <- rep(list.b, yr_per_var)
# from error distribution of model, pull a sigma for each individual alpha (20 different sigma values for each variety)
sig3.dat <- rnorm(len.yr, 0, e3)
# y = a + B*yr sigma
fin3.dat <- a3 + b*year + sig3.dat

# combine these lists into a dataframe
y3.dat <- cbind(dat.yrs, a3, b, sig3.dat, fin3.dat)

write.csv(y3.dat, "~/Documents/git/bayes2020/Projects/Mira/y3.dat.csv")

# plot simulated observations
library(ggplot2)
ggplot( data = y3.dat, aes(x= year, y= fin3.dat, col = variety))+
  geom_point() #as points
ggplot( data = y3.dat, aes(x= year, y= fin3.dat, group = variety, col = variety))+
  geom_line() #as lines

#subset ggplot
numz <- sample(1:50, 10)
mini.y.dat <- y3.dat[which(y3.dat$variety %in% numz), ]
ggplot(mini.y.dat, aes(x= year, y= fin3.dat))+
  geom_line() + facet_wrap(~ variety)

#-------------then for prior predictive check ----------------#

a3.moo <- rnorm(500, 65, 5) #prior for alpha_variety means
a3.sig <- runif(500, 0, 5) # prior for alpha_variety sigma
b3.moo <- rnorm(500, 0, 0.5) # prior for beta means
b3.sig <- runif(500, 0, 0.5) # prior for beta sigma
e3.all <- runif(500, 0, 10) # prior for model error

priorList <- list() #make list to put each dataframe in
for (i in 1:500){ #repeat what you did for the simulated data 500 times
  year_0 <- 1980 #hinge year
  yr_per <- round(runif(n.var, 5, 57)) #number of years of data for each variety
  var.sim <- rep(1:n.var, yr_per) # replicate each variety name the number of years of observations (yr_per_sp)
  len <- length(var.sim)
  yr.sim <- rep(NA, len)
  for (v in 1:n.var){
    yr.sim[var.sim == v] <- rev(runif(1, 1961, 2013) - 1:(yr_per[v])) - year_0
  }
  dat.yrsim <- data.frame(var.sim, yr.sim)
  
  list.alpha <- round(rnorm(n.var, a3.moo[i], a3.sig[i]))
  alpha <- rep(list.alpha, yr_per)
  list.beta <- rnorm(n.var, b3.moo[i], b3.sig[i])
  beta <- rep(list.beta, yr_per)
  e <- rnorm(len, 0, e3.all[i]) #simulate error
  y <- alpha +  beta*yr.sim + e
  icol <- rep(i, length(y)) #add what number i is to a new column so datasets can be ID'ed
  df <- data.frame(icol, var.sim, alpha, beta, yr.sim, e, y)
  priorList[[i]]<- df
}

all.df <- do.call("rbind", priorList)

# plot
library(ggplot2)
ggplot(all.df, aes(x= yr.sim, y= y, group = var.sim, col = var.sim))+
  geom_line()

#subset ggplot
numz <- sample(1:500, 20)
mini.all.df <- all.df[which(all.df$icol %in% numz), ]
ggplot(mini.all.df, aes(x= yr.sim, y= y, group = var.sim, col = var.sim))+
  geom_line() + facet_wrap(~ icol)


#from Cat - lmer and rstanarm code
library(lme4)

#lmer(fin3.dat ~ year + (year|variety), data = y3.dat)
lmer(fin3.dat ~ year + (1|variety), data = y3.dat) # does compile

# rstanarm
library(rstanarm)

stan_glmer(fin3.dat ~ year + (year|variety)) # same c++ error



#end