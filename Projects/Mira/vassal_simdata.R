## Simulating data and prior predictive check for Vassal interpheno phases.
## 10 Feb 2020 MG

#housekeeping


# set working directory
setwd("~/Documents/git/bayes2020/Projects/Mira/")

#-------------- INTERCEPT ONLY model --------------------#

# make dataframe with only complete cases to remove NAs - try to do this in a more controlled way later on
bbfl.cc <- variety.mean.bud.flow[complete.cases(variety.mean.bud.flow$`mean Bud-Flow`, variety.mean.bud.flow$`sd Bud-Flow`), ]

# y.bbfl ~ N(u, sigma)
# u = a.var #the model = for each variety, predict the budburst
# so y.bbfl = a.var + sigma
# a.var ~ N(mean.var, sigma.var) #prior for variety intercept
mean.var <- 70
sigma.UNIvar <- 5
sigma <- 50

# Simulating Data
n.var <- 35 #number of simulations

# make empty dataframe to put simulated data in
names.df <- c()
vardat <- c()

# for each variety, simulate 20 observations
# make list of each variety repeated 20 times (20 observations per variety)
names.df <- rep(bbfl.cc$variety, each = 20)
# make list of alpha for each variety - this alpha repeats 20 times (one for each variety observation)
here.dat <- rep(rnorm(n.var, mean.var, sigma.UNIvar), each = 20)
# from error distribution of model, pull a sigma for each individual alpha (20 different sigma values for each variety)
sig.dat <- rnorm(length(here.dat), 0, sigma)
# y = a + sigma
final.dat <- here.dat + sig.dat
# combine these lists into a dataframe
vardat <- data.frame(names.df, here.dat, sig.dat, final.dat)

# plot simulated observations
library(ggplot2)
ggplot(vardat, aes(x= names.df, y= final.dat))+
  geom_point() #as points
ggplot(vardat, aes(x=names.df, y = final.dat))+
  geom_boxplot()

# then for prior predictive check:

moo.sim <- rnorm(1000, 70, 15) #prior for variety means
sig.sim <- runif(1000, 0, 20) # prior for vareity sigma
sig.all <- runif(1000, 0, 30) # prior for model sigma

priorList <- list() #make list to put each dataframe in
for (i in 1:1000){ #repeat what you did for the simulated data 1000 times
  varname<- rep(bbfl.cc$variety, each = 20)
  alpha <- rep(rnorm(n.var,moo.sim[i], sig.sim), each = 20)
  e <- rnorm(length(alpha), 0, sigma) #simulate error
  y <- alpha + e
  icol <- rep(i, length(y)) #add what number i is to a new column so datasets can be ID'ed
  df <- data.frame(icol, varname, alpha, e, y)
  priorList[[i]]<- df
}

all.df <- do.call("rbind", priorList)

# for each variety, take all the y and make a boxplot
ggplot(all.df, aes(x= varname, y= y))+
  geom_boxplot()

#subset ggplot
numz <- sample(1:1000, 20)
mini.all.df <- all.df[which(all.df$icol %in% numz), ]
ggplot(mini.all.df, aes(x= varname, y= y))+
  geom_boxplot() + facet_wrap(~ icol)

#sigma priors too wide, too many samples?
# prior predictive check with year. 

####------------------------ HINGE MODEL -------------------------####
# hinge model with year, slope does not vary by variety

# model
#  y2 ~ N(u, sigma)
# u = a.var + B(yr.hinge) #the model = for each year, predict the budburst to flowering duration with unique intercept for each variety
# so y2 = a.var + B*year + e2
# a.var ~ N(mu2.var, sig2.var) #prior for variety intercept
# B ~ N(0,5)
# e ~ U(0,20)

# make dataframe with only rows that have budburst - flower difference calculated
d2 <- vassal.clean[which(vassal.clean$DiffBudFlow != "NA days"), ]

# looking at the data (with limits)
ggplot(d2, aes(x=year, y=DiffBudFlow, col = variety)) + geom_point() + ylim(0, 100) + theme(legend.position = "none")


#priors
mu2.var <- 65
sig2.var <- 5
mu.B <- 0
sig.B <- 0.1
e2 <- 10

# Simulating Data

n.var <- length(unique(d2$variety)) #number of simulations/varieties #Mar2020 = 50
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
list.a2 <- round(rnorm(n.var, mu2.var, sig2.var))
a2 <- rep(list.a2, yr_per_var)
# make similar list of beta
b1 <- rep(rnorm(1, mu.B, sig.B), len.yr)
unique(b1)
# from error distribution of model, pull a sigma for each individual alpha (20 different sigma values for each variety)
sig2.dat <- rnorm(len.yr, 0, e2)
# y = a + B*yr sigma
fin2.dat <- a2 + b1*year + sig2.dat

# combine these lists into a dataframe
y.dat <- cbind(dat, a2, b1, sig2.dat, fin2.dat)

# plot simulated observations
library(ggplot2)
ggplot( data = y.dat, aes(x= year, y= fin2.dat, col = variety))+
  geom_point() #as points
ggplot( data = y.dat, aes(x= year, y= fin2.dat, group = variety, col = variety))+
  geom_line() #as lines

#subset ggplot
numz <- sample(1:50, 10)
mini.y.dat <- y.dat[which(y.dat$variety %in% numz), ]
ggplot(mini.y.dat, aes(x= year, y= fin2.dat))+
  geom_line() + facet_wrap(~ variety)

#-------------then for prior predictive check ----------------#

a2.moo <- rnorm(500, 65, 2) #prior for alpha_variety means
a2.sig <- runif(500, 0, 2) # prior for alpha_variety sigma
# should just have on b ~ N(moo, sig)
#b2.moo <- rnorm(500, 0, 0.1) # prior for beta means
#b2.sig <- runif(500, 0, 0.1) # prior for beta sigma
e2.all <- runif(500, 0, 10) # prior for model error

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
  
  list.alpha <- round(rnorm(n.var, a2.moo[i], a2.sig[i]))
  alpha <- round(rep(list.alpha, yr_per))
  beta <- rep(rnorm(1, b2.moo[i], b2.sig[i]), len)
  e <- rnorm(len, 0, e2.all[i]) #simulate error
  y <- alpha +  beta*yr.sim + e
  icol <- rep(i, length(y)) #add what number i is to a new column so datasets can be ID'ed
  df <- data.frame(icol, var.sim, alpha, beta, yr.sim, e, y)
  priorList[[i]]<- df
}

all.df <- do.call("rbind", priorList)

# for each variety, take all the y and make a boxplot
ggplot(all.df, aes(x= yr.sim, y= y, group = var.sim, col = var.sim))+
  geom_line()

#subset ggplot
numz <- sample(1:500, 20)
mini.all.df <- all.df[which(all.df$icol %in% numz), ]
ggplot(mini.all.df, aes(x= yr.sim, y= y, group = var.sim, col = var.sim))+
  geom_line() + facet_wrap(~ icol)


# model with year and slope for each variety
# y.bbfl ~ N(u, sigma)
# u = a.var + B.var(year) #the model = for each variety, predict the budburst
# so y.bbfl = a.var + B.var(year) + sigma
# a.var ~ N(mean.var, sigma.var) #prior for variety intercept
# B.var ~ N(moo.Bvar, sig.Bvar)
# sigma ~ U(0,5)

# run rstanarm on simulated data and return parameters