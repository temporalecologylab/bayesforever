## Simulating data and prior predictive check for Vassal interpheno phases.
## 10 Feb 2020 MG

# make dataframe with only complete cases to remove NAs - try to do this in a more controlled way later on
bbfl.cc <- variety.mean.bud.flow[complete.cases(variety.mean.bud.flow$`mean Bud-Flow`, variety.mean.bud.flow$`sd Bud-Flow`), ]

# Simple, intercept-only model: 
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

# hinge model with year, slope does not vary by variety

# make dataframe with only rows that have budburst - flower difference calculated
d2 <- vassal.clean[which(vassal.clean$DiffBudFlow != "NA days"), ]

# Subtract 1980 from the year number to create hinge.
#yr.hinge <- d2$year - 1980
#d2.h <- cbind(d2, yr.hinge)
#head(d2.h)

# model
#  y2 ~ N(u, sigma)
# u = a.var + B(yr.hinge) #the model = for each year, predict the budburst to flowering duration with unique intercept for each variety
# so y2 = a.var + B*year + e2
# a.var ~ N(mu2.var, sig2.var) #prior for variety intercept
# B ~ N(0,5)
# e ~ U(0,20)

mu2.var <- 70
sig2.var <- 5
mu.B <- 0
sig.B <- 5
e2 <- 20

# Simulating Data

n.var <- length(unique(d2$variety)) #number of simulations/varieties #Mar2020 = 50
#n.obs <- 20 #give each variety 20 observations
#len <- n.var*n.obs
# list of years
year_0 <- 1980 #hinge year
sigma_yr <- 5
yr_per_var <- round(runif(n.var, 5, 40)) #number of years of data for each variety
var_yr <- rep(1:n.var, yr_per_var) # replicate each variety name the number of years of observations (yr_per_sp)
len.yr <- length(var_yr)
y.dat <- data.frame(names2.df, a2, b1, yr2, sig2.dat, fin2.dat)
year <- rep(NA, len.yr)
for (v in 1:n.var){
  year[var_yr == v] <- rev(2020 - 1:(yr_per_sp[v])) - year_0
}
dat <- data.frame(var_yr, year)

# make empty dataframe to put simulated data in
names2.df <- c()
vardat2 <- c()

# for each variety, simulate 20 observations
# make list of each variety repeated 20 times (20 observations per variety)
#names2.df <- rep(unique(d2$variety), each = 20)
# make list of alpha for each variety - this alpha repeats for the number of years of observations
list.a2 <- round(rnorm(n.var, mu2.var, sig2.var))
a2 <- rep(list.a2, yr_per_var)
# make similar list of beta
b1 <- rep(rnorm(1, mu.B, sig.B), len.yr)
# from error distribution of model, pull a sigma for each individual alpha (20 different sigma values for each variety)
sig2.dat <- rnorm(len.yr, 0, e2)
# y = a + B*yr sigma
fin2.dat <- a2 + b1*year + sig2.dat

# combine these lists into a dataframe
y.dat <- cbind(dat, a2, b1, sig2.dat, fin2.dat)
y.dat <- data.frame(var_yr, a2, b1, year, sig2.dat, fin2.dat)

# plot simulated observations
#plot(x = y.dat$yr2, y = y.dat$fin2.dat, xlab = "Year", ylab = "Duration", col = ???)
library(ggplot2)
ggplot( data = y.dat, aes(x= yr2, y= fin2.dat, col = names2.df))+
  geom_point() #as points

# then for prior predictive check:

moo2.sim <- rnorm(500, 70, 5) #prior for alpha_variety means
sig2.sim <- runif(500, 0, 5) # prior for alpha_variety sigma
b2.moo <- rnorm(500, 0, 10) # prior for beta means
b2.sig <- rnorm(500, 0, 10) # prior for beta sigma
sig2.all <- runif(500, 0, 30) # prior for model error

priorList <- list() #make list to put each dataframe in
for (i in 1:500){ #repeat what you did for the simulated data 500 times
  varname<- rep(bunique(d2.h$variety), each = 20)
  alpha <- rep(rnorm(n.var,moo2.sim[i], sig2.sim), each = 20)
  beta <- rep(rnorm(1, b2.moo, b2.sig), len)
  year # sample 5 consecutive years from vector of 1:40
  e <- rnorm(len, 0, sig2.all) #simulate error
  y <- alpha +  beta + e
  icol <- rep(i, length(y)) #add what number i is to a new column so datasets can be ID'ed
  df <- data.frame(icol, varname, alpha, beta, year e, y)
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

# model with year and slope for each variety
# y.bbfl ~ N(u, sigma)
# u = a.var + B.var(year) #the model = for each variety, predict the budburst
# so y.bbfl = a.var + B.var(year) + sigma
# a.var ~ N(mean.var, sigma.var) #prior for variety intercept
# B.var ~ N(moo.Bvar, sig.Bvar)
# sigma ~ U(0,5)

# run rstanarm on simulated data and return parameters