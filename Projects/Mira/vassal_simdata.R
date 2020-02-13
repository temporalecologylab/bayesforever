## Simulating data and prior predictive check for Vassal interpheno phases.
## 10 Feb 2020 MG

#Budbreak-Flowering Mean and Standard Dev. COPIED: Nacho's script from Merged-data Pheno Intervals...

vassal.clean$DiffBudFlow<-difftime(vassal.clean$FLOW,vassal.clean$BB,units="days") 


nvar<-length(unique(vassal.clean$variety))
variety.mean.bud.flow<-as.data.frame(array(NA,dim=c(nvar,3)))
colnames(variety.mean.bud.flow)<-c("variety","mean Bud-Flow", "sd Bud-Flow")

for(i in 1:nvar){
  print(i)
  var.i<-unique(vassal.clean$variety)[i]
  subset.i<-subset(vassal.clean,variety==var.i)
  variety.mean.bud.flow[i,1]<-var.i
  variety.mean.bud.flow[i,2]<-as.numeric(mean(subset.i$DiffBudFlow,na.rm=T))
  variety.mean.bud.flow[i,3]<-as.numeric(sd(subset.i$DiffBudFlow,na.rm=T))
  
  
}

variety.mean.bud.flow #End Nacho's script

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
  e <- rnorm(length(alpha), 0, sigma)
  y <- alpha + e
  df <- data.frame(varname, alpha, e, y)
  priorList[[i]]<- df
  all.df <- do.call("rbind", priorList)
}
# for each variety, take all the y and make a boxplot
ggplot(all.df, aes(x= varname, y= y))+
  geom_boxplot()
