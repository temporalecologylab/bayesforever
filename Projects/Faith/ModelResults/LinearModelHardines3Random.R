rm(list = ls())
#script started by Faith Jones on the 11th March 2020 

#running a mixed linear model of bud winter hardiness regressed against air temperature. There is grouping 
#on the intercept for year, variety and location 


setwd("/home/faith/Documents/github/bcvin/hardiness/analyses/")


#libraries
library(reshape2)
library(ggplot2)
library(rstan)
library(truncnorm) # truncated normal distribution 
library(rethinking) # for HPDI function 
library(scales) # alpha making plotting points translucent 
library(bayesplot)# nice posterior check plots 
library(tidyr)
library(dplyr)
library(shinystan)

#climate data
clim <- read.delim("input/envcanada_penticton.csv", skip=25, sep=",", header=TRUE)
clim$date <- as.Date(clim$Date.Time, format="%m/%d/%y")
clim$month <- format(clim$date, "%b")
clim$day<- format(clim$date,"%d")
head(clim)
climsm <- subset(clim, select=c("Year", "month","day", "Mean.Temp..C.", "Mean.Temp.Flag", "date", "Date.Time"))
names(climsm) <- c("Year", "month","day", "meanC", "meanC.flag", "date", "Date.Time")

# hardiness data
#--------------------------
budhardiness2012to13 <- read.csv("input/budhardiness2012to13.csv", header=TRUE)
budhardiness2013to14 <- read.csv("input/budhardiness2013to14.csv", header=TRUE)
budhardiness2014to15 <- read.csv("input/budhardiness2014to15.csv", header=TRUE)
budhardiness2015to16 <- read.csv("input/budhardiness2015to16.csv", header=TRUE)
budhardiness2016to17 <- read.csv("input/budhardiness2016to17.csv", header=TRUE)
budhardiness2017to18 <- read.csv("input/budhardiness2017to18.csv", header=TRUE) 
budhardiness2018to19 <- read.csv("input/budhardiness2018to19.csv", header=TRUE) 

bh12 <- melt(budhardiness2012to13, id.var=c("X2012...2013", "Variety"))
bh13 <- melt(budhardiness2013to14, id.var=c("X2013...2014", "Variety"))
bh14 <- melt(budhardiness2014to15, id.var=c("X2014...2015", "Variety"))
bh15 <- melt(budhardiness2015to16, id.var=c("X2015...2016", "Variety"))
bh16 <- melt(budhardiness2016to17, id.var=c("site", "Variety")) 
bh17 <- melt(budhardiness2017to18, id.var=c("site", "X2017...2018")) 
bh18 <- melt(budhardiness2018to19, id.var=c("site", "Variety")) 

nameshere <- c("site", "variety", "Date", "lte")
names(bh12) <- nameshere
names(bh13) <- nameshere
names(bh14) <- nameshere
names(bh15) <- nameshere
names(bh16) <- nameshere
names(bh17) <- nameshere
names(bh18) <- nameshere
bh12$years <- "2012to2013"
bh13$years <- "2013to2014"
bh14$years <- "2014to2015"
bh15$years <- "2015to2016"
bh16$years <- "2016to2017"
bh17$years <- "2017to2018"
bh18$years <- "2018to2019"

bhall.rbind <- rbind(bh12, bh13, bh14, bh15, bh16, bh17, bh18)

# remove the averages....
bhall <- subset(bhall.rbind, site!="Average Bud Hardiness (all sites, all varieties)")

# cleaning names
sort(unique(bhall$site))
bhall$site[bhall$site=="Naramata bench"] <- "Naramata Bench"
sort(unique(bhall$variety))
sort(unique(bhall$Date))
# cleaning dates
breakbyperiod <- strsplit(as.character(bhall$Date), ".", fixed=TRUE) 
bhall$Day <- unlist(lapply(breakbyperiod, function(x) x[1]))
bhall$month <- unlist(lapply(breakbyperiod, function(x) x[2]))
bhall$day <- unlist(lapply(strsplit(as.character(bhall$Day), "X", fixed=TRUE), function(x) x[2]))

# right, so now, we need to fix year!
bhall$year <- NA
bhall$year[bhall$years=="2012to2013" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2012
bhall$year[bhall$years=="2012to2013" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2013
bhall$year[bhall$years=="2013to2014" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2013
bhall$year[bhall$years=="2013to2014" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2014
bhall$year[bhall$years=="2014to2015" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2014
bhall$year[bhall$years=="2014to2015" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2015
bhall$year[bhall$years=="2015to2016" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2015
bhall$year[bhall$years=="2015to2016" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2016
bhall$year[bhall$years=="2016to2017" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2016
bhall$year[bhall$years=="2016to2017" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2017
bhall$year[bhall$years=="2017to2018" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2017
bhall$year[bhall$years=="2017to2018" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2018
bhall$year[bhall$years=="2018to2019" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2018
bhall$year[bhall$years=="2018to2019" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2019

# and make a useful df
bh <- subset(bhall, select=c("year", "month", "day", "variety", "lte", "site"))
head(bh)

#make a date column 
bh$Date <- paste(bh$month, bh$day, bh$year, sep = "/")
bh$Datestrptime <- as.POSIXct(strptime(bh$Date ,format="%b/%d/%Y"))
climsm$Datestrptime <- as.POSIXct(strptime(climsm$Date.Time,format="%m/%d/%y"))
#note, dates that are in strptime format rather than as,POSIXct cannot be used to merge 

#combine datasets
bhclim <- merge(bh, climsm, by.x = "Datestrptime", by.y = "Datestrptime") 
bhclim$Month_num <- format(as.Date(bhclim$Datestrptime), "%m")
bhclim$month_day <- as.numeric(paste(bhclim$Month_num, bhclim$day.x, sep = "."))

#set columns as factors
bhclim$year <- as.factor(bhclim$year)
bhclim$variety <- as.factor(bhclim$variety)

#explore a bit 
head(bhclim)
str(bhclim)
plot(bhclim$lte ~ bhclim$meanC)
climatehardPlot <- ggplot(aes(x = meanC, y = lte), data = bhclim)
climatehardPlot + geom_point(aes(colour = factor(Year))) +	
	theme_classic() + ylab("LTE50")

climatePlot <- ggplot(aes(x = Datestrptime, y = lte), data = bhclim)
climatePlot + geom_point() +
  xlab("Date") + ylab("LTE50")	+
	theme_classic()


plot(bhclim$lte ~bhclim $Datestrptime)
plot(bhclim$lte ~bhclim $month_day )
plot(bhclim$lte ~bhclim $meanC)


plot(bhclim$lte ~ bhclim$meanC, xlab = "Mean two day temperature (degrees C)", ylab = "bud hardiness (LTE50 degrees C)")
abline(lmFit, col = "red")

yearPlot <- ggplot(aes(x = year, y = lte), data = bhclim)
yearPlot + geom_boxplot()+theme_classic()

varietyPlot <- ggplot(aes(x = variety, y = lte), data = bhclim)
varietyPlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

sitePlot <- ggplot(aes(x = site, y = lte), data = bhclim)
sitePlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

#check i have enouph data spread for to include site 
levels(bhclim$site)
bhclim$site2 <- bhclim$site # make a new column to simplify site names 
levels(bhclim$site2) 
table(bhclim$site, bhclim$variety)

bhclim[bhclim$site == "Average Bud Hardiness (all sites, all varieties)",] # these must have been removed


bhclim$site2 <- as.character(bhclim$site2)
bhclim$site2 <- gsub(", east", "", bhclim$site2 )
bhclim$site2 <- gsub(", west", "", bhclim$site2 )
bhclim$site2 <- gsub(" east", "", bhclim$site2 )
bhclim$site2 <- gsub(" west", "", bhclim$site2 )
bhclim$site2 <- gsub(", southeast", "", bhclim$site2 )
bhclim$site2 <- gsub(", northeast", "", bhclim$site2 )
bhclim$site2 <- gsub(" northeast", "", bhclim$site2 )
bhclim$site2 <- gsub("West ", "", bhclim$site2 )
bhclim$site2 <- gsub("s ", "s", bhclim$site2 )


unique(bhclim$site2)



bhclimnoNoData <- bhclim[!bhclim$site == "",]

table(bhclimnoNoData$site2, as.character(bhclimnoNoData$variety))


#run the model on real data
#--------------------------------

head(bhclimnoNoData)
bhclimComplete <- bhclimnoNoData[!is.na(bhclimnoNoData$lte),]
nReal <- nrow(bhclimComplete)
x <- I(bhclimComplete$meanC)
y <- bhclimComplete$lte
year <- as.integer(as.factor(bhclimComplete$year))
n_year <- length(unique(bhclimComplete$year))
variety <- as.integer(as.factor(as.character(bhclimComplete$variety))) 
n_vars <- length(unique(as.character(bhclimComplete$variety)))
n_site <- length(unique(bhclimComplete$site2))
site <- as.integer(as.factor(bhclimComplete$site2))

stan_data3 <- list(N = nReal, x = x, y = y, n_vars = n_vars, n_year = n_year, n_site = n_site, year = year, variety = variety, site = site )

str(stan_data3)


write("//
// This Stan program defines a linear model predicting LTE50 from temperature, with partial pooling of variety and year 
//
// Stan model for partially pooled linear regression including priors 

data {

	//Level 1
	int < lower = 1 > N; // Sample size - number of observations
	vector[N] x; // Predictor
	vector[N] y; // Outcome

	//Level 2 
	int < lower = 1 > n_vars; // number of random effect levels (varieties) 
	int < lower = 1, upper = n_vars > variety[N]; // id of random effect (variety)

	int < lower = 1 > n_year; // number of random effect levels (years) 
	int < lower = 1, upper = n_year > year[N]; // id of random effect (year)

	int < lower = 1 > n_site; //number of sites 
	int < lower = 1, upper = n_site > site[N]; // id of random effect (site)


	}

parameters {

	//level 1
	real < upper = -3 > alpha_g; // mean intercept accross all varieties. Grand mean
	real beta; //slope accross all varieties
	real <lower =0> sigma_y; // overall variation accross observations

	//level 2
	real <lower = 0> sigma_v; // variation of intercept amoung varieties  
	real varmu[n_vars];

	real <lower = 0> sigma_k; // variation of intercept amoung years
	real yearmu[n_year];

	real <lower = 0> sigma_s; // variation of intercept amoung vsites
	real sitemu[n_site];

}
transformed parameters{
	//Individual mean 
	real ymu[N];

	//Individual mean calculation 
	for (i in 1:N){
		ymu[i] = alpha_g + varmu[variety[i]] + yearmu[year[i]] + sitemu[site[i]];  
	}

}

model{
	//Level 1
	alpha_g ~ normal(-15,12); // prior for grand alpha, assumes intercept will negative and around -10.
	//i chose this because -3 is minimum hardiness (least hardy) and few vines can manage 
	//temps much lower than -27
	beta ~ lognormal(0,1);
	sigma_y ~ normal(0,5); // prior around estiamted mean LTE50.

	//Level 2
	varmu ~ normal(0,sigma_v); // prior for the effect of random factor on grand mean 
	sigma_v ~ normal(0, 5); // prior for the variety around levels of random factor. Same as sigma_y

	yearmu ~ normal(0,sigma_k); // prior for the effect of random factor on grand mean 
	sigma_k ~ normal(0, 5); // prior for the variety around levels of random factor. Same as sigma_y

	sitemu ~ normal(0,sigma_s); // prior for the effect of random factor on grand mean 
	sigma_s ~ normal(0, 5); // prior for the variety around levels of random factor. Same as sigma_y


	//liklihood
	for (i in 1:N){
		y[i] ~ normal(ymu[i] + beta * x[i], sigma_y);
	}
}

generated quantities {

	real realY[N]; 

	for (i in 1:N){
	realY[i] = ymu[i] + beta * x[i];
	}

} // The posterior predictive distribution",

"stan_model3.stan")

stan_modelMulti3 <- "stan_model3.stan"



fit3 <- stan(file = stan_modelMulti3, data = stan_data3, warmup = 1000, 
	iter = 3000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))


launch_shinystan(fit3)

post3 <- extract.samples(fit3)

str(post3)

#density plots for main parameters
plot(density(post3$alpha))# 
mean(post3$alpha)
HPDI(post3$alpha)
plot(density(post3$beta))# 
mean(post3$beta)
HPDI(post3$beta)
plot(density(post3$sigma_y))# standar derror around full model sigma. 
mean(post3$sigma_y)
HPDI(post3$sigma_y)
plot(density(post3$sigma_v))#  standard error around variety. 
mean(post3$sigma_v)
HPDI(post3$sigma_v)
plot(density(post3$sigma_k))#standard error around year
mean(post3$sigma_k)
HPDI(post3$sigma_k)
plot(density(post3$sigma_s))#standard error around site
mean(post3$sigma_s)
HPDI(post3$sigma_s)

#simlate x values in case i need them 
#------------------------------------
nObs <- 30
meanTemp <- mean(bhclimComplete$meanC)
sigmaTemp <- sd(bhclimComplete$meanC)
simTemps <- rnorm(nObs , meanTemp , sigmaTemp)

#get teh predicted hardiness from teh model
#---------------------------------------------

str(post3$realY)
predY <- data.frame(post3$realY)
str(predY)
predYMean <- colMeans(predY)
bhclimComplete$predLTE <- predYMean# add the predicted values to the original data frame 

#make some pretty plots
#-------------------------

#model prediction 
plot( predYMean ~ bhclimComplete$meanC, xlab = "Temperature (C)", ylab = "predicted LTE", main = "model prediction") 

#The estimated lte againts the real lte 
plot ( bhclimComplete$lte ~ predYMean, xlab = "Predicted LTE", ylab = "empirical LTE", main = "estimated lte against predicted lte") #model is overestimating hardiness especially at low temperatures 


#variety effects
#----------------

#make a dataframe of the how each variety varies from teh grand mean  
varietyEffects <- data.frame(post3$varmu)
colnames(varietyEffects) <- levels(as.factor(as.character(bhclimComplete$variety)))#only the first 13 varietyies have data that is used in teh model
mcmc_intervals(varietyEffects)

#plot predicted values split by variety 
varPlot <- ggplot(data = bhclimComplete, aes(y = lte, x = predYMean))
varPlot + geom_point()+theme_classic()+
	facet_wrap(variety)

#siteEffects
#--------------
siteEffects <- data.frame(post3$sitemu)
colnames(siteEffects) <- levels(as.factor(as.character(bhclimComplete$site2)))#only the first 13 varietyies have data that is used in teh model
mcmc_intervals(siteEffects)

unique(bhclimComplete$site2)
#plot predicted values split by variety 
bhclimComplete$site2F <- as.factor(bhclimComplete$site2)
sitePlot <- ggplot(data = bhclimComplete, aes(y = lte, x = predYMean))
sitePlot + geom_point()+theme_classic() +
	facet_wrap(site)

names(bhclimComplete)
bhclimComplete$site2
str(bhclimComplete)
#year effects
#------------

yearEffects <- data.frame(post3$yearmu)
colnames(yearEffects) <- levels(as.factor(as.character(bhclimComplete$year)))#only the first 13 varietyies have data that is used in teh model
mcmc_intervals(yearEffects)

#plot predicted values split by variety 
yearPlot <- ggplot(data = bhclimComplete, aes(y = lte, x = predYMean))
yearPlot + geom_point()+theme_classic()+
	facet_wrap(year)

#effect of year againt mean temp for that year 
meanYear <- bhclimComplete %>% group_by(Year) %>%
	summarise(meanTempYear = mean(meanC), minTempYear = min(meanC))
meanYear$yearEffect <- colMeans(yearEffects)
plot(meanYear$yearEffect ~ meanYear$meanTempYear, xlab = "mean temperature (C)", ylab = "effect of year") # colder years are less hardy, given teh temperature.


