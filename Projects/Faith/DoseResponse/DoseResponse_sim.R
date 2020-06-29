rm(list = ls())

#Simulating a new winter hardiness model, thi sone based on a dose response curve. 
#Model is based on teh one in Ritz et al, 2015. PLOS one. 

#script started by Faith Jones on June 26th, 2020
#First focus on vust varieties rather than site and variety 

set.seed(16)


if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/vinmisc/bcvin/hardiness/analyses/") 
} else
setwd("/home/faith/Documents/github/bcvin/bcvin/hardiness/analyses/")

library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(drc)

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

plot(bhclim$lte ~bhclim $Datestrptime)
plot(bhclim$lte ~bhclim $month_day )
plot(bhclim$lte ~bhclim $meanC)



##simulate temperature data 
#-------------------------------------------------

#inputs
nrep <- 20 # number of reps of each variety 
meanTemp <- 4
sigmaTemp <- 8
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)


#make temperatures positive because teh x value cant be negative. I do this by adding 20 to the data
# I will also model teh hardiness resonse as inverted sp plants get a higher number as they get more cold hardy rather than a more negative number
simTempsPos <- simTemps + 30

#Set model parameters - I have no idea what they should be so I will run a similar model in R (below)

b <- 11 #this is the rate paramater, like a slope in a linear regession 
d <- 24 # maximum hardiness (inverted from -25)
c <- 10 # minimum hardiness (inverted from -3)
e <- 37 # Effective dose ED50. x value where y value is halfway bewteen max(d) and min (c)

x <- simTempsPos

hardinessPos <- c + ( (d-c) / (1 + exp(b*(log(x)-log(e)))))
hardiness <- (-1 * hardinessPos)

plot(hardinessPos ~ simTempsPos, pch = 16, col = 2, xlab = "Simulated temperatures plus 30", ylab = "winter hardiness * -1")#modefied positive  data
plot(hardiness ~ simTemps, pch = 16, col = 3, xlab = "Simulated temperatures", ylab = "winter hardiness")#changed back to negative values

#Try eith some varietiy level differences 

nvariety <- 20
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"

bvarsigma <- 1
bvars <- rnorm(nvariety , b, bvarsigma)

dvarsigma <- 0
dvars <- rnorm(nvariety , d, dvarsigma)

cvarsigma <- 0
cvars <- rnorm(nvariety , c, cvarsigma)

evarsigma <- 5
evars <- rnorm(nvariety , e, evarsigma)

#make a database to hold results
varieties <- rep(varNames, each = nrep)
airtemp <- rep(simTempsPos, times = nvariety)

doseSimData <- data.frame(cbind(varieties, airtemp))

doseSimData$ltePositive <- NA

#loop through each variety
for (i in 1:nvariety){
	doseSimData$ltePositive[doseSimData$varieties == as.factor(i)] <- cvars[i] + ( (dvars[i]-cvars[i]) / (1 + exp(bvars[i]*(log(x)-log(evars[i])))))
}

#add some variation 
gsigma <- 0.5
doseSimData$eps <- rnorm(nrow(doseSimData), 0, gsigma)

doseSimData$finalLTEPos <- doseSimData$ltePositive + doseSimData$eps 

#add columns where data is not transformed to eb positive
doseSimData$negLTE <- doseSimData$finalLTEPos*-1
doseSimData$airtempCold <- doseSimData$airtemp - 30


#make some data to plot the line mean model
plotingTemps <- rnorm(100, meanTemp,sigmaTemp) + 30
plotingTemps <- sort(plotingTemps)
plotingTempsCold <- plotingTemps - 30

plottingLTE <- (c + ( (d-c) / (1 + exp(b*(log(plotingTemps)-log(e)))))) * -1

plot(doseSimData$negLTE ~ doseSimData$airtempCold, 
	xlab = "air temp (degrees)", 
	ylab = "LTE50 (degrees C)", 
	pch = 16, 
	col = 4,
	main = "bsigma = 1, dsig = 0, csig = 0, esig = 5, gsig= 0.5")
lines(plottingLTE ~ plotingTempsCold)





#Get sensible parameters by running a real dose response curve model on my data
#--------------------------------------------------------------------------
bhclim$ltePositive <- bhclim$lte * -1 # I want positive values?
bhclim$meanC20 <- bhclim$meanC + 30 # make suer all temperatures are positive 

drmHardiness <- drm(ltePositive  ~ meanC20, data = bhclim, fct = LL.4())
summary(drmHardiness)
plot(drmHardiness)

predictedHardiness <- data.frame(predict(drmHardiness, newdata=expand.grid(simTempsPos), interval="confidence"))
names(predictedHardiness)

#Plot model results
ggplot(bhclim, aes(x = meanC20, y = ltePositive)) +geom_point() +
	geom_ribbon(data=predictedHardiness, aes(x=simTempsPos, y=Prediction, ymin=Lower, ymax=Upper), alpha=0.2) +
	geom_line(data=predictedHardiness, aes(x=simTempsPos, y=Prediction))+
	xlab("Air temp plus 30 degrees") + 
	ylab("inverted hardiness (-LTE50)")

predictedHardiness$PredictedNegative <- -1*predictedHardiness$Prediction
predictedHardiness$LowerNegative <- -1*predictedHardiness$Lower
predictedHardiness$UpperNegative <- -1*predictedHardiness$Upper

#Plot model results with hardiness and air temp back how they were originally 
ggplot(bhclim, aes(x = meanC, y = lte)) +geom_point() +
	geom_ribbon(data=predictedHardiness, aes(x=simTemps, y=PredictedNegative, ymin=LowerNegative, ymax=UpperNegative), alpha=0.2) +
	geom_line(data=predictedHardiness, aes(x=simTemps, y=PredictedNegative))+
	xlab("Air temp (degrees C)") + 
	ylab("cold hardiness (LTE50)") +
	theme_classic()

#
#https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling/13823