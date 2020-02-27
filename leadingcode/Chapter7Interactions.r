#chapter 7 Rethinking - interactions
#----------------------------------------
rm(list = ls())

library(rethinking)
library(rstan)
library(ggplot2)
library(dplyr)

#manatees are killed by blunt collisions, not propellar cuts. Thats why manatees have scars from propellars - they tend to survive these encounters. 
#similar with bommer planes. Damage on survivors does not inform where we should protect more. Damage is CONDITIONAL on survival.
#All models are conditional on something, but some models need more complicated structures of conditionality. These are INTERACTION
#models.
#An interaction is a way of alowing parameters to be conditional on on further aspects of the data. 
#Multi level models are basicly massive interaction models, where parameter estimates are conditional on clusters in the data
#Models with complex interactions can be difficult to understand

#Africa example
#-----------------

data(rugged)
d <- rugged

#make a log version of the outcome
d$log_gdp <- log10(d$rgdppc_2000) 

#extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000),]

#split countries into Africa and nonAfrica
d.A1 <- dd[dd$cont_africa ==1 ,] # Africa
d.A0 <- dd[dd$cont_africa == 0, ] # not Africa

#interlude about logarythms
# consider what sort of log to use. I prefer log 10 because visually interpreting it makes more sense to me 
#also understand that logs show magnitude of change rather than absolute change, so useful for things that 
#increase expinentially. FOr example change in body size or GDP

#African Nations
m7.1 <- map(
	alist(log_gdp ~ dnorm(mu, sigma),
		mu <- a + bR * rugged,
		a ~ dnorm(8,100),
		bR ~ dnorm(0,1),
		sigma ~ dunif(0,10)
		),
	data = d.A1
	)

#non African nations
m7.2 <- map(
	alist(
		log_gdp ~ dnorm(mu, sigma),
		mu <- a + bR*rugged,
		a ~ dnorm(8,100),
		bR ~ dnorm(0,1),
		sigma ~ dunif(0,10)
		),
	data = d.A0
	)

#posterior checks 
par(mfrow=c(1,2)) 
#Africa
post1 <- extract.samples(m7.1)
mu.link1 <- function(rugged) post1$a + post1$bR* rugged # define the function based on my linear model
rugged.seq <- seq(from = 0, to = 6, by = 1) # make a sequence of rugeddness values 
mu1 <- sapply(rugged.seq, mu.link1)
mu1.mean <- apply(mu1, 2, mean)
mu1.HPDI <- apply(mu1, 2, HPDI, prob = 0.89)
plot(log_gdp ~ rugged, data = d.A1, main = "Africa")
lines(rugged.seq, mu1.mean)
shade(mu1.HPDI, rugged.seq)

#not Africa
post0 <- extract.samples(m7.2)
mu.link0 <- function(rugged) post0$a + post0$bR* rugged
mu0 <- sapply(rugged.seq, mu.link0)
mu0.mean <- apply(mu0, 2, mean)
mu0.HPDI <- apply(mu0, 2, HPDI, prob = 0.89)
plot(log_gdp ~ rugged, data = d.A0, main = "non-Africa")
lines(rugged.seq, mu0.mean)
shade(mu0.HPDI, rugged.seq)
par(mfrow=c(1,1)) 

#Dont split the data because:

#1. if there is a parameter that doesnt change for the two different variables,
#for example general error around the predicted mean, then you will do a worse job
#of predicting it in teh two smaller models

#2. you cant judge statistically how likly the interaction is. Maybe its the same 
#accross all values, but you will never know!

#3. you cant easily compare the fit of two models against the fit of one

#4. If all the indormation is in teh same model, you can "borrow" information 
#from different bits of teh model. especially useful when sample size varies a
#lot accross categories

# you cant just fit a dummy variable either (i.e. Africa- and non-Africa as 0s and 1s ) 
# because thsi wont alow the SLOPE to vary like you want 

#Side note - resist the tyrany of the Tetrapoda

#fit the interaction model
m7.5 <- map(
	alist(
		log_gdp ~ dnorm(mu, sigma),
		mu <- a + gamma*rugged + bA*cont_africa,
		gamma <- bR + bAR*cont_africa,
		a ~ dnorm(8,100),
		bA ~ dnorm(0,1), 
		bR ~ dnorm(0,1), 
		bAR  ~ dnorm(0,1), 
		sigma ~ dnorm(0, 10) 
		),
		data = dd
	)
#we dont knwo where the intercept will end up, but if we regularize 
#the coefficents then teh intercept will effetively be reguralised too 

rugged.seq <- seq(from = -1, to = 8, by = 0.25)

mu.Africa <- link( m7.5 , data=data.frame(cont_africa=1,rugged=rugged.seq) )
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )

mu.NotAfrica <- link( m7.5 , data=data.frame(cont_africa=0,rugged=rugged.seq) )
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )


par(mfrow=c(1,2)) 
# plot African nations with regression
d.A1 <- dd[dd$cont_africa==1,]
plot( log10(rgdppc_2000) ~ rugged , data=d.A1 ,
    col=rangi2 , ylab="log GDP year 2000" ,
    xlab="Terrain Ruggedness Index" )
mtext( "African nations" , 3 )
lines( rugged.seq , mu.Africa.mean )
shade( mu.Africa.PI , rugged.seq )

# plot non-African nations with regression
d.A0 <- dd[dd$cont_africa==0,]
plot( log10(rgdppc_2000) ~ rugged , data=d.A0 ,
    col="black" , ylab="log GDP year 2000" ,
    xlab="Terrain Ruggedness Index" )
mtext( "Non-African nations" , 3 )
lines( rugged.seq , mu.NotAfrica.mean )
shade( mu.NotAfrica.PI , rugged.seq )
par(mfrow=c(1,2)) 

# dont try and understand your interaction effect from a table of numbers. 
#Instead plot, plot, plot!

#two reasons to be careful when interpreting interactions:

#1. the "main effects" mean something different. Distributions cannot be easily compared.
#2. tables of numbers make it difficult to incorporate uncertainty 

#ANYTHING CALCULATED AS A PARAMETER MUST HAVE A DISTRIBUTION

#7.2.7 Africa depends on ruggedness
#---------------------------------

#min and max rugged values
q.rugged <- range(dd$rugged)

#compute lines and confidence intervals
mu.ruggedlo <- link(m7.5, data = data.frame(rugged = q.rugged[1], cont_africa = 0:1))
mu.ruggedlo.mean <- apply(mu.ruggedlo, 2, mean)
mu.ruggedlo.PI <- apply(mu.ruggedlo, 2, PI)

mu.ruggedhi <- link(m7.5, data = data.frame(rugged = q.rugged[2], cont_africa = 0:1))
mu.ruggedhi.mean <- apply(mu.ruggedhi, 2, mean)
mu.ruggedhi.PI <- apply(mu.ruggedhi, 2, PI)

#plot the results, splitting points bu median
med.r <- median (dd$rugged)
ox <- ifelse(dd$rugged > med.r, 0.05, -0.05)
plot(dd$cont_africa + ox, log(dd$rgdppc_2000),
	col = ifelse(dd$rugged>med.r, rangi2, "black"),
	xlim = c(-0.25, 1.25), xaxt = "n", ylab = "log GDP year 2000",
	xlab = "continent")
axis (1, at = c(0,1), labels = c("other", "Africa"))
lines(0:1, mu.ruggedlo.mean, lty = 2)
shade(mu.ruggedlo.PI, 0:1)
lines(0:1, mu.ruggedhi.mean, col = rangi2)
shade(mu.ruggedhi.PI, 0:1, col = col.alpha(rangi2, 0.25))

#Practice
#-------------------------------

#7E1.
#1) Bread dough rises because of yeast, conditional on temperature
#2) Education leads to higher income, conditional on social privlage 
#3) Gasoline makes a car go, conditional it being the appropriate fuel for the vehicle

#7E2.
#1 has the interaction? Caremelising onioins successfuly means cooking over a low heat, conditional on the onions not drying out?

#7H1. 
#include bed as a predictor in the interaction model of water and shade predicing blooming 

data(tulips)

#explore data
tail(tulips)
str(tulips)

#make a dummy variable for bed (three levels)
tulips$bed <- as.character(tulips$bed)
tulips$dBeda <- 0
tulips$dBedb <- 0
tulips$dBedb[tulips$bed == "b"] <- 1
tulips$dBedc <- 0
tulips$dBedc[tulips$bed == "c"] <- 1

#preparing the data
#-----------------------------

#centre values
tulips$meanCentredBlooms <- tulips$blooms - mean(tulips$blooms)
tulips$meanCentredShade <- tulips$shade - mean(tulips$shade)
tulips$meanCentredWater <- tulips$water - mean(tulips$water)


# I need to index the x values from 1 to n(x)

w <- I(tulips$meanCentredWater)
sh <- I(tulips$meanCentredShade)
beda <- I(tulips$dBeda)# i dont need this level in the model because all 0s. it will be alpha.
bedb <- I(tulips$dBedb)
bedc <- I(tulips$dBedc)

y <- tulips$blooms
N <- length(tulips$meanCentredBlooms)


#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame
stan_data <- list(N = N, w = w, sh = sh, bedb = bedb, bedc = bedc, y = y)

#stan model 
#--------------------

#https://github.com/ssp3nc3r/rethinking - rethinking code but in STAN 

write("// Stan model for simple linear regression including priors 
data {
	int < lower = 1 > N; // Sample size
 
 	vector[N] w; // Predictor - water
 	vector[N] sh; //Predictor - shade
 	vector <lower=0,upper=1> [N] bedb; // Predictor bed b
 	vector <lower=0,upper=1> [N] bedc; //predictor bed c

 	vector[N] y; // Outcome
 }

parameters {
	real  < lower = 0 > alpha; // Intercept, cant be lower than 0 blooms
	real betaW; // slope for effect of water 
	real betaSh ; // slope for effect of shade
	real betaWSh; //slope for interaction between shade and water 
	real betaBb; //slope for bed b
	real betaBc; //slope for bed c
	real < lower = 0 > sigma; // Error SD

}
model {
	vector[N] mu; // mean value 
	
	//priors
	alpha ~ normal (130, 100); //
	betaW ~ normal(0, 100);
	betaSh ~ normal(0, 100);
	betaWSh ~ normal(0, 100);
	betaBb ~ normal(0, 100);
	betaBc~ normal(0, 100);
	sigma ~ normal(0, 100);

	//likelyhood 

	for (i in 1:N) {
		mu[i] = alpha + w [i] * betaW + sh[i]*betaSh + betaWSh * w[i] * sh[i] + bedb[i]*betaBb + bedc[i]*betaBc;
	}
	y ~ normal(mu, sigma);

}

generated quantities {
} // The posterior predictive distribution",

"stan_model1.stan")

stan_model1 <- "stan_model1.stan"

fit1 <- stan(file = stan_model1, data = stan_data, warmup = 2000, iter = 6000, chains = 4, cores = 4, thin = 1, control = list(adapt_delta = 0.90))
#i had problems with divergent transitions for this model, which confused me a lot until I realised
#i had set parameters for bed in teh parameter section, given them priors and then not included them
#in the liklyhood function. Eventually I also had to change the sigma prior from flat to a truncated normal 
#to get the model to work. 

par(mfrow=c(1,1)) 

#1. Did teh model run ok? CHeck teh model feels pretty happy with its estimates
#---------------------------------------------------------------------------

fit1#Rhat values of 1 - chains probably mixed well 
traceplot(fit1) # check convergence of the caterpillers 
plot(fit1) # plots the distributions of the different parameters 

#extract model output to see what it predicts 
#---------------------------------------------
posterior1 <- extract(fit1)
plot(posterior1$alpha, type = "l") # another way to check a traceplot 


#how we would plot posteriors of each individual parameter 
plot(density(posterior1$alpha)
plot(density(posterior1$betaW)

#plot posterior predictive checks to see the effect of water and shade on blooms
#in each block

#get predicted blooms from the model using mean values 

postPred <- data.frame(matrix(NA, length(w), 1000))
data.frame(matrix(NA, 3, 5))
postPred$iteration <- rep(1:1000, each = length(w))

for (iter in 1:1000){
	mu <- vector()

	for (i in 1:N){
		mu[i] <-  posterior1$alpha[iter] + w[i] * posterior1$betaW[iter] + sh[i]*posterior1$betaSh[iter] + 
			posterior1$betaWSh[iter] * w[i] * sh[i] + bedb[i]*posterior1$betaBb[iter] + bedc[i]*posterior1$betaBc[iter]
		y[i] <- mu[i] + posterior1$sigma[iter]
	}
	postPred[,iter] <- y 
}

meanBlooms <- apply(X=postPred, MARGIN=1, FUN=mean, na.rm=TRUE)
HPDIBlooms <- apply(X=postPred, MARGIN=1, FUN=HPDI, prob = 0.89)
HPDIData<- data.frame(matrix(unlist(HPDIBlooms)))
HPDIData$type <- rep(c("lower", "upper"))
lowerHpdi <- HPDIData[HPDIData$type == "lower",1]
upperHpdi <- HPDIData[HPDIData$type == "upper",1]

plot(meanBlooms ~ y)
plotData <- data.frame(cbind(y, meanBlooms, lowerHpdi, upperHpdi))
predPlot <-ggplot(data = plotData, aes(y=meanBlooms, x=y)) + geom_point() 
predPlot + geom_ribbon(aes(ymin=plotData$lowerHpdi, ymax=plotData$upperHpdi), linetype=2, alpha=0.1)+
	theme_classic()+
	xlab("empiricaly observed blooms")+
	ylab("simulated number of blooms")

#For block A get predicted blooming against water for three different levels of shade 

head(tulips)

par(mfrow=c(1,3)) # 3 plots in 1 row

shadeType <- 1

shade1Data <- tulips
shade1Data$meanCentredShade <- 1 # set shade to 1 for this pannel 
# I am using teh real water data because it is easier with the different beds, but you could 
# simulate water instead
# what is teh relationship between blooms and water when shade is constant 1?
Shade1List <- list()
randomRows <- sample(1:1000, 20)#select 20 random rows to draw from teh posterior 

listCount <- 1 #counter for making teh list 

for(shadeType in -1:1){ # run through each shade type 

	for (iter in 1:20){#number of times i run the "model"
		shade1Data$Iteration <- iter 
		shade1Data$ShadeSim <- shadeType

		rnRowI <-randomRows[iter] 

		for (i in 1:nrow(tulips)){#runnin through teh model for each water value

		shade1Data$muSh1[i] <-  posterior1$alpha[rnRowI]+ w[i] * posterior1$betaW[rnRowI] + sh[i]*posterior1$betaSh[rnRowI] + 
				posterior1$betaWSh[rnRowI] * w[i] * shadeType + bedb[i]*posterior1$betaBb[rnRowI] + bedc[i]*posterior1$betaBc[rnRowI]
		}

		Shade1List [[listCount]] <- shade1Data
		listCount <- listCount + 1 
	}
}

shadeData <- do.call("rbind", Shade1List)
head(shadeData)

#faceted plot showing that when conditions are very light, teh relationship between 
#water and blooms is very positive. WHen shade is moderate, the relationship is still
#positive but less strongly so, and at high shade (low light) water doesnt help much 
#for blooms. 

#the relationship seems consistant accross beds, although there are fewer blooms in bed a . 

interPlot <- ggplot(data = shadeData, aes(x = meanCentredWater, y = muSh1))
interPlot2 <- interPlot + geom_point()+
    stat_smooth(method = 'lm', se = FALSE, aes(group = Iteration)) +
    theme_bw() + facet_grid(bed ~ ShadeSim)

