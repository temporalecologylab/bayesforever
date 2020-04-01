#Rethinking Chapter 11 - random slopes 
#----------------------------------------

rm(list = ls())
setwd("~/Documents/github/bayes2020/leadingcode")

library(MASS)
library(ellipse)
library(rethinking)
library(rstan)

#You can partially pool slopes like you do intercepts, and correlate the effects on slope and intercept  
#pool information accross  parameters 
#improves estimation of intercept by pooling information accross parameters 
#Massive interaction mashine
#it is a joint multivariate model that allows this to happen 

#uses the example of waiting times in coffee shops again. Popular shopes have longer waits in 
#the morning and shorter waits in the afternoon. 


#simulating a multi level model of hardiness against temp partially pooled by variety 
#-------------------------------------------------------------------------------------
set.seed(16) # this makes my randomisation reproducable. 

#model should be:
# y ~ Normal((alpha + alphaSite) + beta * x, sigma/eps)

#parameters 

#LTE50sim (y), winter hardiness, is simulated using parameters

#inputs
nrep <- 30 # number of reps of each variety and year (days sampled in a year and for a variety )
meanTemp <- 2 #the mean winter temperature 
sigmaTemp <- 5 # The varation (standard error) around that mean winter temperature 
simTemps <- rnorm(nrep, meanTemp,sigmaTemp) # simulating winter temperatures 

nvariety <- 20 #number of winegrape varieties 
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"
nyear <- 20 # there are 20 years of data
yearNames <- as.factor(1:20) #name of each year (year is treted like a factor rather than a continuous variable)

#parameters

nObs <- nyear*nvariety*nrep # the number of observations  for each year and each variety combined 

#make a multivariate distribution of slopes and intercepts for each grouping effect
alpha <- -21 # overall gran mean alpha
betag <- 0.5 # overall grand mean beta

muAB <- c(alpha, betag)#combine two gran effects

#Year
alphaYear <- rep(rnorm(nyear, 0, 0.5), times = nvariety) # random effect of year, 20 years and each year has a each variety in it 
alphaYearObs <- rep(alphaYear, each = nrep) 

#variety
sigma_vara <- 0.2 # standard deviation in intercepts for year 
sigma_varb <- 0.3 # standard deviation in slopes for year 
rho_var <- -0.7 # correlation between intercept and slope. Lower intercepts (more cold hardy) should have steeper slopes

#Correlation matrix 
sigmas_var <- c(sigma_vara, sigma_varb) # combine sigma values into a vector
Rho_var <- matrix(c(1, rho_var, rho_var, 1), nrow = 2) # correlation matrix
Sigma_var <- diag(sigmas_var) %*% Rho_var %*% diag(sigmas_var) # this does some matrix black magic
diag(sigmas_var)#diags puts the two values diagnally from each other and fills extra space with 0s 
varEffects <- MASS::mvrnorm(nyear, muAB, Sigma_var)#get overall slopes and intercepts for each year 

#plotting data
#-------------------

plot(alphaVar, betaVar) # how the effect of variety on alpha and beta realate. Varieies with a lower
#alphavar (intercept) have steeper slopes. This means that a variety that is hardy to a really low temperature
#when the air temperature is 0 will also get more/less cold hardy quicker. My theory behind this setup was that 
#varieties that get really cold hardy midwinter need to be able to get un-hardy quicker so they are still 
#ready to "spring" into action in the spring.

# overlay population distribution
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
lines(ellipse::ellipse(Sigma_var,centre=muAB,level=l),col=col.alpha("black",0.2)) 
points(muAB[1], muAB[2], col = "red", pch = 3)#grand mean and slope 

#simulating individual observations
#-----------------------------------------------

alphaVar <- rep(varEffects[,1], times = nyear) # replicate the value so i can get a unique combination of year and variety
alphaVarObs <- rep(alphaVar, each = nrep) # make sure I have 30 replicates for each variety/year combination

betaVar <- rep(varEffects[,2], times = nyear) # replicate the value so i can get a unique combination of year and variety
betaVarObs <- rep(betaVar, each = nrep) # make sure I have 30 replicates for each variety/year combination


#other model parameters (no grouing)
sigma <-  2
eps <- rnorm(nObs , 0, sigma)

#make columns for teh name of the year, variety and day of the year 
varNamesRep <- rep(varNames, each = nyear)
varNamesObs <- rep(varNamesRep, each = nrep)
YearNameRep <- rep(1:nyear, times = nvariety)
yearNamesObs <- rep(YearNameRep, each = nrep)

#sigma <- sqrt(sigma2)

meanTemp <- mean(bhclim$meanC)
sigmaTemp <- sd(bhclim$meanC)
simTemps <- rnorm(nObs , meanTemp , sigmaTemp)


simLTEVar <- alphaVarObs + alphaYearObs + betaVarObs * simTemps + eps
plot(simLTEVar ~ simTemps)

#combine into a single data table

simVarData <- data.frame(cbind(simTemps, varNamesObs, yearNamesObs, simLTEVar))
str(simVarData)
simVarData[order(simVarData$varNames),]
simVarData$varNamesObs <- as.factor(simVarData$varNamesObs )
simVarData$yearNamesObs <- as.factor(simVarData$yearNamesObs ) 

#Note: in this example, everything is ballanced, but it doesnt need to be in real life 


#prior predictive checks 
#---------------------------------------------

#how do i chose a prior for the covarience structure???? I guess that it needs to be a negative number. Normal as well?
R <- rlkjcorr(1e4, K = 2, eta = 2)
plot(density(rlkjcorr(Ni, K = 2, eta = 2)[,1,2]))#what this prior looks like 


#range of temperatures to simulate over
Ni <- 10 #number of repeat runs of the model 
nObs <- 30 # number fo temperature observations per simulation 
preTemps <- rnorm(30, -15, 20) # i think this is a sensible range of winter temps

#1st level parameters 
alpha_g <-  rnorm(Ni, -15, 12)
beta_g <- rlnorm(Ni, 0 , 1)
sigma_g <- rnorm(Ni, 0, 5)#i wont need thsi to simulate mu values 

#second level parameters 
Rho_varNi <- rlkjcorr(Ni, K = 2, eta = 2)[,1,2]#prior for correlation matrix. K - dimensions i think, and eta is teh shape of the density plot 
Sigma_vara <- rnorm(Ni, 0, 1) # standard deviation in intercepts for variety  
Sigma_varb <- rnorm(Ni, 0, 1) # standard deviation in slopes for variety

#make a dataframe for the outputs of random effects 
n_vars  <- 20 # number of random effects 
n_year <- 20
varietySim <- rep(as.factor(c(1:n_vars)), each = n_year)
yearSim <- rep(as.factor(c(1:n_year)), times = n_vars)
raneffectV <- rep(varietySim, times = Ni )
raneffectY <- rep(yearSim, times = Ni )

repetition <- rep(1:Ni, each = n_vars*n_year)
randomEffectsPre <- data.frame(cbind(raneffectY, raneffectV) )
randomEffectsPre$alpha_v <- NA # this will be the alpha values for each varity, not teh grand alpha 
randomEffectsPre$sigma_year <- NA
randomEffectsPre$rep <- repetition
#randomEffectsPre$alpha_g <- rep(alpha_g , each = n_vars)
#randomEffectsPre$beta <- rep(beta, each = n_vars) Thsi will come from teh below loop so each variety has a different slope
#randomEffectsPre$x <- rep(preTemps, times = n_vars)


#loop for making 100 different repetitions of the model, where there are 20 varieties and 20 years 
#i then repeat the values so that there is a combination of variety and year for each value 
for (counter in 1:Ni){

	#counter <- 1

	#Year effect on alpha
	sigma_yearc <- rep(rnorm(n_vars, 0, 5), times = n_vars)
	randomEffectsPre$sigma_year[randomEffectsPre$rep == counter] <- sigma_yearc

	#Variety effect on alpha and beta  
	muABs <- c(alpha_g[counter], beta_g[counter])#combine two grand effects

	sigma_varas <- Sigma_vara[counter]# standard deviation in intercepts for year 
	sigma_varbs <- Sigma_varb[counter] # standard deviation in slopes for year 
	rho_vars <- Rho_varNi[counter] # correlation between intercept and slope. Lower intercepts (more cold hardy) should have steeper slopes

	sigmas_vars <- c(sigma_varas, sigma_varbs) # combine sigma values into a vector
	Rho_vars <- matrix(c(1, rho_vars, rho_vars, 1), nrow = 2) # correlation matrix
	Sigma_vars <- diag(sigmas_vars) %*% Rho_vars %*% diag(sigmas_vars) # this does some matrix black magic
	varEffectss <- mvrnorm(nyear, muABs, Sigma_vars)#get overall slopes and intercepts for each year 

	alpha_vc <- varEffectss[,1] # alpha values for each variety 
	alphaVar <- rep(alpha_vc, each = nyear) # replicate the value so i can get a unique combination of year and variety

	randomEffectsPre$alpha_v[randomEffectsPre$rep == counter] <- alphaVar

	betaVars <- varEffectss[,2] # beta values for each variety 
	betaVars2 <- rep(betaVars, each = nyear) # replicate the value so i can get a unique combination of year and variety
	randomEffectsPre$beta_v[randomEffectsPre$rep == counter] <- betaVars2

}

randomEffectsPre$alphaBoth <- randomEffectsPre$alpha_v +  randomEffectsPre$sigma_year # the alpha for each variety, plus teh effect of year on alpha

#replot my actual data to remind me 
plot(simVarData$ simLTEVar ~ simVarData$simTemps, col = "grey74")

#add all the possible slopes of variety based on the priors 
for (i in 1:length(randomEffectsPre$alphaBoth)){
	abline(a = randomEffectsPre$alphaBoth[i], b = randomEffectsPre$beta_v[i], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.8))
}

plot(density(randomEffectsPre$alphaBoth))#potential alpha values for each variety/year combination 
plot(density(randomEffectsPre$beta_v))#potential slope (beta) values for each variety (year doesnt affect the slope)

#it is trying a lot of crazy values, but maybe that is ok?????

#The model
#------------------------------------------------------


head(simVarData)

x <- I(simVarData$simTemps)
y <- simVarData$simLTEVar
N <- length(simVarData$simTemps)
Variety <- as.integer(as.factor(simVarData$varNames ))
year <- as.integer(as.factor(simVarData$yearNamesObs ))
J <- length(unique(varNamesObs))
k <- length(unique(yearNamesObs))

#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame
stan_data3 <- list(N = N, x = x, y = y, n_vars = J, N = N, K = k, year = year, variety = Variety)




write("//

// This Stan program defines a linear model predicting LTE50 from temperature, with partial pooling of variety and year 
//
// Stan model for partially pooled linear regression including priors 

data {

	//Level 1
	int < lower = 1 > N; 					// Sample size - number of observations
	vector[N] x; 							// Predictor
	vector[N] y; 							// Outcome

	//Level 2 	
	int < lower = 1 > n_vars; 				// number of random effect levels (varieties) 
	int < lower = 1, upper = n_vars > variety[N]; // id of random effect (variety)

	int < lower = 1 > K; 					// number of random effect levels (years) 
	int < lower = 1, upper = K > year[N]; 	// id of random effect (year)

	}

parameters {

	//level 1
	real < upper = 0 > alpha_g; 			// mean intercept accross all varieties. Grand mean
	real beta_g;							//grand slope accross all varieties
	real <lower =0> sigma_y; 				// overall variation accross observations

	//level 2				  
	vector[n_vars] var_alpha;					// a new alpha for each variety, which includes grand alpha and effect of variety 
	vector[n_vars] var_beta;					// a new beta for each variety, which includes grand alpha and effect of variety 
	vector<lower = 0>[2] var_sigma; 		// a vector of standard deviations, one for alpha and one for beta (overall effect of variety)
	corr_matrix[2] Rho; 						// correlation between alpha and beta for effect of variety 

	real <lower = 0> sigma_k; 				// variation of intercept amoung varieties  
	real yearmu[K];

}
transformed parameters {

	//variety level alpha and beta 
	vector[2] mu_ab = [alpha_g, beta_g]'; 	//Vector of grand alpha and grand beta 
	vector[2] a_b_variety[n_vars];			//vector of an alpha and beta value for each varity 
	cov_matrix[2] SR_sigma_variety;			//the covarience matrix for the multinormal distribution 

	for (j in 1:n_vars){
		a_b_variety[j,1:2] = [var_alpha[j], var_beta[j]]';
	}
		SR_sigma_variety = quad_form_diag(Rho, var_sigma);

}

model{
  
  //Individual mean 
	real ymu[N];
	
	//Level 1
	alpha_g ~ normal(-15,12); 				// prior for grand alpha, assumes intercept will negative and around -10.
	//i chose this because -3 is minimum hardiness (least hardy) and few vines can manage temps much lower than -27
	beta_g ~ lognormal(0,1);
	sigma_y ~ normal(0,3); 					// prior around estiamted mean LTE50.

	//Level 2 - year
	yearmu ~ normal(0,sigma_k); 			// prior for the effect of random factor on grand mean 
	sigma_k ~ normal(0, 3); 				// prior for the variety around levels of random factor. Same as sigma_y

	//Level 2 - variety
	var_sigma ~ normal(0, 3); 				// prior for the variety effect that gets multiplied with rho (correlation)
	Rho ~ lkj_corr_lpdf(2); 				// prior for teh correlation between alpha and beta effect of variety 

	a_b_variety ~ multi_normal_lpdf(mu_ab , SR_sigma_variety);

	//liklihood
	for (i in 1:N){
		ymu[i] = var_alpha[variety[i]] + yearmu[year[i]] + var_beta[variety[i]] * x[i];
		y[i] ~ normal(ymu[i], sigma_y);
	}
}
generated quantities {


} // The posterior predictive distribution",

"stan_model_slope.stan")

stan_modelMulti6 <- "stan_model_slope.stan"

#Warining, it takes a while to run!
#fit6 <- stan(file = stan_modelMulti6, data = stan_data3, warmup = 500, iter = 1000, chains = 2, cores = 2, thin = 1,)

#if you do run it, you will probably see a warning about sampling stements. I think thats ok?
#it is because I a m transforming parameters by using the multinormal distribution. Usually when 
#you transform parameters you need to consider how thsi transformation changes the geometry. I 
#dont really understand though. See Overthinking box p. 398

#this model fits terribly - Non centred parameterisation to teh rescue!

#Non centred parameters in the correlation matrix
#---------------------------------------------------------

#what this does is basicly teh same as changing yi ~ N(ai + b*xi, sigma) to yi = ai + b*xi + ei

#what is Cholesky decomposition?
#Lower triangular matrix (L) * Lower triangular matrix TRANSPOSED (Lt)
#lower triangle * upper triangle 
#only works with a symetric maxtrix 
#https://www.youtube.com/watch?v=j1epLYdfqT4

### 13.1.3 the varying slopes model from Rethinking (https://github.com/ssp3nc3r/rethinking)
"
data {
  int N;
  int N_actors;
  int N_blocks;
  int L[N];
  int C[N];
  int P[N];
  int actor_id[N];
  int block_id[N];
}
parameters {
  real a;
  real bp;
  real bpc;
  vector[N_actors] za_actor;
  vector[N_actors] zbp_actor;
  vector[N_actors] zbpc_actor;
  
  vector[N_blocks] za_block;
  vector[N_blocks] zbp_block;
  vector[N_blocks] zbpc_block;
  
  vector<lower=0>[3] sigma_block;
  vector<lower=0>[3] sigma_actor;
  corr_matrix[3] Rho_actor;
  corr_matrix[3] Rho_block;
}
transformed parameters {
  vector[3] v_block[N_blocks];
  vector[3] v_actor[N_actors];
  for(j in 1:N_blocks) v_block[j] = [za_block[j], zbp_block[j], zbpc_block[j]]';
  for(j in 1:N_actors) v_actor[j] = [za_actor[j], zbp_actor[j], zbpc_actor[j]]';
}
model {
  vector[N] p;
  vector[N] A;
  vector[N] Bp;
  vector[N] Bpc;
  
  // priors
  target += lkj_corr_lpdf(Rho_block | 4);
  target += lkj_corr_lpdf(Rho_actor | 4);
  target += cauchy_lpdf(sigma_block | 0, 2);
  target += cauchy_lpdf(sigma_actor | 0, 2);
  target += normal_lpdf(a | 0, 1);
  target += normal_lpdf(bp | 0, 1);
  target += normal_lpdf(bpc | 0, 1); 
  
  target += multi_normal_lpdf(v_block | rep_vector(0, 3), Rho_block);
  target += multi_normal_lpdf(v_actor | rep_vector(0, 3), Rho_actor);
  
  // linear models
  A = a + za_actor[actor_id] * sigma_actor[1] + za_block[block_id] * sigma_block[1];
  Bp = bp + zbp_actor[actor_id] * sigma_actor[2] + zbp_block[block_id] * sigma_block[2];
  Bpc = bpc + zbpc_actor[actor_id] * sigma_actor[3] + zbpc_block[block_id] * sigma_block[3];
  for (i in 1:N) p[i] = A[i] + (Bp[i] + Bpc[i] * C[i]) * P[i];
  
  // likelihood
  target += binomial_logit_lpmf(L | 1, p);
}
generated quantities {
  vector[N] log_lik;
  {
  vector[N] p;
  vector[N] A;
  vector[N] Bp;
  vector[N] Bpc;
  
  A = a + za_actor[actor_id] * sigma_actor[1] + za_block[block_id] * sigma_block[1];
  Bp = bp + zbp_actor[actor_id] * sigma_actor[2] + zbp_block[block_id] * sigma_block[2];
  Bpc = bpc + zbpc_actor[actor_id] * sigma_actor[3] + zbpc_block[block_id] * sigma_block[3];
  for (i in 1:N) {
    p[i] = A[i] + (Bp[i] + Bpc[i] * C[i]) * P[i];
    log_lik[i] = binomial_logit_lpmf(L[i] | 1, p[i]);
  }
  }
}

"


write("//
// This Stan program defines a linear model predicting LTE50 from temperature, with partial pooling of variety and year 
//
// Stan model for partially pooled linear regression including priors 
//
//centered parameters for effect of variety on alpha and beta 

data {

	//Level 1
	int < lower = 1 > N; 						// Sample size - number of observations
	vector[N] x; 								// Predictor
	vector[N] y; 								// Outcome

	//Level 2 	
	int < lower = 1 > n_vars; 					// number of random effect levels (varieties) 
	int < lower = 1, upper = n_vars > variety[N]; // id of random effect (variety)

	int < lower = 1 > K; 						// number of random effect levels (years) 
	int < lower = 1, upper = K > year[N]; 		// id of random effect (year)

	}

parameters {

	//level 1
	real < upper = 0 > alpha_g; 				// mean intercept accross all varieties. Grand mean
	real beta_g;								//grand slope accross all varieties
	real <lower =0> sigma_y; 					// overall variation accross observations

	//level 2				  

	vector<lower = 0>[2] var_sigma; 			// a vector of standard deviations, one for alpha and one for beta (overall effect of variety)
	corr_matrix[2] Rho; 	
	vector[n_vars] za_variety;					// z score of alpha for effect of variety 
	vector[n_vars] zb_variety;					// z score of beta for effect of variety 


	real <lower = 0> sigma_k; 					// variation of intercept amoung varieties  
	real yearmu[K];

}
transformed parameters {

 	vector[2] v_variety[n_vars];				//vector of an z scores of alpha and beta value for each varity 
  	for(j in 1:n_vars) { 
  		v_variety[j] = [za_variety[j], zb_variety[j]]'; //put the two effects in a single list 
  	}

}

model{
  
	//---extra parametres 

	//alpha and beta for each variety
  	vector[n_vars] var_alpha;					// a new alpha for each variety, which includes grand alpha and effect of variety 
	vector[n_vars] var_beta;					// a new beta for each variety, which includes grand alpha and effect of variety 

	real ymu[N];								 //Individual mean predicted y value for each x value 
	
	//---Priors 

	//Level 1
	alpha_g ~ normal(-15,12); 					// prior for grand alpha, assumes intercept will negative and around -10.
	//i chose this because -3 is minimum hardiness (least hardy) and few vines can manage temps much lower than -27
	beta_g ~ lognormal(0,1);
	sigma_y ~ normal(0,3); 						// prior around estiamted mean LTE50.

	//Level 2 - year
	yearmu ~ normal(0,sigma_k); 				// prior for the effect of random factor on grand mean 
	sigma_k ~ normal(0, 3); 					// prior for the variety around levels of random factor. Same as sigma_y

	//Level 2 - variety
	var_sigma ~ normal(0, 3); 					// prior for the variety effect that gets multiplied with rho (correlation)
	Rho ~ lkj_corr_lpdf(2); 					// prior for teh correlation between alpha and beta effect of variety 

	target += multi_normal_lpdf(v_variety | rep_vector(0, 2), Rho);

	//---Linear model
	for(j in 1:n_vars){

		var_alpha[j] = alpha_g + za_variety[j] * var_sigma[1]; // get an alpha for each variety 
		var_beta[j] = beta_g + zb_variety[j] * var_sigma[2]; // get a beta for each variety  

	}

	for (i in 1:N){
		ymu[i] = var_alpha[variety[i]] + yearmu[year[i]] + var_beta[variety[i]] * x[i];

	}

	//---liklihood
	for (i in 1:N){	
		y[i] ~ normal(ymu[i], sigma_y);
	}
}
generated quantities {
  vector[N] var_alpha;
  vector[N] var_beta;
  vector[N] ymu;
  
  var_alpha = alpha_g + za_variety[variety] * var_sigma[1]; // get an alpha for each variety 
	var_beta = beta_g + zb_variety[variety] * var_sigma[2]; // get a beta for each variety  

  for (i in 1:N){
		ymu[i] = var_alpha[variety[i]] + yearmu[year[i]] + var_beta[variety[i]] * x[i];
	}
	
} // The posterior predictive distribution",

"slope_nonCentre.stan")

stan_modelMulti7 <- "slope_nonCentre.stan"


fit7 <- stan(file = stan_modelMulti7, data = stan_data3, warmup = 1000, 
	iter = 2000, chains = 2, cores = 2, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.80))


#interpreting the model
#--------------------------------

#Extra points
#-----------------------------------

#the different alpha and beta values for each variety (or cafe in the book example) are not all shrunken towards the middle alpha and 
#beta values. This is because of the correlation matrix. If, for example, the model shrinks the slope of a value because it looks
#unlikely, then it will also increase the intercept because they are negatively correlated.

#I am not sure what is meant by the parameter shrincage bit (p.107). WHy does more shrinkage mean fewer effective parameters? 