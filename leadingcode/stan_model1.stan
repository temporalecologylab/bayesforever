// Stan model for simple linear regression including priors 
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
} // The posterior predictive distribution
