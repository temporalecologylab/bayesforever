//
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

} // The posterior predictive distribution
