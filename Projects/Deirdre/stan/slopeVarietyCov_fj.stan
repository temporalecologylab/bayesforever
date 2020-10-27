// This model gets convergent transition 

// This Stan program defines a linear model predicting LTE50 from temperature, with partial pooling of variety 
//
//thsi includes a correlation Rho between slope and interecpt effect of variety
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

	}

parameters {

	//level 1
	real < upper = 0 > alpha_g; 			// mean intercept accross all varieties. Grand mean
	real beta_g;							// grand slope accross all varieties
	real <lower =0> sigma_y; 				// overall variation accross observations

	//level 2				  
	vector[n_vars] var_alpha;					// a new alpha for each variety, which includes grand alpha and effect of variety 
	vector[n_vars] var_beta;					// a new beta for each variety, which includes grand alpha and effect of variety 
	vector<lower = 0>[2] var_sigma; 		// a vector of standard deviations, one for alpha and one for beta (overall effect of variety)
	corr_matrix[2] Rho; 						// correlation between alpha and beta for effect of variety 

}
transformed parameters {

	//variety level alpha and beta 
	vector[2] mu_ab = [alpha_g, beta_g]'; 	//Vector of grand alpha and grand beta 
	vector[2] a_b_variety[n_vars];			//vector of an alpha and beta value for each varity 
	cov_matrix[2] SR_sigma_variety;			//the covarience matrix for the multinormal distribution 

	SR_sigma_variety = quad_form_diag(Rho, var_sigma);

	for (j in 1:n_vars){
		a_b_variety[j,1:2] = [var_alpha[j], var_beta[j]]';
	}

}

model{
  
  //Individual mean 
	real ymu[N];
	
	//Level 1
	alpha_g ~ normal(-15,12); 				// prior for grand alpha, assumes intercept will negative and around -10.
	//i chose this because -3 is minimum hardiness (least hardy) and few vines can manage temps much lower than -27
	beta_g ~ lognormal(0,1);
	sigma_y ~ normal(0,3); 					// prior around estiamted mean LTE50.

	//Level 2 - variety
	var_sigma ~ normal(0, 3); 				// prior for the variety effect that gets multiplied with rho (correlation)
	Rho ~ lkj_corr_lpdf(2); 				// prior for teh correlation between alpha and beta effect of variety 

	a_b_variety ~ multi_normal_lpdf(mu_ab , SR_sigma_variety); 

	//liklihood
	for (i in 1:N){
		ymu[i] = var_alpha[variety[i]] + var_beta[variety[i]] * x[i];
		y[i] ~ normal(ymu[i], sigma_y);
	}
}
generated quantities {


} // The posterior predictive distribution
