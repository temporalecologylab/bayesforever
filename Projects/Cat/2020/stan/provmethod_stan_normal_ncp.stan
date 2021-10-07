// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] prov; 	// provenance predictor
	vector[N] method; 	// method predictor
	
		
	}

parameters {
  real mu_a_sp;   
  real mu_b_prov_sp; 
  real mu_b_method_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_prov_sp;
  real<lower=0> sigma_b_method_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  
  vector[n_sp] b_prov_ncp; // NCP slope of provenance effect 
  vector[n_sp] b_method_ncp; // NCP slope of method effect
  
  real<lower=0> sigma_prov_ncp;  
  real<lower=0> sigma_method_ncp;

	}

transformed parameters {
  vector[n_sp] b_prov; 
  vector[n_sp] b_method;
  vector[N] yhat;

   b_prov = mu_b_prov_sp + sigma_prov_ncp*b_prov_ncp;
   b_method = mu_b_method_sp + sigma_method_ncp*b_method_ncp; 
  
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_prov[sp[i]] * prov[i] +
		b_method[sp[i]] * method[i];
	}
}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	
	b_prov ~ normal(0,20);
	b_method ~ normal(0,150);

        mu_a_sp ~ normal(400, 50);
        sigma_a_sp ~ normal(0, 50);

        mu_b_prov_sp ~ normal(0, 20);
        sigma_b_prov_sp ~ normal(0, 10);
        
        mu_b_method_sp ~ normal(0, 150);
        sigma_b_method_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_prov[sp[n]] * prov[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}