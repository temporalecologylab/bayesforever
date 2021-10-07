// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
  
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] urban; 	// urban predictor
	vector[N] hobo; 	// hobo predictor
		
	}

parameters {
  
  real mu_a_sp;   
  real mu_b_urban_sp;     
  real mu_b_hobo_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_urban_sp;
  real<lower=0> sigma_b_hobo_sp;
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
  
  vector[n_sp] b_urban_ncp; // NCP slope of urban effect
  vector[n_sp] b_hobo_ncp; // NCP slope of hobo effect
  
  real<lower=0> sigma_urban_ncp;  
  real<lower=0> sigma_hobo_ncp;

	}

transformed parameters {
  vector[n_sp] b_urban; 
  vector[n_sp] b_hobo;
  vector[N] yhat;

  b_urban = mu_b_urban_sp + sigma_urban_ncp*b_urban_ncp; 
  b_hobo = mu_b_hobo_sp + sigma_hobo_ncp*b_hobo_ncp; 
       	
  for(i in 1:N){    
    yhat[i] = a_sp[sp[i]] + // indexed with species
		          b_urban[sp[i]] * urban[i] + 
		          b_hobo[sp[i]] * hobo[i];
	      }
	      
}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	
	b_urban ~ normal(0, 75);
	b_hobo ~ normal(0, 150);
	      
        mu_a_sp ~ normal(400, 50);
        sigma_a_sp ~ normal(0, 50);

        mu_b_urban_sp ~ normal(0, 75);
        sigma_b_urban_sp ~ normal(0, 10);
        
        mu_b_hobo_sp ~ normal(0, 150);
        sigma_b_hobo_sp ~ normal(0, 10);
	      
	y ~ normal(yhat, sigma_y);

}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_urban[sp[n]] * urban[n] +
		b_hobo[sp[n]] * hobo[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}