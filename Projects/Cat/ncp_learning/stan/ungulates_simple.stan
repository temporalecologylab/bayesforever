// NCP and stan model block learning
// Started by Cat 26 January 2021
// Level: Species on INTERCEPTS and SLOPES

data {
  
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] prov; 	// provenance predictor
	vector[N] leglength; 	// leg length predictor
		
	}
	
/*transformed data {
  vector[N] inter_provleg;                 

  inter_provleg = prov .* leglength; 
}*/

parameters {
  
  real mu_a_sp;   
  real mu_b_prov_sp;     
  real mu_b_leg_sp;
  //real mu_b_pl_sp; // slope of prov x leg length effect
  
  real<lower=0> sigma_b_prov_sp;
  real<lower=0> sigma_b_leg_sp;
  //real<lower=0> sigma_b_pl_sp;
  real<lower=0> sigma_a_sp;
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
  
  vector[n_sp] b_prov; // slope of prov effect 
  vector[n_sp] b_leg; // slope of leg length effect 
  //vector[n_sp] b_pl;
  
  //vector[n_sp] b_prov_raw; // slope of prov effect 
  //vector[n_sp] b_leg_raw; // slope of leg length effect 
  //vector[n_sp] b_pl_raw;
  
	}

transformed parameters {
  vector[N] yhat;

  //vector[n_sp] b_prov = mu_b_prov_sp + sigma_b_prov_sp*b_prov_raw; 
  //vector[n_sp] b_leg = mu_b_leg_sp + sigma_b_leg_sp*b_leg_raw; 
  //vector[n_sp] b_pl = mu_b_pl_sp + sigma_b_pl_sp*b_pl_raw;
  
  for(i in 1:N){    
    yhat[i] = a_sp[sp[i]] + // indexed with species
		          b_prov[sp[i]] * prov[i] + 
		          b_leg[sp[i]] * leglength[i];// +
		          //b_pl[sp[i]] *  inter_provleg[i];
	      }
	      
}

model {
	//b_prov_raw ~ normal(0, 1);
	//b_leg_raw ~ normal(0, 1);
	//b_pl_raw ~ normal(0, 1);
	
	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	
	target += normal_lpdf(to_vector(b_prov) | mu_b_prov_sp, sigma_b_prov_sp); // just another way to write normal()
	target += normal_lpdf(to_vector(b_leg) | mu_b_leg_sp, sigma_b_leg_sp);
	//target += normal_lpdf(to_vector(b_pl) |  mu_b_pl_sp, sigma_b_pl_sp;
	
	/// NOTE ON NCP: since we used NCP, the b_prov etc are assumed to be partially pooled, if we did 
	 // b_prov(mu_b_prov_sp, sigma_b_prov_sp) it would confuse the model because it would be redundant
	  
	//target += normal_lpdf(to_vector(b_prov) | 0, 75); // just another way to write normal()
	//target += normal_lpdf(to_vector(b_leg) | 0, 75);
	//target += normal_lpdf(to_vector(b_pl) | 0, 75);
	      
        mu_a_sp ~ normal(400, 75);
        sigma_a_sp ~ normal(0, 50);

        mu_b_prov_sp ~ normal(0, 75);
        sigma_b_prov_sp ~ normal(0, 30);
        
        mu_b_leg_sp ~ normal(0, 75);
        sigma_b_leg_sp ~ normal(0, 30);
        
        mu_b_pl_sp ~ normal(0, 75);
	      sigma_b_pl_sp ~ normal(0, 30);
        
        sigma_y ~ normal(0, 100);
	      
	y ~ normal(yhat, sigma_y);

}

/*generated quantities{ /// include if you want to look at posterior predictive checks
   real y_ppc[N];
   
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_prov[sp[n]] * prov[n] +
		b_leg[sp[n]] * leglength[n] +
		b_pl[sp[n]] * inter_provleg[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}*/