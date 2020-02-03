// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
		
	}

parameters {
  real mu_a_sp;     
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species 

	}

transformed parameters {
  vector[N] yhat;
  
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]];
	}
}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp);  

        mu_a_sp ~ normal(600, 50);
        sigma_a_sp ~ uniform(0, 1);

	y ~ normal(yhat, sigma_y);
	
}