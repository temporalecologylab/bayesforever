//Simple centred mixed effect model made by faith for stats class,
//based on code from Michael betancourt's course March 2021


data {
  int<lower=0> N;      // Number of observations
  vector[N] y;         // Observations
  
  // Number of individual contexts in hierarchy (number of trees in this case)
  int<lower=0> nSpec;
  
  // Species ID
  int<lower=1, upper=nSpec> SpecID[N]; 
}

parameters {
  real mu;           // Population location (mean tree hight)
  real<lower=0> sigma_sp; // Population scale (species level sigma)
  real <lower = 0> sigma_g; // gerenal varience  
  vector[nSpec] etaRaw; //non-centred parameter
}


transformed parameters {
  // Recentered  individual parameters for each species
  vector[nSpec] speciesHeight = mu + sigma_sp * etaRaw;
}


model {
  
  mu ~ normal(0, 5);                   // Prior model
  sigma_sp ~ normal(0, 5);                  // Prior model
  sigma_g ~ normal(0,10);             // Prior model
  etaRaw ~ normal(0,1) ;                //non-centred parameter 
  y ~ normal(speciesHeight[SpecID], sigma_g); // Observational model
}

// Simulate a full observation from the current value of the parameters
generated quantities {
  real y_post_pred[N] = normal_rng(speciesHeight[SpecID], sigma_g);
}
