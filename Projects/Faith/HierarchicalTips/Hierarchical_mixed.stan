//Simple centred mixed effect model made by faith for stats class,
//based on code from Michael betancourt's course March 2021

//combined centred and non centred parameterisation for species values 


data {
  int<lower=0> N;      // Number of observations
  vector[N] y;         // Observations
  
  // Number of individual contexts in hierarchy (number of trees in this case)
  int<lower=0> nSpec;
  
  // Species ID
  int<lower=1, upper=nSpec> SpecID[N]; 
  
  // sorting out which species to centre and which to ncp
  int<lower=0, upper=nSpec> nSpec_ncp;          // Number of noncentered species
  int<lower=1, upper=nSpec> ncp_idx[nSpec_ncp]; // Index of noncentered species
  
  int<lower=0, upper=nSpec> nSpec_cp;           // Number of centered species
  int<lower=1, upper=nSpec> cp_idx[nSpec_cp];   // Index of noncentered species
}

parameters {
  real mu;           // Population location (mean tree hight)
  real<lower=0> sigma_sp; // Population scale (species level sigma)
  real <lower = 0> sigma_g; // gerenal varience 
  
  vector[nSpec_ncp] sigma_etaRaw_ncp;  // Non-centered species parameters. wan etaRaw in ncp model. 
  vector[nSpec_cp]  sigma_sp_cp;   // Centered species parameters
  
}


transformed parameters {
  // Recentered individual parameters
  vector[nSpec] speciesHeight;
  speciesHeight[ncp_idx] = mu + sigma_etaRaw_ncp * sigma_sp;
  speciesHeight[cp_idx] = sigma_sp_cp;
  
}


model {
  
  mu ~ normal(0, 5);                   // Prior model
  sigma_sp ~ normal(0, 5);                  // Prior model for variance of species 
  sigma_etaRaw_ncp ~ normal(0,1) ;                //prior for non-centred parameter 
  sigma_sp_cp ~ normal(mu, sigma_sp); //prior for centred species observations
  sigma_g ~ normal(0,10);             // Prior model for general varience 
  y ~ normal(speciesHeight[SpecID], sigma_g); // Observational model
}

// Simulate a full observation from the current value of the parameters
generated quantities {
  real y_post_pred[N] = normal_rng(speciesHeight[SpecID], sigma_g);
}
