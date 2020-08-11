//A Dose Response Curve with no partial pooling of varience 
//Based on a logistic 4 parameter regression 
//started by Faith Jones 1/7/2020 (Canada day!)

//based on code from https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling/13823
//needs x and y to be positive 
// thsi model is different from doseResponseSimple in that ehat rather than e is used. 

data {
  int<lower=1> N;                           // Number of observations
  vector<lower=1>[N] x;                      // Dose values (air temperature)
  vector [N] y;                                 // Response values (Winter cold hardiness). was an int in original code 
}

// Sampling space
parameters {
  real <lower=0>b;                             // slope
  real<lower=0> c;                     // lower asymptote
  real<lower=0> d;                     // upper asymptote
  real<lower=0> ehat;                           //  x where y is half way between c and d, but logged in equation
  real<lower=0> sigma_g;                        //error around the estimated hardiness 
}

// Calculate posterior
model {
  vector[N] mu_y;

  // Priors on parameters
  c ~ gamma(3, 1);                    // centred around 3, coudl very towards 0.  
  d ~ normal(25, 10);               // centred around maximum hardiness, 10 se 
  ehat ~ normal(log(30), 0.15);   // centred around mean temperature, se 20
  b ~ gamma(7, 0.75);                 // centred around 1, not really sure of appropriate prior tbh  
  sigma_g ~ normal(0, 5);           // I have little concept of how much variation there shoudl be, thsi is a stab in the dark

  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = c + ((d - c) / (1 + exp(b * (log(x[i]) - ehat))));
  }
  y ~ normal(mu_y, sigma_g); // I dont know why, but this was a poisson distribution in the original model 

}


generated quantities {
  
  // Simulate model configuration from prior model (get mu_y)
  vector[N] mu_y;                      // Simulated mean data from likelyhood 
  vector[N] y_sim;                           //Simulated Data
  real<lower=0> e;
  e = exp(ehat);
  
  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = c + ((d - c) / (1 + exp(b * (log(x[i]) - ehat))));
  }
  // Simulate data from observational model
  for (n in 1:N) y_sim[n] = normal_rng(mu_y[n], sigma_g); 
}

