//A Dose Response Curve with no partial pooling of varience 
//Based on a logistic 4 parameter regression 
//started by Faith Jones 1/7/2020 (Canada day!)

//based on code from https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling/13823
//needs x and y to be positive 

data {
  int<lower=1> N;                           // Number of observations
  vector<lower=1>[N] x;                      // Dose values (air temperature)
}

generated quantities {
  
  // Simulate model configuration from prior model (get mu_y)
  vector[N] mu_y;                      // Simulated mean data from likelyhood 
  vector[N] y_sim;                           //Simulated Data
  real<lower = 0> b;                     // slope
  real<lower=0> c;                     // lower asymptote
  real<lower=0> d;                     // upper asymptote. Cant go below 
  real<lower=0> ehat;                           //  x where y is half way between c and d, but logged in equation
  real<lower=0> sigma_g; 
  real<lower=0> e;

  // Priors on parameters
  // NOTE THE FABS IS ESSENTIAL FOR LOWER BOUND = 0, otherwise there will be lost of negative numbers 
  c = fabs(gamma_rng(3, 1));                    // centred around no hardiness, could be as high as -20
  d = fabs(normal_rng(25, 10));               // centred around maximum hardiness, 10 sd 
  ehat = fabs(normal_rng(log(30), 0.15));   // centred around roughly mean temperature (plus 30, sd 0.2exp(0.2)
  b = gamma_rng(7, 1);                 // centred around o, not really sure of appropriate prior tbh  
  sigma_g = fabs(normal_rng(0, 5));           // I have little concept of how much variation there shoudl be, thsi is a stab in the dark

  e = exp(ehat);
  
  //liklyhood function 
  for (i in 1:N) {
    mu_y[i] = c + ((d - c) / (1 + exp(b * (log(x[i]) - ehat))));
  }
  // Simulate data from observational model
  for (n in 1:N) y_sim[n] = normal_rng(mu_y[n], sigma_g); 
}
