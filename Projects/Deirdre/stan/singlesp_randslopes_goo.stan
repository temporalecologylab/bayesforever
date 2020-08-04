//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //No. obs
  int<lower=0> Nspp; //No. spp
  int species[N]; // Grouping by species
  vector[N] year;
//response
  real ypred[N]; //DOY of pheno event
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //seting random values for intercepts
  real mu_a_sp;
  real<lower=0> sigma_a_sp;
  real<lower=0,upper=365> a[Nspp]; // intercept for species
  
  real mu_b_sp;
  real<lower=0> sigma_b_sp;
  real b[Nspp]; // slopes for species
  
  real<lower=0> sigma_y;
}

transformed parameters{
  real mu_y[N]; //individual mean
  
  for(i in1:N){
    mu_y[i]=a[species[i]]+b[species[i]]*year[i];
  }
}
    
// The model to be estimated. We model the outputn'y' to be normally distributed with mean 'mu' and standard deviation 'sigma'.
model {
  a ~ normal(mu_a_sp, sigma_a_sp); //using the previously defined values here 
  b ~ normal(mu_b_sp, sigma_b_sp);
  ypred ~ normal(mu_y, sigma_y);
  
    //Priors
  mu_a_sp ~normal(188, 20); 
  sigma_a_sp ~normal(0,20);
  mu_b_sp ~normal(5,2);
  sigma_b_sp ~normal(0,2); 
  sigma_y ~normal(0,2); 
}
