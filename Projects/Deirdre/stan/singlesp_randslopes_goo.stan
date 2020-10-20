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

//August 15 update: turning off partial pooling on intercept and removing constraints on the parameters -- better to do this with the priors

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //No. obs
  int<lower=0> Nspp; //No. spp
  int species[N]; // Grouping by species
  vector[N] year;
//response
  real ypred[N]; //DOY of pheno event
}

// The parameters accepted by the model. Our model accepts two parameters 'mu' and 'sigma'.
parameters {
  //seting random values for intercepts
  real mu_a;
  real<lower=0> sigma_a;
  //real<lower=0,upper=365> a[Nspp]; // intercept for species-- removing the constraints
  real a[Nspp]; // intercept for species 
  // setting random values for the slopes
  real mu_b_sp;
  real<lower=0> sigma_b_sp;
  real b[Nspp]; // slopes for species
  
  real<lower=0> sigma_y;
}

transformed parameters{
  real mu_y[N]; //individual mean
  
  for(i in 1:N){
    mu_y[i]=a[species[i]]+b[species[i]]*year[i];
    //mu_y[i]=a[species[i]]+b[species[i]]*year[i];
  }
}

// The model to be estimated. We model the outputn'y' to be normally distributed with mean 'mu' and standard deviation 'sigma'.
model {



  a ~ normal(mu_a, sigma_a); //using the previously defined values here 
  b ~ normal(mu_b_sp, sigma_b_sp);
  ypred ~ normal(mu_y, sigma_y);


    //Priors
  mu_a ~normal(188, 20); 
  sigma_a ~normal(0,20);
  mu_b_sp ~normal(5,3);
  sigma_b_sp ~normal(0,3); 
  //sigma_b_sp ~normal(0,200);
  sigma_y ~normal(0,3); 
}

generated quantities {
  vector[N] ypred_new;
  
  for (i in 1:N){
    ypred_new[i] = normal_rng(mu_y[species[i]], sigma_y);
    }
}
