//
// Synchrony stan model: 
// Basic model with just partial pooling on the slopes

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //No. obs
  int<lower=0> Nspp; //No. spp
  int species[N]; // Grouping by species
  vector[N] year;
//response
  real ypred[N]; //DOY of pheno event
}

parameters {
  real a[Nspp]; // intercept for each sp, but not partially pooled
  real b[Nspp]; // slopes for species

  real<lower=0> sigma_y; //measurement error, noise 

//hyperparameters
  real mu_b; // mean slope across sp
  real<lower=0> sigma_b; //var of slope among sp

}

transformed parameters{
  real mu_y[N]; //individual mean
  
  for(i in 1:N){
    mu_y[i]= a[species[i]] +b[species[i]]*year[i]; 
  }
}

// The model to be estimated. We model the output 'y' to be normally distributed with mean 'mu' and standard deviation 'sigma'.
model {
  b ~ normal(mu_b, sigma_b);
    //Priors
  a ~ normal(180, 50); 
  mu_b ~normal(5,3); //could also be centred at zero, 10
  sigma_b ~normal(0,10); //sigma_b 0,10
  sigma_y ~normal(0,10); 
  ypred ~ normal(mu_y, sigma_y);

}

// 
generated quantities { //this block is evaluated after each iteration, uses the samples of all para to gen a new y and updates the posterior with prob of getting outcome with esti para and then it does the gen quan block
//no point in regenerating values again, but adds effort
//could compare a model that does both, compare the y_pred --> they should be identical

 real ypred_new[N];

   for (i in 1:N) // now over writing this with the sample dist, but this is already done for you in the transformed para block
    ypred_new[i] = normal_rng(mu_y[i], sigma_y);
  // does include the partial pooling because the mu-y does this above
}


//Block from OSPREE model, see above explanation for why don't need to do it this way
// generated quantities {
// 
//  real ypred_temp[N];
//  real ypred_new[N];
// 
//  for (i in 1:N) // generating  a dew mu, calculated for each i
//    ypred_temp[i]=a+b[species[i]]*year[i];
//   for (i in 1:N) // now over writing this with the sample dist, but this is already done for you in the transformed para block
//    ypred_new[i] = normal_rng(ypred_temp[species[i]], sigma_y);
// 
// }
