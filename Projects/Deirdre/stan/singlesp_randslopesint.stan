//
//Synchrony Stan model
//model with both pooling on the slopes and intercepts

//August 15 update: turning off partial pooling on intercept and removing constraints on the parameters -- better to do this with the priors

//October 2020 update: adding a generated quantities block to calculate automatically ypred values for ppc
// Also trying to add a covariance matrix that would account for correlations between intercpet and the slope, this would help account for differences in the magnitude of changes across studies of different lengths

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
  real a[Nspp] ;// intercept for species, I will have Nspp number of intercepts 
  real b[Nspp]; // slopes for species, Nspp in total

  real mu_a; //mean int across sp -- with pooled inttth
  real<lower=0> sigma_a; // variation in int among sp
  real mu_b; // mean slope across sp
  real<lower=0> sigma_b; //var of slope among sp

  real<lower=0> sigma_y; //measurement error, noise 
  
}

transformed parameters{
  real mu_y[N]; //individual mean

  
  for(i in 1:N){

    mu_y[i]=a[species[i]]+b[species[i]]*year[i];
  }
}
// here relating the values to the factors, naming each for each species
  


// The model to be estimated. We model the output 'y' to be normally distributed with mean 'mu' and standard deviation 'sigma'.

model {
a ~ normal(mu_a, sigma_a);
b ~ normal(mu_b, sigma_b);

    //Priors
mu_a ~normal(188, 50); 
sigma_a ~normal(0,50);
mu_b ~normal(0,10); //could also be centred at zero, 10
sigma_b ~normal(0,10); //sigma_b 0,10
sigma_y ~normal(0,10); 

ypred ~ normal(mu_y, sigma_y);
}


generated quantities { //this block is evaluated after each iteration, uses the samples of all para to gen a new y and updates the posterior with prob of getting outcome with esti para and then it does the gen quan block
//no point in regenerating values again, but adds effort
//could compare a model that does both, compare the y_pred --> they should be identical

 real ypred_new[N];
 
   for (i in 1:N) { // now over writing this with the sample dist, but this is already done for you in the transformed para block 
    ypred_new[i] = normal_rng(mu_y[i], sigma_y);
   }
  // does include the partial pooling because the mu-y does this above   
}




