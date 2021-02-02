//
//Synchrony Stan model
//model with both pooling on the slopes and intercepts

//August 15 update: turning off partial pooling on intercept and removing constraints on the parameters -- better to do this with the priors

//October 2020 update: adding a generated quantities block to calculate automatically ypred values for ppc
// Also trying to add a covariance matrix that would account for correlations between intercpet and the slope, this would help account for differences in the magnitude of changes across studies of different lengths

// January 29, 2021: adding studyid onto the intercept with a grand mean

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //No. obs
  
  int<lower=0> Nspp; //No. spp
  int species[N]; // Grouping by species
  
  int<lower=0> Nstudy; // No studies 
  int study[N]; // Grouping by study
  
  vector[N] year;
//response
  real ypred[N]; //DOY of pheno event
}

parameters {
  
  real mu_grand; //mean int across sp and study 
  //real a[Nspp]; // intercept for species, I will have Nspp number of intercepts 
  //real<lower=0> sigma_a; // variation in int among sp
  
  real b[Nspp]; // slopes for species, Nspp in total
  real mu_b; // mean slope across sp
  real<lower=0> sigma_b; //var of slope among sp

  real<lower=0> sigma_y; //measurement error, noise 
  
  real mu_sp[Nspp]; //effect of each sp without study
  real<lower=0> sigma_sp; // variation in int among sp
  
  real<lower=0> sigma_study; // variation in int among sp  
  real mu_study[Nstudy]; // mean of the alpha for studies
  // 
  // real<lower=0> sigma_a; // variation in int among sp
  // 
  // real b[Nspp]; // slopes for species, Nspp in total
  // real mu_b; // mean slope across sp
  // real<lower=0> sigma_b; //var of slope among sp
}

// The model to be estimated. We model the output 'y' to be normally distributed with mean 'mu' and standard deviation 'sigma'.

model {

  real mu_y[N]; //individual mean
 
  for(i in 1:N){
    mu_y[i]= mu_grand + mu_sp[species[i]] + mu_study[study[i]] + b[species[i]] * year[i];
  }
  
  sigma_y ~ normal(0,10); 
  
  //mu_a ~ normal(188, 50); 
  mu_grand ~ normal (0, 10);
  
  sigma_sp ~ normal (0, 10);
  mu_sp ~ normal(0, sigma_sp);

  sigma_study ~ normal(0,10);
  mu_study ~ normal(0, sigma_study);

  mu_b ~normal(0,10); //could also be centred at zero, 10
  sigma_b ~normal(0,10); //sigma_b 0,10

  ypred ~ normal(mu_y, sigma_y);
}

generated quantities { //this block is evaluated after each iteration, uses the samples of all para to gen a new y and updates the posterior with prob of getting outcome with esti para and then it does the gen quan block
//no point in regenerating values again, but adds effort
//could compare a model that does both, compare the y_pred --> they should be identical

 real ypred_new[N];
 
   for (i in 1:N) { // now over writing this with the sample dist, but this is already done for you in the transformed para block 
    ypred_new[i]= mu_grand + mu_sp[species[i]] + mu_study[study[i]] +b [species[i]] * year[i];
    ypred_new[i] = normal_rng(ypred_new[i], sigma_y);
   }
  // does include the partial pooling because the mu-y does this above   
}




