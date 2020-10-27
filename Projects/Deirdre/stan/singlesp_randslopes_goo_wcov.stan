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

// The parameters accepted by the model. Our model accepts two parameters 'mu' and 'sigma'.
parameters {
  
  real a ;
  real b[Nspp]; // slopes for species
  real<lower=0> sigma_y; //measurment error, noise 
  
  //hyperparameters
  real mu_b; // mean slope across sp
  real<lower=0> sigma_b; //var of slope among sp

//Adding covariance matrix:
//Attempt 1: Based on this vingette: https://willhipson.netlify.app/post/stan-random-slopes/varying_effects_stan/
   //matrix[N,Nspp] x; // should this not be year? Stan doesn't like it  
   // is this it? do you not need to reference the matrix again in the stan code? 
   
//Attempt 2 based on statistical rethinking ch 13
   corr_matrix[2] Rho; //just want the correlation between a and b (the slope and int) for the effect of year
   vector<lower = 0>[2] syn_sigma; 		// a vector of standard deviations, one for int and one for slope

}

transformed parameters{
  real mu_y[N]; //individual mean

  vector[2] ab = [a, mu_b]'; 	//Vector of grand int and grand slope
	vector[2] a_b_sp[Nspp];			//vector of an int and slope value for each species 
  cov_matrix[2] covmatrix; //the cov matrix for the multinormal dist
  covmatrix = quad_form_diag(Rho, syn_sigma);
  
  for(i in 1:N){
    mu_y[i]=a+b[species[i]]*year[i];
    
  }
}

// The model to be estimated. We model the output 'y' to be normally distributed with mean 'mu' and standard deviation 'sigma'.
model {

  b ~ normal(mu_b, sigma_b);
  ypred ~ normal(mu_y, sigma_y);


    //Priors
  a ~normal(188, 20); 
  mu_b ~normal(5,3); //could also be centred at zero, 10
  sigma_b ~normal(0,3); //sigma_b 0,10
  sigma_y ~normal(0,3); 

  syn_sigma ~ normal(0,5); // this prior is multiplied with rho, the correlation
  Rho ~ lkj_corr(2); // ljk is defining a weakly informative prior on sigma that does not give much weight to extreme correlations
  
  //a_b_sp ~ multi_normal_cholesky_lpdf(ab, covmatrix);  // why this distribution
  a_b_sp ~ multi_normal(ab, covmatrix);
}


generated quantities { //this block is evaluated after each iteration, uses the samples of all para to gen a new y and updates the posterior with prob of getting outcome with esti para and then it does the gen quan block
//no point in regenerating values again, but adds effort
//could compare a model that does both, compare the y_pred --> they should be identical

 real ypred_new[N];
  
   for (i in 1:N) // now over writing this with the sample dist, but this is already done for you in the transformed para block 
    ypred_new[i] = normal_rng(mu_y[species[i]], sigma_y);
  // does include the partial pooling because the mu-y does this above   
}

