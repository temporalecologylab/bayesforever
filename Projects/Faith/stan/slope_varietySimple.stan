//no divergent transition sbut not doing a great job of estiamting 
    //Stan model of hardiess against temp, with partical pooling for variety on slope 
  // and intercept, but no covarience between the two
  //thsi model has no non-centring/zscoring of parameters 

data{

  //Level 1
  int < lower = 1 > N; // Sample size - number of observations
  vector[N] x; // Predictor
  vector[N] y; // Outcome

  //Level 2 
  int < lower = 1 > n_vars; // number of random effect levels (varieties) 
  int < lower = 1, upper = n_vars > variety[N]; // id of random effect (variety)

  }

parameters {

  //level 1
  real < upper = -3 > alpha_g; // mean intercept accross all varieties. Grand mean
  real beta_g; //slope accross all varieties
  real <lower =0> sigma_y; // overall variation accross observations

  //level 2
  real <lower = 0> sigma_alpha_v; // variation of intercept amoung varieties  
  real varmu[n_vars]; // a list of the effect of each variety on the intercept 
  real <lower = 0> sigma_beta_v; // variation around the grand slope for varieties 
  real varbeta[n_vars]; // a list of the effect of each variety on the slope


}
transformed parameters{
  //Individual mean 
  real ymu[N];

  //variety slope
  real ybeta[N];
 
  //Individual mean and slope calculation 
  for (i in 1:N){

    ymu[i] = alpha_g + varmu[variety[i]];  //mean
    ybeta[i] = beta_g + varbeta[variety[i]]; //slope 
  
  }

}

model{
  //Level 1
  alpha_g ~ normal(-15,12); // prior for grand alpha, assumes intercept will negative and around -10.
  //i chose this because -3 is minimum hardiness (least hardy) and few vines can manage 
  //temps much lower than -27
  beta_g ~ lognormal(0,1); // prior around teh grand mean slope 
  sigma_y ~ normal(0,5); // prior around estiamted mean LTE50.

  //Level 2
  varmu ~ normal(0,sigma_alpha_v); // prior for the effect of random factor on grand mean 
  sigma_alpha_v ~ normal(0, 5); // prior for the variety around levels of random factor. Same as sigma_y
  varbeta ~ normal(0, sigma_beta_v); //prior for the effect of variety on slope 
  sigma_beta_v ~ normal(0, 1); // prior for the variation around 

  //liklihood
  for (i in 1:N){
    y[i] ~ normal(ymu[i] + ybeta[i] * x[i], sigma_y);
  }
}

generated quantities {

  real realY[N]; 

  for (i in 1:N){
  realY[i] = ymu[i] + ybeta[i] * x[i];
  }
}