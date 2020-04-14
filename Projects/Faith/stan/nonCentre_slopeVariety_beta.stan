// Thsi model doesnt get divergent transitions, but doesnt do the best job of estimating parameters 
  //Stan model of hardiess against temp, with partical pooling for variety on slope 
  // and intercept, but no covarience between the two
  //thsi mode also has non-centred parameterisation for sigma_beta_var  

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
  real <lower = 0> sigma_beta_v; // variation around the grand slope for varieties 
  real varAlpha[n_vars]; // a list of the effect of each variety on the slope
  real zb_variety[n_vars]; // prior for the z bit of the non centerd bit for alpha variety 

}
transformed parameters{
  //variety mean
  real alpha_var[n_vars];

  //variety slope
  real ybeta[n_vars];

  //Variety means
  for (j in 1:n_vars){
    alpha_var[j] = alpha_g + varAlpha[j]; // non-centred effect of variety on alpha  
    ybeta [j] = beta_g +  zb_variety[j] * sigma_beta_v;
  }
 

}

model{
  //Individual mean 
  real ymu[N];
  
  //Level 1
  alpha_g ~ normal(-15,12); // prior for grand alpha, assumes intercept will negative and around -10.
  //i chose this because -3 is minimum hardiness (least hardy) and few vines can manage 
  //temps much lower than -27
  beta_g ~ lognormal(0,1); // prior around teh grand mean slope 
  sigma_y ~ normal(0,5); // prior around estiamted mean LTE50.

  //Level 2
  zb_variety ~ normal(0,1);  // prior for the z bit of the non centerd bit for alpha variety
  sigma_beta_v ~ normal(0, 5); // prior for the variety around levels of random factor. Same as sigma_y
  varAlpha ~ normal(0, sigma_alpha_v); //prior for the effect of variety on slope 
  sigma_alpha_v ~ normal(0, 1); // prior for the variation around 

  //liklihood
  for (i in 1:N){
    ymu[i] = alpha_var[variety[i]] + ybeta[variety[i]] * x[i];  //mean
    y[i] ~ normal(ymu[i] , sigma_y); // mean plus extra noise 
  }
      
}


generated quantities {

  real realY[N]; 

  for (i in 1:N){
  realY[i] = alpha_var[variety[i]] + ybeta[variety[i]] * x[i];
  }
}
