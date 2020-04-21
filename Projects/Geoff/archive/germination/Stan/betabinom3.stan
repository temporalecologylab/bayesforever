data {
  int<lower=1> N;
  int<lower=0> D;
  int<lower=0> G[N];
  
}

parameters {
  real shape1log;
  real shape2log;
}

transformed parameters {
  real<lower=0> shape1;
  real<lower=0> shape2;
  shape1 = exp(shape1log);
  shape2 = exp(shape2log);
}
      
model {
  G ~ beta_binomial(D, shape1, shape2);

  shape1log ~ normal(0, 5);
  shape2log ~ normal(0, 10);
}

generated quantities {
  int<lower=0,upper=D>G_new;
    G_new = beta_binomial_rng(D, shape1, shape2);
}
    
