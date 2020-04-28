data {
  int<lower=1> N;
  int<lower=0> D;
  int<lower=0> G[N];
  
}

parameters {
  real<lower=0> shape1;
  real<lower=0> shape2;
}

transformed parameters {
}
      
model {
  G ~ beta_binomial(D, shape1, shape2);

  shape1 ~ normal(30, 10);
  shape2 ~ normal(250, 10);
}

generated quantities {
  int<lower=0,upper=D>G_new;
    G_new = beta_binomial_rng(D, shape1, shape2);
}
    