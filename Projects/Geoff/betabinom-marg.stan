data {
  int<lower=1> N;
  int<lower=0> G[N];
  int<lower=0> D;
  int<lower=0> Days[D];
  
}

parameters {
  real shape1log;
  real shape2log;
}

transformed parameters {
  real shape1;
  real shape2;
  vector[D] lp;
  
  shape1 = exp(shape1log);
  shape2 = exp(shape2log);

  for(i in 1:D){
    lp[i] = 0;
    for(n in 1:N){
      /* print(G[n]); */
      /* print(Days[i]); */
      /* print(shape1); */
      /* print(shape2); */
      /* print(lp); */
      lp[i] = lp[i] + beta_binomial_lpmf(G[n] | Days[i], shape1, shape2);
	}
  }   
}
      
model {
  target += log_sum_exp(lp);

  shape1log ~ normal(0, 2);
  shape2log ~ normal(0, 3);
}

generated quantities {
  int<lower=0>D_new;
  int<lower=0>G_new;
  D_new = Days[categorical_rng(softmax(lp))];
  G_new = beta_binomial_rng(D_new, shape1, shape2);
}
    
