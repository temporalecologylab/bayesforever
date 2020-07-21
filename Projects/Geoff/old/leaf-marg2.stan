data {
  int<lower=1> N;
  real<lower=0> G[N];
}

parameters {
  real mu_fresh;
  real mu_dried;
  real<lower=0> sigma_fresh;
  real<lower=0> sigma_dried;
  real logitp;
}

transformed parameters {
  real<lower=0, upper=1> p;
  vector[2] lp;

  p = inv_logit(logitp);

  for(i in 1:2){
    lp[i] = 0
      for(n in 1:N){
		    lp[n] = log(exp(normal_lpdf(G[n] | mu_fresh, sigma_fresh)) * p + exp(normal_lpdf(G[n] | mu_dried, sigma_dried)) * (1 - p));
  }
}
      
model {
  target += sum(lp);

  mu_fresh ~ normal(25, 2);
  mu_dried ~ normal(10, 2);
  sigma_fresh ~ normal(2, 1);
  sigma_dried ~ normal(1, 1);
  logitp ~ normal(0, 2);
}

generated quantities {
}
    
