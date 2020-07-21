data {
  int<lower=1> N_temp_rec;
  vector[N_temp_rec] temp_time; // Days
  vector[N_temp_rec] temp_rec;  // Celcius
}

parameters {
  real<lower=0> temp_amp;  // Amplitude in Celcius
  real<lower=0> temp_per;  // Period in days
  real temp_phase;         // Phase in days
  real temp_baseline;      // Baseline temperature in Celcius
 
  real<lower=0> temp_sigma; // Variability of temperature recording
}

transformed parameters {
  vector[N_temp_rec] model_temp =   temp_amp * sin(2 * pi() * (temp_time - temp_phase) / temp_per) 
                                  + temp_baseline;
}

model {
  // Prior model
  temp_amp ~ normal(0, 10);
  temp_per ~ normal(360, 10);
  temp_phase ~ normal(100, 10);
  temp_baseline ~ normal(0, 10);

  temp_sigma ~ normal(0, 10);
  
  // Observational model
  temp_rec ~ normal(model_temp, temp_sigma);
}

generated quantities {
  real temp_rec_pred[N_temp_rec] = normal_rng(model_temp, temp_sigma);
}
