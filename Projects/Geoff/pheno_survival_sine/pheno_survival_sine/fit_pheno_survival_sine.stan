functions {
  real temp(real time, real phase, real period, real amp, real baseline) {
    return amp * sin(2 * pi() * (time - phase) / period) + baseline;
  }
  
  vector temps(vector times, real phase, real period, real amp, real baseline) {
    return amp * sin(2 * pi() * (times - phase) / period) + baseline;
  }
  
  // Wang-Engels Rescaled Temperature
  real x_WE(real T, real T_min, real T_max, real T_opt) {
    real alpha = log(2) / log( (T_max - T_min) / (T_opt - T_min) );
    return 0.5 * ( (T - T_min) / (T_opt - T_min) )^alpha;
  }
  
  // Logarithm of Wang-Engels Rescaled Temperature
  real log_x_WE(real T, real T_min, real T_max, real T_opt) {
    real alpha = log(2) / log( (T_max - T_min) / (T_opt - T_min) );
    return -0.6931471 + alpha * log( (T - T_min) / (T_opt - T_min) );
  }
  
  // Log hazard function in inverse days
  real log_hazard(real t, real phase, real period, real amp, real baseline,
                  real gamma, real T_min, real T_max, real T_opt) {
    real T = temp(t, phase, period, amp, baseline);
        
    if (T < T_min) {
      return negative_infinity();
    } else if (T > T_max) {
      return negative_infinity();
    } else {
      real log_x = log_x_WE(T, T_min, T_max, T_opt);
      return log(gamma) + 1.3862943 + log_x + log_sum_exp(0, log_x);
    }
    return 0;
  }
  
  // Hazard function in inverse days
  real hazard(real t, real phase, real period, real amp, real baseline,
              real gamma, real T_min, real T_max, real T_opt) {
    real T = temp(t, phase, period, amp, baseline);
    if (T < T_min)
      return 0;
    else if (T > T_max) 
      return 0;
    else {
      real x = x_WE(T, T_min, T_max, T_opt);
      return gamma * 4 * x * (1 - x);
    }
  }
  
  // Hazard function formatted for Stan's numerical integrator
  real hazard_integrand(real x, real xc, real[] theta, real[] x_r, int[] x_i) {
    real phase    = theta[1];
    real period   = theta[2];
    real amp      = theta[3];
    real baseline = theta[4];
    real gamma    = theta[5];
    real T_min    = theta[6];
    real T_max    = theta[7];
    real T_opt    = theta[8];
    return hazard(x, phase, period, amp, baseline, gamma, T_min, T_max, T_opt);
  }
  
  // Survival function
  real log_survival(real t, real phase, real period, real amp, real baseline,
              real gamma, real T_min, real T_max, real T_opt, real[] x_r, int[] x_i) {
    real theta[8] = {phase, period, amp, baseline, gamma, T_min, T_max, T_opt};
    return -integrate_1d(hazard_integrand, 1, t, theta, x_r, x_i, 0.01);
  }
}

data {
  int<lower=1> N;
  real obs_tran_time[N]; // Days since dormancy (around 230)
  
  int<lower=1> N_temp_rec;
  vector[N_temp_rec] temp_time; // Days
  vector[N_temp_rec] temp_rec;  // Celcius
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
  real<lower=0> temp_amp;  // Amplitude in Celcius
  real<lower=0> temp_per;  // Period in days
  real temp_phase;         // Phase in days
  real temp_baseline;      // Baseline temperature in Celcius

  real<lower=0> temp_sigma; // Variability of temperature recording
  
  real T_min;
  real T_opt;
  real T_max;
  real<lower=0> gamma;
}

transformed parameters {
  vector[N_temp_rec] latent_temp = temps(temp_time, temp_phase, temp_per, temp_amp, temp_baseline);
}

model {
  // Prior model
  temp_amp ~ normal(0, 10);
  temp_per ~ normal(360, 10);
  temp_phase ~ normal(100, 10);
  temp_baseline ~ normal(0, 10);

  temp_sigma ~ normal(0, 10);
  
  T_min ~ normal(4, 0.75);
  T_max ~ normal(40, 1.5);
  T_opt ~ normal(26, 2);
  gamma ~ gamma(4.63, 22.07);
  
  // Temperature bservational model
  temp_rec ~ normal(latent_temp, temp_sigma);
  
  // Phenology observational model
  for (n in 1:N) {
    target +=   log_survival(obs_tran_time[n], 
                             temp_phase, temp_per, temp_amp, temp_baseline,
                             gamma, T_min, T_max, T_opt, x_r, x_i) 
              + log_hazard(obs_tran_time[n], 
                           temp_phase, temp_per, temp_amp, temp_baseline,
                           gamma, T_min, T_max, T_opt);
  }
}

generated quantities {
  real temp_rec_pred[N_temp_rec] = normal_rng(latent_temp, temp_sigma);
  real integrated_we_pred[N_temp_rec];
  real survival_pred[N_temp_rec];
  {
    for (n in 1:N_temp_rec) {
      real int_hazard = log_survival(n, 
                                     temp_phase, temp_per, temp_amp, temp_baseline,
                                     gamma, T_min, T_max, T_opt, x_r, x_i);
      integrated_we_pred[n] = -int_hazard / gamma;
      survival_pred[n] = exp(int_hazard);
    }
  }
}
