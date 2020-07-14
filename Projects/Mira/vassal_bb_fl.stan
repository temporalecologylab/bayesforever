//
      // stan linear regression model for duration of vassal interphenophases

      data{
        int<lower=0> N;       //all the samples
        vector[N] x;          // year predictor
        vector[N] y;          // predicted duration of interphenophase
        int<lower=1> Nv;      // number of varieties
        int<lower=1, upper=Nv> variety[N]; // what does this do?
      }
      
      parameters{
        vector[Nv] a_var;               // mu, one per var
        vector[Nv] b_var;               // mu, one per var
        real mu_a;
        real mu_b;
        real<lower=0> s_avar;     // for alpha, one per var
        real<lower=0> s_bvar;     // for beta, one per var
        real<lower=0> sigma_y;    // big sig
    
      }
      
      model{
        // likelihood
        for (n in 1:N)
        y[n] ~ normal(a_var[variety[n]] + b_var[variety[n]] * x[n], sigma_y);
        
        //priors
        a_var ~ normal(mu_a,s_avar);
        b_var ~ normal(mu_b,s_bvar);
        mu_a ~ normal(65,5);
        mu_b ~ normal(0,1);
        s_avar ~ normal(0,10);
        s_bvar ~ normal(0,0.2);
        sigma_y ~ normal(0,10);
      }
