//
      // stan linear regression model for gdd of vassal interphenophases

      data{
        int<lower=0> N;       //all the samples
        //vector[N] x;        // year predictor
        vector[N] y;          // predicted GDD of interphenophase
        int<lower=1> Nv;      // number of varieties
        int<lower=1, upper=Nv> variety[N]; // what does this do
      }
      
      parameters{
        vector[Nv] a_var;         
        //vector[Nv] b_var;         
        real mu_a;                // mu, one per var 
        //real mu_b;              // mu, one per var
        real<lower=0> s_avar;     // for alpha, one per var
        //real<lower=0> s_bvar;   // for beta, one per var
        real<lower=0> sigma_y;    // big sig
    
      }
      
      model{
        // likelihood
        for (n in 1:N)
        y[n] ~ normal(a_var[variety[n]], sigma_y);
        
        //priors
        a_var ~ normal(mu_a,s_avar);
        //b_var ~ normal(mu_b,s_bvar);
        mu_a ~ normal(700,20);
        //mu_b ~ normal(0,1);
        s_avar ~ normal(50,5);
        //s_bvar ~ normal(0,0.2);
        sigma_y ~ normal(0,10);
      }
