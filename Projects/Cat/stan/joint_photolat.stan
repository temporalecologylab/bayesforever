/* First pass at a joint model to best estimate species-level lat values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
	real mindat[N]; // y min lat data 
  real maxdat[N]; // y max lat data 
  
  // Model of pheno
  int < lower = 1 > Npheno; // Sample size for pheno data 
  vector[Npheno] photoperiod; // predictor photoperiod
 	vector[Npheno] photodat; // y photo data 
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower=0> sigma_y; // overall variation accross observations for lat
	
	//real agrand; // grand mean for trait
	
  real a_mins_sp[nsp]; // lower 10% of min latitudes per species
  real a_maxs_sp[nsp]; // upper 10% of max latitudes per species
	
  // Model of pheno
  real mua_sp[nsp]; // mean of the alpha value for species
  real <lower = 0> sigma_sp; // variation of intercept amoung species
  
  real a_photo[nsppheno]; // mean of the alpha value for species for photo
  
  vector[nsppheno] mu_bphotomin;
  vector[nsppheno] mu_bphotomax;
  
	real <lower=0> sigma_yphoto; // overall variation accross observations for photo

	real mu_aphotomin[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_aphotomin; // variation of intercept amoung species for pheno
	
	real mu_aphotomax[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_aphotomax; // variation of intercept amoung species for pheno
}

transformed parameters{
  real b_photomin[nsppheno]; 
  real b_photomax[nsppheno];
  
  for(i in 1:nsppheno){
    
    b_photomin[i] =  mu_aphotomin[i] + mu_bphotomin[i] * a_mins_sp[i]; // ideally I would want latmins here?
    b_photomax[i] =  mu_aphotomax[i] + mu_bphotomax[i] * a_maxs_sp[i];
	  
	}
	  
}

model{ 
  real ypredphoto[Npheno];
	//real ypred[N];
	
	real latmins[N] = a_mins_sp[species]; 
	real latmaxs[N] = a_maxs_sp[species]; 
	
	mindat ~ normal(latmins, sigma_y);
  maxdat ~ normal(latmaxs, sigma_y);
	
	for(i in 1:Npheno){
	  
	ypredphoto[i] = a_photo[speciespheno[i]] + b_photomin[speciespheno[i]]*photoperiod[i] +  
	                b_photomax[speciespheno[i]]*photoperiod[i];
	
	}
  
  a_photo ~ normal(0, sigma_yphoto);
  
  mu_bphotomin ~ normal(0, sigma_aphotomin);
  mu_bphotomax ~ normal(0, sigma_aphotomax);

  mua_sp ~ normal(0, sigma_sp);
  sigma_sp ~ normal(0, 2);
  
  sigma_y ~ normal(0, 3);
  sigma_yphoto ~ normal(0, 2);
  
  sigma_aphotomin ~ normal(0, 2);
  sigma_aphotomax ~ normal(0, 2);

	// likelihoods 
  photodat ~ normal(ypredphoto, sigma_yphoto);
  
}

generated quantities {
   real y_ppmin[N];
   real y_ppmax[N];
   real y_ppphoto[Npheno];
   real y_ppforce[Npheno];
   real y_ppchill[Npheno];
   //real y_pp[N]
   
   y_ppmin = a_mins_sp[species];
   y_ppmax = a_maxs_sp[species];
   
   y_ppmin = normal_rng(y_ppmin, sigma_y);
   y_ppmax = normal_rng(y_ppmax, sigma_y);
   
   for(i in 1:Npheno){
     
   y_ppphoto[i] =  a_photo[speciespheno[i]] + b_photomin[speciespheno[i]]*photoperiod[i] +  
	                b_photomax[speciespheno[i]]*photoperiod[i];
   
   }
   
   y_ppphoto = normal_rng(y_ppphoto, sigma_yphoto);

   
} 
