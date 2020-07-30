/* First pass at a joint model to best estimate species-level lat values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
	real mindat[N]; // y min lat data 
  //real maxdat[N]; // y max lat data 
  
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
	
  real a_mins_sp[nsp]; // lower 10% of min latitudes per species
  //real a_maxs_sp[nsp]; // upper 10% of max latitudes per species
	
  // Model of pheno
  real mua_sp[nsppheno]; // mean of the alpha value for species
  real <lower = 0> sigma_sp; // variation of intercept amoung species
  
  real a_photo[nsppheno]; // mean of the alpha value for species for photo
  
  real a_photomin[nsppheno];
  real mu_bphotomin[nsppheno];
  real <lower = 0> sigma_bphotomin;
  //vector[nsppheno] mu_bphotomax;
  
	real <lower=0> sigma_yphoto; // overall variation accross observations for photo

  real agrand[nsppheno]; // grand mean for trait
	real mub_grand[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigmab_grand; // variation of intercept amoung species for pheno
	
}

model{ 
  real ypredphoto[Npheno];
  //real b_photo[nsppheno];
	
	real latmins[N] = a_mins_sp[species]; 
	//real latmaxs[N] = a_maxs_sp[species]; 
	
	//for(i in 1:nsppheno){
    //b_photo[i] = agrand[i] + a_photomin[i] * latmins[i]; //+ mu_bphotomax[i] * latmaxs[i];
	//}
	
	for(i in 1:Npheno){
	  ypredphoto[i] = agrand[speciespheno[i]] +  (a_photo[speciespheno[i]] * photoperiod[i]) + 
	                  (a_photomin[speciespheno[i]] * latmins[speciespheno[i]]) ;
	}
  
  a_photo ~ normal(mua_sp, sigma_sp); // needs partial pooling - should this (mua_sp, sigma_sp)
  agrand ~ normal(mub_grand, sigmab_grand); //should this be partially pooled? regular prior skip sigmab_grand
  
  a_photomin ~ normal(mu_bphotomin, sigma_bphotomin); //should this be partially pooled?
  //mu_bphotomax ~ normal(1, 10);

  mua_sp ~ normal(-2, 10);
  sigma_sp ~ normal(0, 5);
  
  sigma_y ~ normal(0, 10);
  sigma_yphoto ~ normal(0, 30);
  
  mub_grand ~ normal(0, 10);
  sigmab_grand ~ normal(0, 5); //removed
  
  mu_bphotomin ~ normal(0, 10);
  sigma_bphotomin ~ normal(0, 5); //removed

	// likelihoods 
	mindat ~ normal(latmins, sigma_y);
  //maxdat ~ normal(latmaxs, sigma_y);
  photodat ~ normal(ypredphoto, sigma_yphoto);
  
}

generated quantities {
   //real b_photo[nsppheno];
   real y_ppmin[N];
   //real y_ppmax[N];
   real y_ppphoto[Npheno];
   
   y_ppmin = a_mins_sp[species];
   //y_ppmax = a_maxs_sp[species];
   
   //for(i in 1:nsppheno){
     // b_photo[i] = mub_grand[i] + mu_bphotomin[i] * y_ppmin[i]; //+ mu_bphotomax[i] * y_ppmax[i];
	 //}
   
   for(i in 1:Npheno){
      y_ppphoto[i] = a_photo[speciespheno[i]] + 
                    (mu_bphotomin[speciespheno[i]] * y_ppmin[speciespheno[i]]) * photoperiod[i];
   }
   
   y_ppmin = normal_rng(y_ppmin, sigma_y);
   //y_ppmax = normal_rng(y_ppmax, sigma_y);
   y_ppphoto = normal_rng(y_ppphoto, sigma_yphoto);

   
} 
