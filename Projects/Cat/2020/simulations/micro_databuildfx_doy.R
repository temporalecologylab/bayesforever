### Started 25 May 2020 by Cat
## Source file to build fake data simulations for urban versus provenance lat effects
## TAKE II: randomizing day of budburst and then calculate GDD from there

library(dplyr)
library(tidyr)

set.seed(12321)

if(use.urban==TRUE){
  
  #### Here, I set up provenance for each individual
  spind <- paste(rep(c(1:nspps), each=10), rep(1:ninds, 20), sep="_")
  provenance.hf <- 42.5
  provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
  
  df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                 site = rep(c("arb", "hf"), each=nobs*nmethods),
                                 provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, 400)),
                                 method = rep(c("ws", "hobo"), nsites*nobs)))
  df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
  
  
  #### Next I set up an fstar or a GDD threshold for each individual
  spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")
  provenance.hf <- 42.5
  provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
  
  df.prov <- as.data.frame(cbind(sp_ind = rep(spind, nsites), 
                                 site = rep(c("arb", "hf"), each=nobs),
                                 provenance = c(provenance.arb, rep(provenance.hf, 200))))
  
  fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                  fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                  site=rep(c("arb", "hf"), each=ninds*nmethods),
                                  method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  
  df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
  
  if(hypothA==TRUE){
    
  df.fstar$urbtx <- ifelse(df.fstar$site=="arb", 1, 0)
  df.fstar$gdd.noise  <- df.fstar$fstarspp + df.fstar$urbtx * rep(rnorm(n=nspps, mean=urbeffect, sd=urbsd), each=ninds*nmethods)  
    
  
  df.fstar$tx <- ifelse(df.fstar$method=="hobo", 1, 0)
  df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$tx * rep(rnorm(n=nspps, mean=0, sd=methodsd), each=ninds*nmethods)  
  
 
  }
  
  if(hypothB==TRUE){
    
 
    df.fstar$tx <- ifelse(df.fstar$method=="hobo", 1, 0)
    df.fstar$gdd.noise <- df.fstar$fstarspp + df.fstar$tx * rep(rnorm(nspps, methodeffect, methodsd), each=ninds*nmethods)  
    
    
  }
  
  df.fstar$fstar.new <- rnorm(df.fstar$inds, df.fstar$gdd.noise, fstarindsd)
  
  
  df.fstar <- full_join(df.fstar, df.prov)
  
  
}

if(use.provenance==TRUE){
  
  provenance.arb <- round(rnorm(nobs, 42.5, 10), digits=2)
  provenance.hf <- 42.5
  
  doybb <- round(rnorm(nspps, doybb, doybbspeciessd), digits=0)
  df.doybb <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites), inds=1:ninds, doybb=rep(doybb, each=ninds*nsites),
                                  site=rep(c("arb", "hf"), each=ninds)))
  df.doybb$doybb <- as.numeric(df.doybb$doybb)
  df.doybb$sp_ind <- paste(df.doybb$species, df.doybb$inds, sep="_")
  
  df.doybb$provenance <- ifelse(df.doybb$site=="hf", provenance.hf, provenance.arb)
  df.doybb$prov.adj <- ifelse(df.doybb$provenance!=provenance.hf, df.doybb$provenance-provenance.hf, 0)
  
  df.doybb$doybb.new <- round(ifelse(df.doybb$site=="hf", rnorm(df.doybb$inds, df.doybb$doybb, doybbindsd), 
                                     rnorm(df.doybb$inds, df.doybb$doybb+(df.doybb$prov.adj*proveffect), doybbindsd)), digits=0)
  
  df.doybb$dayz <- round(df.doybb$doybb.new, digits=0)
  
}


# Step 2: find GDDs
#### Now I set up climate data for the Arboretum
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                      #dayz=rep(arb.doybb$dayz, each=daysperyr),
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                      site = as.character("arb"))


### This is how I get weather station data versus hobo logger data
arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(arbclim$day, cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect), rnorm(arbclim$day, cc.arb, sigma.arb)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite


#### Harvard Forest climate data
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods), 
                     #dayz=rep(hf.doybb$dayz, each=daysperyr),
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))


### Again, where I set up the difference between hobo logger and weather station
hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(hfclim$day, cc.hf + microhf.effect, sigma.hf + microsigmahf.effect), rnorm(hfclim$day, cc.hf, sigma.hf)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite


# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- dplyr::full_join(arbclim, hfclim)
df$tmean <- as.numeric(df$tmean)

df$sp_ind <- paste(df$species, df$ind, sep="_")

### Calculate the OBSERVED GDDs!!!
df$gdd.obs <- ave(df$tmean, df$sp_ind, df$site, df$method, FUN=cumsum)


### Let's just tidy everything up
df$species <- as.character(df$species)
df <- left_join(df, df.fstar)

df$spind_site_method <- paste0(df$sp_ind, df$site, df$method)

## Find the day of budburst to find the actual GDD versus the "observed GDD"
for(i in c(unique(df$spind_site_method))){ 
  
  bb <- which(df$gdd.obs[i==df$spind_site_method] >= df$fstar.new[i==df$spind_site_method])[1]
  df$bb[i==df$spind_site_method] <- bb
  
}

df.bb <- df[(df$bb==df$day),]

if(hypothB==TRUE){
  
  #df.bb$tx <- ifelse(df.bb$method=="ws", 1, 0)
  #df.bb$gdd.noise <- df.bb$gdd.obs + df.bb$tx * rep(rnorm(nspps, methodeffect, methodsd), each=ninds*nmethods)  ### This "methodsd" isn't being captured
  
  df.bb$urbtx <- ifelse(df.bb$site=="arb", 1, 0)
  df.bb$gdd  <- df.bb$gdd.obs + df.bb$urbtx * rep(rnorm(nspps, urbeffect, urbsd), each=ninds*nmethods)  ### This "urbsd" isn't being captured
  
  
}

if(hypothA==TRUE){
  
  df.bb$gdd <- df.bb$gdd.obs
  
}
df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "bb", "gdd", "gdd.noise", "fstar.new", "provenance"))
df.bb$species <- as.numeric(df.bb$species)

bball <- df.bb[!duplicated(df.bb),]


