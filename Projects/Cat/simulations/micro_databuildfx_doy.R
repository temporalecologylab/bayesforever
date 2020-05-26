### Started 25 May 2020 by Cat
## Source file to build fake data simulations for urban versus provenance lat effects
## TAKE II: randomizing day of budburst and then calculate GDD from there

library(dplyr)
library(tidyr)

set.seed(12321)

if(use.urban==TRUE){
  
  spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")
  provenance.arb <- round(rnorm(nobs, 42.5, 5), digits=2)
  provenance.hf <- 42.5
  
  df.prov <- as.data.frame(cbind(sp_ind = rep(spind, nsites), 
                                 site = rep(c("arb", "hf"), each=nobs),
                                 provenance = c(provenance.arb, rep(provenance.hf, 200))))
  
  doybb <- round(rnorm(nspps, doybb, doybbspeciessd), digits=0)
  df.doybb <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                  doybb=rep(doybb, each=ninds*nsites*nmethods),
                                  site=rep(c("arb", "hf"), each=ninds*nmethods),
                                  method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  
  df.doybb$doybb <- as.numeric(df.doybb$doybb)
  df.doybb$sp_ind <- paste(df.doybb$species, df.doybb$inds, sep="_")
  
  df.doybb <- full_join(df.doybb, df.prov)
  
  urbeffectall <- rnorm(df.doybb$inds[df.doybb$site=="arb" & df.doybb$method=="hobo"], urbeffect, urbsd)
  methodeffectall <- rnorm(df.doybb$inds[df.doybb$method=="ws" & df.doybb$site=="hf"], methodeffect, methodsd)
  urbandmethod <- rnorm(df.doybb$inds[df.doybb$method=="ws" & df.doybb$site=="arb"], methodeffect + urbeffect, methodsd + urbsd)
  
  for(i in c(1:nrow(df.doybb))){
    if(df.doybb$site[i]=="hf" && df.doybb$method[i]=="hobo") {
      
      df.doybb$doybb.new[df.doybb$site=="hf" & df.doybb$method=="hobo"] <- 
        rnorm(df.doybb$inds[df.doybb$site=="hf" & df.doybb$method=="hobo"], 
              df.doybb$doybb[df.doybb$site=="hf" & df.doybb$method=="hobo"], doybbindsd)
      
    } else if (df.doybb$site[i]=="arb" && df.doybb$method[i]=="hobo") {
      
      df.doybb$doybb.new[df.doybb$site=="arb" & df.doybb$method=="hobo"] <- 
        rnorm(df.doybb$inds[df.doybb$site=="arb" & df.doybb$method=="hobo"], 
              df.doybb$doybb[df.doybb$site=="arb" & df.doybb$method=="hobo"] + urbeffectall, doybbindsd)
      
    } else if (df.doybb$site[i]=="hf" && df.doybb$method[i]=="ws") {
      
      df.doybb$doybb.new[df.doybb$site=="hf" & df.doybb$method=="ws"] <- 
        rnorm(df.doybb$inds[df.doybb$site=="hf" & df.doybb$method=="ws"], 
              df.doybb$doybb[df.doybb$site=="hf" & df.doybb$method=="ws"] + methodeffectall, doybbindsd)
      
    } else if (df.doybb$site[i]=="arb" && df.doybb$method[i]=="ws") {
      
      df.doybb$doybb.new[df.doybb$site=="arb" & df.doybb$method=="ws"] <- 
        rnorm(df.doybb$inds[df.doybb$site=="arb" & df.doybb$method=="ws"], 
              df.doybb$doybb[df.doybb$site=="arb" & df.doybb$method=="ws"] + urbandmethod, doybbindsd)
      
    }
  }
  
  df.doybb$dayz <- round(df.doybb$doybb.new, digits=0)
  
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
arb.doybb <- df.doybb[(df.doybb$site=="arb"),]
hf.doybb <- df.doybb[(df.doybb$site=="hf"),]

arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = as.character(rep(c(1:nspps), each=daysperyr*nmicros*nmethods)), 
                      dayz=rep(arb.doybb$dayz, each=daysperyr),
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                      site = as.character("arb"))


arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(arbclim$day, cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect), rnorm(arbclim$day, cc.arb, sigma.arb)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite

arbclim <- arbclim[(arbclim$day<=arbclim$dayz),]


hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = as.character(rep(c(1:nspps), each=daysperyr*nmicros*nmethods)), 
                     dayz=rep(hf.doybb$dayz, each=daysperyr),
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))


hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(hfclim$day, cc.hf + microhf.effect, sigma.hf + microsigmahf.effect), rnorm(hfclim$day, cc.hf, sigma.hf)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite
hfclim <- hfclim[(hfclim$day<=hfclim$dayz),]

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- full_join(arbclim, hfclim)
df$tmean <- as.numeric(df$tmean)

df$microsite <- paste0(df$site, df$ind)

df$sp_ind <- paste(df$species, df$ind, sep="_")

df$gdd <- ave(df$tmean, df$sp_ind, df$site, df$method, FUN=cumsum)

df.doybb.sub <- subset(df.doybb, select=c("site", "method", "provenance", "sp_ind", "doybb.new"))
df <- full_join(df.doybb.sub, df)

#### Now we simplify the dataset..
bball <- df[(df$day==df$dayz),]

bball <- subset(bball, select=c("site", "method", "microsite", "ind", "species", "provenance", "day", "gdd"))
colnames(bball) <- c("site", "method", "microsite", "ind", "species", "provenance", "bb", "gdd")

bball <- bball[!duplicated(bball),]



