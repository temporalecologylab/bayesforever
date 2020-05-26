### Started 23 April 2020 by Cat
## Source file to build fake data simulations for urban versus provenance lat effects

library(dplyr)
library(tidyr)

set.seed(12321)

if(use.urban==TRUE){
  
  spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")
  provenance.hf <- 42.5
  provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
  
  df.prov <- as.data.frame(cbind(sp_ind = rep(spind, nsites), 
                           site = rep(c("arb", "hf"), each=nobs),
                           provenance = c(provenance.arb, rep(provenance.hf, 200))))
  
  fstar <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                  fstar=rep(fstar, each=ninds*nsites*nmethods),
                                site=rep(c("arb", "hf"), each=ninds*nmethods),
                                method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  
  df.fstar$fstar <- as.numeric(df.fstar$fstar)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
  
  df.fstar <- full_join(df.fstar, df.prov)
  
  urbeffectall <- rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="hobo"], urbeffect, urbsd)
  methodeffectall <- rnorm(df.fstar$inds[df.fstar$method=="ws" & df.fstar$site=="hf"], methodeffect, methodsd)
  urbandmethod <- rnorm(df.fstar$inds[df.fstar$method=="ws" & df.fstar$site=="arb"], methodeffect + urbeffect, methodsd + urbsd)
  
  for(i in c(1:nrow(df.fstar))){
    if(df.fstar$site[i]=="hf" && df.fstar$method[i]=="hobo") {
    
      df.fstar$fstar.new[df.fstar$site=="hf" & df.fstar$method=="hobo"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="hf" & df.fstar$method=="hobo"], 
              df.fstar$fstar[df.fstar$site=="hf" & df.fstar$method=="hobo"], fstarindsd)
    
    } else if (df.fstar$site[i]=="arb" && df.fstar$method[i]=="hobo") {
    
      df.fstar$fstar.new[df.fstar$site=="arb" & df.fstar$method=="hobo"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="hobo"], 
              df.fstar$fstar[df.fstar$site=="arb" & df.fstar$method=="hobo"] + urbeffectall, fstarindsd)
    
    } else if (df.fstar$site[i]=="hf" && df.fstar$method[i]=="ws") {
    
      df.fstar$fstar.new[df.fstar$site=="hf" & df.fstar$method=="ws"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="hf" & df.fstar$method=="ws"], 
              df.fstar$fstar[df.fstar$site=="hf" & df.fstar$method=="ws"] + methodeffectall, fstarindsd)
    
    } else if (df.fstar$site[i]=="arb" && df.fstar$method[i]=="ws") {
    
      df.fstar$fstar.new[df.fstar$site=="arb" & df.fstar$method=="ws"] <- 
          rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="ws"], 
              df.fstar$fstar[df.fstar$site=="arb" & df.fstar$method=="ws"] + urbandmethod, fstarindsd)
    
    }
  }
  
}

if(use.provenance==TRUE){ ##### NEED TO REVAMP!!!
  
  spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")
  provenance.hf <- 42.5
  provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
  
  df.prov <- as.data.frame(cbind(sp_ind = rep(spind, nsites), 
                                 site = rep(c("arb", "hf"), each=nobs),
                                 provenance = c(provenance.arb, rep(provenance.hf, 200))))
  
  fstar <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), inds=rep(1:ninds, nmethods), 
                                  fstar=rep(fstar, each=ninds*nsites*nmethods),
                                  site=rep(c("arb", "hf"), each=ninds*nmethods),
                                  method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  
  df.fstar$fstar <- as.numeric(df.fstar$fstar)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
  
  df.fstar <- full_join(df.fstar, df.prov)
  
  df.fstar$provenance <- as.numeric(df.fstar$provenance)
  #lowlat <- min(df.fstar$provenance)
  df.fstar$prov.adj <- ifelse(df.fstar$site!=provenance.hf, df.fstar$provenance-provenance.hf, 0)
  
  proveffectall <- rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="hobo"], 
                         (proveffect*df.fstar$prov.adj[df.fstar$site=="arb" & df.fstar$method=="hobo"]), provsd)
  methodeffectall <- rnorm(df.fstar$inds[df.fstar$method=="ws" & df.fstar$site=="hf"], methodeffect, methodsd)
  provandmethod <- rnorm(df.fstar$inds[df.fstar$method=="ws" & df.fstar$site=="arb"], methodeffect + 
                           (proveffect*df.fstar$prov.adj[df.fstar$method=="ws" & df.fstar$site=="arb"]), methodsd + provsd)
  
  for(i in c(1:nrow(df.fstar))){
    if(df.fstar$site[i]=="hf" && df.fstar$method[i]=="hobo") {
      
      df.fstar$fstar.new[df.fstar$site=="hf" & df.fstar$method=="hobo"] <- 
        rnorm(df.fstar$inds[df.fstar$site=="hf" & df.fstar$method=="hobo"], 
              df.fstar$fstar[df.fstar$site=="hf" & df.fstar$method=="hobo"], fstarindsd)
      
    } else if (df.fstar$site[i]=="arb" && df.fstar$method[i]=="hobo") {
      
      df.fstar$fstar.new[df.fstar$site=="arb" & df.fstar$method=="hobo"] <- 
        rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="hobo"], 
              df.fstar$fstar[df.fstar$site=="arb" & df.fstar$method=="hobo"] + proveffectall, fstarindsd)
      
    } else if (df.fstar$site[i]=="hf" && df.fstar$method[i]=="ws") {
      
      df.fstar$fstar.new[df.fstar$site=="hf" & df.fstar$method=="ws"] <- 
        rnorm(df.fstar$inds[df.fstar$site=="hf" & df.fstar$method=="ws"], 
              df.fstar$fstar[df.fstar$site=="hf" & df.fstar$method=="ws"] + methodeffect, fstarindsd)
      
    } else if (df.fstar$site[i]=="arb" && df.fstar$method[i]=="ws") {
      
      df.fstar$fstar.new[df.fstar$site=="arb" & df.fstar$method=="ws"] <- 
        rnorm(df.fstar$inds[df.fstar$site=="arb" & df.fstar$method=="ws"], 
              df.fstar$fstar[df.fstar$site=="arb" & df.fstar$method=="ws"] + provandmethod, fstarindsd)
      
    }
  }
  
}


# Step 2: find GDDs

  ## 2a) Arboretum climate data
arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                      species = as.character(rep(c(1:nspps), each=daysperyr*nmicros*nmethods)), 
                      day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                      method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                      site = as.character("arb"))


arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(arbclim$day, cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect), rnorm(arbclim$day, cc.arb, sigma.arb)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite


  ## 2b) Harvard Forest climate data
hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods),nspps), ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                     species = as.character(rep(c(1:nspps), each=daysperyr*nmicros*nmethods)), 
                     day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                     method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                     site = as.character("hf"))


hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(hfclim$day, cc.hf + microhf.effect, sigma.hf + microsigmahf.effect), rnorm(hfclim$day, cc.hf, sigma.hf)) ### and now we draw from mean and sigma for each day to find daily temp for each microsite

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- full_join(arbclim, hfclim)
df$tmean <- as.numeric(df$tmean)

df$microsite <- paste0(df$site, df$ind)

df$sp_ind <- paste(df$species, df$ind, sep="_")

df$gdd <- ave(df$tmean, df$sp_ind, df$site, df$method, FUN=cumsum)

df.fstar.sub <- subset(df.fstar, select=c("site", "method", "provenance", "sp_ind", "fstar.new"))
df <- full_join(df.fstar.sub, df)

#### Now we find budburst day..
df$group <- paste(df$site, df$method, df$sp_ind)
grouplist <- 1:length(unique(df$group))
grouplist <- data.frame(grouplist, group=unique(df$group))
df <- left_join(df, grouplist)

df$doybb <- NA
for(i in 1:length(unique(df$grouplist))){ #i=1
  df$doybb[df$grouplist==i] <- which(df$gdd[df$grouplist==i] >= df$fstar.new[df$grouplist==i])[1]
}

bball <- df[(df$day==df$doybb),]

bball <- subset(bball, select=c("site", "microsite", "method", "ind", "species", "provenance", "day", "gdd"))
colnames(bball) <- c("site", "microsite", "method", "ind", "species", "provenance", "bb", "gdd")

