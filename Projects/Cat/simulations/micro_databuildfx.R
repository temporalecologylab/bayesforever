### Started 23 April 2020 by Cat
## Source file to build fake data simulations for urban versus provenance lat effects

# Maybe I should use estimates for fstar from real models?

set.seed(12321)

if(use.urban==TRUE){
  
  provenance.arb <- round(rnorm(nobs, 42.5, 8), digits=2)
  provenance.hf <- 42.5
  
  fstar <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites), inds=1:ninds, fstar=rep(fstar, each=ninds*nsites),
                                site=rep(c("arb", "hf"), each=ninds)))
  df.fstar$fstar <- as.numeric(df.fstar$fstar)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")

  df.fstar$fstar.new <- round(ifelse(df.fstar$site=="hf", rnorm(df.fstar$inds, df.fstar$fstar, fstarindsd), 
                                   rnorm(df.fstar$inds, df.fstar$fstar+urbeffect, fstarindsd)), digits=0)

}

if(use.provenance==TRUE){
  
  provenance.arb <- round(rnorm(nobs, 42.5, 10), digits=2)
  provenance.hf <- 42.5
  
  fstar <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites), inds=1:ninds, fstar=rep(fstar, each=ninds*nsites),
                                  site=rep(c("arb", "hf"), each=ninds)))
  df.fstar$fstar <- as.numeric(df.fstar$fstar)
  df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$inds, sep="_")
  
  df.fstar$provenance <- ifelse(df.fstar$site=="hf", provenance.hf, provenance.arb)
  df.fstar$prov.adj <- ifelse(df.fstar$provenance!=provenance.hf, df.fstar$provenance-provenance.hf, 0)
  
  df.fstar$fstar.new <- round(ifelse(df.fstar$site=="hf", rnorm(df.fstar$inds, df.fstar$fstar, fstarindsd), 
                                     rnorm(df.fstar$inds, df.fstar$fstar+(df.fstar$prov.adj*proveffect), fstarindsd)), digits=0)
  
  
}


# Step 2: find GDDs
arbmicromeans <- rnorm(nmicros, cc.arb, mean.microarb)
arbmicrosigmas <- rnorm(nmicros, sigma.arb, sigma.microarb)
arbclim <- data.frame(site=rep(c(1:nmicros), each=daysperyr), means=rep(arbmicromeans, each=daysperyr), 
                      sigmas=rep(arbmicrosigmas, each=daysperyr), day=rep(c(1:daysperyr), nmicros))

arbclim$tmean <- rnorm(arbclim$day, arbclim$means, arbclim$sigmas)


hfmicromeans <- rnorm(nmicros, cc.hf, mean.microhf)
hfmicrosigmas <- rnorm(nmicros, sigma.hf, sigma.microhf)
hfclim <- data.frame(site=rep(c(1:nmicros), each=daysperyr), means=rep(hfmicromeans, each=daysperyr), 
                     sigmas=rep(hfmicrosigmas, each=daysperyr), day=rep(c(1:daysperyr), nmicros))

hfclim$tmean <- rnorm(hfclim$day, hfclim$means, hfclim$sigmas)

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df.arb <- data.frame(cbind(doy=dayz, tmean=round(arbclim$tmean, digits=2), 
                           species=as.character(rep(1:nspps, each=daysperyr)),
                           ind=as.character(rep(1:ninds, each=daysperyr*nspps)),
                           site="arb",
                           provenance = rep(provenance.arb, each=daysperyr)))

df.hf <- data.frame(cbind(doy=dayz, tmean=round(hfclim$tmean, digits=2), 
                          species=as.character(rep(1:nspps, each=daysperyr)),
                          ind=as.character(rep(1:ninds, each=daysperyr*nspps)),
                          site="hf",
                          provenance = provenance.hf))

df <- full_join(df.arb, df.hf)
df$tmean <- as.numeric(df$tmean)

df$microsite <- paste0(df$site, df$ind)
df$tmean.ws <- ave(df$tmean, df$doy, df$site)


df$sp_ind <- paste(df$species, df$ind, sep="_")

df$gdd.ws <- ave(df$tmean.ws, df$sp_ind, df$site, FUN=cumsum)
df$gdd.hl <- ave(df$tmean, df$sp_ind, df$microsite, FUN=cumsum)

df.fstar.sub <- subset(df.fstar, select=c("site", "sp_ind", "fstar.new"))
df <- full_join(df.fstar.sub, df)


#### Now we find budburst day...
# Step 4: Now, in a very slow, painful way I get the BB date
df$bb.ws <- ifelse(df$gdd.ws>=df$fstar.new, "Y", "N")
df$bb.hl <- ifelse(df$gdd.hl>=df$fstar.new, "Y", "N")

df$doy <- as.numeric(df$doy)

bbws <- df[(df$bb.ws=="Y"),]
bbws <- subset(bbws, select=c("species", "ind", "sp_ind", "site", "microsite", "doy", "gdd.ws", "bb.ws", "provenance"))
bbws$bbws.doy <- ave(bbws$doy, bbws$sp_ind, bbws$site, FUN=min)
bbws$bbws.gdd <- ave(bbws$gdd.ws, bbws$sp_ind, bbws$site, FUN=min)
bbws$doy <- NULL
bbws$gdd.ws <- NULL
bbws$bb.ws <- NULL
bbws <- bbws[!duplicated(bbws),]

bbhl <- df[(df$bb.hl=="Y"),]
bbhl <- subset(bbhl, select=c("species", "ind", "sp_ind", "site", "microsite", "doy", "gdd.hl", "bb.hl", "provenance"))
bbhl$bbhl.doy <- ave(bbhl$doy, bbhl$sp_ind, bbhl$microsite, FUN=min)
bbhl$bbhl.gdd <- ave(bbhl$gdd.hl, bbhl$sp_ind, bbhl$microsite, FUN=min)
bbhl$doy <- NULL
bbhl$gdd.hl <- NULL
bbhl$bb.hl <- NULL
bbhl <- bbhl[!duplicated(bbhl),]

bball <- full_join(bbws, bbhl)
