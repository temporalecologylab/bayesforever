### Started 6 April 2020 by Cat
## Now work on changing observation frequency using simulation data
# Following Lizzie's code from ospree/bb_analysis/pep_sims/pepvarsimfxs.R and pepvarsim.R

# Maybe I should use estimates for fstar from real models?

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Questions to address:
# GDDlo ~ urban + (urban|species) 

## Let's start with Question 1 first...
#library(rethinking)
library(RColorBrewer)
library(lme4)

## Let's load some real data to check out.
setwd("~/Documents/git/microclimates/analyses/")

ws <- read.csv("output/clean_gdd_chill_bbanddvr.csv")
mean(ws$gdd_bb, na.rm=TRUE) ## 292
sd(ws$gdd_bb, na.rm = TRUE) ## 116

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 70 ## needs to be divisible by sampling frequencies (1, 3, 7, 10)
nspps <- 30
ninds <- 10
nobs <- nspps*ninds
#yrstotal <- 3
#yrs <- rep(1:yrstotal, each=(daysperyr*nobs))
dayz <- rep(1:daysperyr, (nobs))
cc <- 12
sigma <- 3
fstar <- round(rnorm(nspps, 300, 75), digits=0)
df.fstar <- as.data.frame(cbind(species=1:nspps, fstar, bb=rep("Y", nspps)))
df.fstar$fstar <- as.numeric(df.fstar$fstar)
samplefreq <- 10 #### Change based on desired sampling frequency

gdd <- c()
#gddall <- c()
# Step 2: find GDDs
for (i in 1:nobs){
  dailytemp <- rnorm(daysperyr, cc, sigma)
  gdd <- c(gdd, cumsum(dailytemp))
}

#gddall <- c(gddall, rep(gdd, ninds))

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- data.frame(cbind(doy=dayz, tmean=round(dailytemp, digits=2), 
                       gdd=round(gdd, digits=2), species=as.character(rep(1:nspps, each=daysperyr)),
                       ind=as.character(rep(1:ninds, each=daysperyr*nspps))))
df$gdd <- as.numeric(df$gdd)

# Step 4: Now, in a very slow, painful way I get the BB date
df$bb.YN <- NA

for (i in c(1:nrow(df))){ # This loop just makes a Yes/No vector for budburst
  for (j in c(1:nrow(df.fstar))) 
    if (df$species[i]==df.fstar$species[j] && df$gdd[i]<df.fstar$fstar[j]){
      df$bb.YN[i] <- "N"
    } else if (df$species[i]==df.fstar$species[j] && df$gdd[i]>df.fstar$fstar[j]) {
      df$bb.YN[i] <- "Y"
    } else {
      df$bb.YN[i] <- df$bb.YN[i]
    }
}



# Step 5: Now we remove rows based on sampling frequency and then calculate the observed BB date
df.sample <- df[seq(1, nrow(df), samplefreq),]

df.sample.sub <- df.sample[(df.sample$bb.YN=="Y"),]
df.sample.sub$bb <- ave(df.sample.sub$doy, df.sample.sub$species, df.sample.sub$ind, FUN=min)

df.sample.sub <- subset(df.sample.sub, select=c("species", "gdd", "bb", "ind"))

draws.tendays <- df.sample.sub[!duplicated(df.sample.sub[c("bb", "ind", "species")]),] ### change name based on sampling frequency


write.csv(draws.oneday, file="simulations/output/everydayobs_multspp.csv", row.names=FALSE)
write.csv(draws.threedays, file="simulations/output/threedayobs_multspp.csv", row.names=FALSE)
write.csv(draws.sevendays, file="simulations/output/sevendayobs_multspp.csv", row.names=FALSE)
write.csv(draws.tendays, file="simulations/output/tendayobs_multspp.csv", row.names=FALSE)



