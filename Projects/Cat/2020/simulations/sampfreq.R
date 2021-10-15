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

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 100
yrstotal <- 100
yrs <- rep(1:yrstotal, each=daysperyr)
dayz <- rep(1:daysperyr, yrstotal)
cc <- 12
sigma <- 3
fstar <- 300
samplefreq <- 14 #### Change based on desired sampling frequency

gdd <- c()
# Step 2: find GDDs
for (i in 1:yrstotal){
  dailytemp <- rnorm(daysperyr, cc, sigma)
  gdd <- c(gdd, cumsum(dailytemp))
}

# Step 3: Make a data frame and get the mean temp per year (to double check the data)
df <- data.frame(cbind(yrs, dayz, dailytemp, gdd))

# Step 4: Now, in a very slow, painful way I get the BB date
df$bb.YN <- NA

for (i in c(1:nrow(df))){ # This loop just makes a Yes/No vector for budburst
  if (df$gdd[i]<fstar) {
    df$bb.YN[i] <- "N"
  } else {
    df$bb.YN[i] <- "Y"
  }
}

# Step 5: Now we remove rows based on sampling frequency and then calculate the observed BB date
df.sample <- df[seq(1, nrow(df), samplefreq),]

df.sample.sub <- df.sample[(df.sample$bb.YN=="Y"),]
df.sample.sub$bb <- ave(df.sample.sub$dayz, df.sample.sub$yrs, FUN=min)

df.sample.sub <- subset(df.sample.sub, select=c("yrs", "gdd", "bb"))

draws.fourteendays <- df.sample.sub[!duplicated(df.sample.sub[c("bb", "yrs")]),] ### change name based on sampling frequency


write.csv(draws.oneday, file="simulations/output/everydayobs.csv", row.names=FALSE)
write.csv(draws.threedays, file="simulations/output/threedayobs.csv", row.names=FALSE)
write.csv(draws.sevendays, file="simulations/output/sevendayobs.csv", row.names=FALSE)
write.csv(draws.tendays, file="simulations/output/tendayobs.csv", row.names=FALSE)
write.csv(draws.fourteendays, file="simulations/output/fourteendayobs.csv", row.names=FALSE)



