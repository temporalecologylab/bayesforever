## Answering Questions from Chapter 5 & 6 in Rethinking
# Questions assigned: 5H2-3 and 6M5-6

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
dev.off()

## Load libraries
library(ggplot2)
library(rethinking)

# Load the data
data(foxes)
foxes <- foxes

# Start with 5H1 since it leads into 5H2 and 5H3
### Linear model with territory size
mod.terr <- map(
  alist(
    weight ~ dnorm(mu , sigma),
    mu <- intercept + b_terr*area,
    intercept ~ dnorm(0,10),
    b_terr ~ dnorm(0,10),
    sigma ~ dunif(0,5)
  ),
  data=foxes
)
precis(mod.terr)

terr.seq <- seq(1,6,0.1)
mu.modterr <- link(mod.terr, data=list(area=terr.seq))
mu.modterr.mean <- apply(mu.modterr, 2, mean)
mu.modterr.PI <- apply(mu.modterr, 2, PI, .89)

plot(weight~area, foxes, col='red')
lines(terr.seq, mu.modterr.mean, col='black')
shade(mu.modterr.PI, terr.seq)

plot(precis(mod.terr))
abline(v=0, col="gray", lty=3, lwd=3)

### SO AREA IS NOT VERY IMPORTANT IN PREDICTING FOX WEIGHT!

# Let's check out group size now, in the exact same way as above...
mod.group <- map(
  alist(
    weight ~ dnorm(mu , sigma),
    mu <- intercept + b_group*groupsize,
    intercept ~ dnorm(0,10),
    b_group ~ dnorm(0,10),
    sigma ~ dunif(0,5)
  ),
  data=foxes
)
precis(mod.group)

group.seq <- seq(1,9,0.1)
mu.modgroup <- link(mod.group, data=list(groupsize=group.seq))
mu.modgroup.mean <- apply(mu.modgroup, 2, mean)
mu.modgroup.PI <- apply(mu.modgroup, 2, PI, .89)

plot(weight~groupsize, foxes, col='red')
lines(group.seq, mu.modgroup.mean, col='black')
shade(mu.modgroup.PI, group.seq)

plot(precis(mod.group))
abline(v=0, col="gray", lty=3, lwd=3)

### AND NEITHER IS GROUP SIZE BUT SLIGHTLY MORE THAN TERRITORY SIZE... INTERESTING!

par(mfrow=c(1,2))
plot(precis(mod.terr))
abline(v=0, col="gray", lty=3, lwd=3)

plot(precis(mod.group))
abline(v=0, col="gray", lty=3, lwd=3)

dev.off()


########################## 5H2 ################################
# Alright, now for the good stuff! 
mod.both <- map(alist(
  weight ~ dnorm(mu , sigma),
  mu <- intercept + b_group*groupsize + b_terr*area,
  intercept ~ dnorm(0,10),
  b_group ~ dnorm(0,10),
  b_terr ~ dnorm(0,10),
  sigma ~ dunif(0,5)
),
data=foxes
)
precis(mod.both)

## Okay, now we'll plot territory size while keeping group size constant
mod.both.terr <- link(mod.both, data=data.frame(groupsize=mean(foxes$groupsize), area=terr.seq))
mod.both.terr.mean <- apply(mod.both.terr, 2, mean)
mod.both.terr.PI <- apply(mod.both.terr, 2, PI, .89)

plot(weight~area, foxes, col='red')
lines(terr.seq, mod.both.terr.mean, col='blue')
shade(mod.both.terr.PI, terr.seq, col = col.alpha("blue",0.5))

# Now we'll plot groupsize while keeping territory size constant
mod.both.group <- link(mod.both, data=data.frame(area=mean(foxes$area), groupsize=group.seq))
mod.both.group.mean <- apply(mod.both.group, 2, mean)
mod.both.group.PI <- apply(mod.both.group, 2, PI, .89)

plot(weight~groupsize, foxes, col='red')
lines(group.seq, mod.both.group.mean, col='blue')
shade(mod.both.group.PI, group.seq, col = col.alpha("blue",0.5))

par(mfrow=c(1,3))
plot(precis(mod.both))
abline(v=0, col="gray", lty=3, lwd=3)

plot(weight~area, foxes, col='red')
lines(terr.seq, mod.both.terr.mean, col='blue')
shade(mod.both.terr.PI, terr.seq, col = col.alpha("blue",0.5))

plot(weight~groupsize, foxes, col='red')
lines(group.seq, mod.both.group.mean, col='blue')
shade(mod.both.group.PI, group.seq, col = col.alpha("blue",0.5))

dev.off()


########################## 5H3 ################################
# So we start with body size as an additive function of avgfood and groupsize, per Richard's request
mod.foodgroup <- map(alist(
  weight ~ dnorm(mu , sigma),
  mu <- intercept + b_group*groupsize + b_food*avgfood,
  intercept ~ dnorm(0,10),
  b_group ~ dnorm(0,10),
  b_food ~ dnorm(0,10),
  sigma ~ dunif(0,5)
),
data=foxes
)
precis(mod.foodgroup)


## And now all three!
mod.all <- map(alist(
  weight ~ dnorm(mu , sigma),
  mu <- intercept + b_group*groupsize + b_terr*area + b_food*avgfood,
  intercept ~ dnorm(0,10),
  b_group ~ dnorm(0,10),
  b_terr ~ dnorm(0,10),
  b_food ~ dnorm(0,10),
  sigma ~ dunif(0,5)
),
data=foxes
)
precis(mod.all)

## What's a better predictor of weight? avgfood or area?
mod.all.group <- link(mod.all, data=data.frame(area=mean(foxes$area), avgfood=mean(foxes$avgfood), groupsize=group.seq))
mod.all.group.mean <- apply(mod.all.group, 2, mean)
mod.all.group.PI <- apply(mod.all.group, 2, PI, .89)

food.seq <- seq(0,1.5,0.1)
mod.all.food <- link(mod.all, data=data.frame(area=mean(foxes$area), groupsize=mean(foxes$groupsize), avgfood=food.seq))
mod.all.food.mean <- apply(mod.all.food, 2, mean)
mod.all.food.PI <- apply(mod.all.food, 2, PI, .89)

mod.all.area <- link(mod.all, data=data.frame(avgfood=mean(foxes$avgfood), groupsize=mean(foxes$groupsize), area=terr.seq))
mod.all.area.mean <- apply(mod.all.area, 2, mean)
mod.all.area.PI <- apply(mod.all.area, 2, PI, .89)

par(mfrow=c(2,3))

plot.new()

plot(precis(mod.all))
abline(v=0, col="gray", lty=2, lwd=3)

plot.new()

plot(weight~groupsize, foxes, col='red')
lines(group.seq, mod.all.group.mean, col='blue')
shade(mod.all.group.PI, group.seq, col = col.alpha("blue",0.5))

plot(weight~avgfood, foxes, col='red')
lines(food.seq, mod.all.food.mean, col='blue')
shade(mod.all.food.PI, food.seq, col = col.alpha("blue",0.5))

plot(weight~area, foxes, col='red')
lines(terr.seq, mod.all.area.mean, col='blue')
shade(mod.all.area.PI, terr.seq, col = col.alpha("blue",0.5))

dev.off()




#### okay so let's just do a quick collinearity check for fun...
# For question 2, we look at fox weight as a function of groupsize and territory size. We'll start here.

plot(groupsize ~ area, data=foxes)

## and then we look at average food variable
par(mfrow=c(1, 3))
plot(groupsize ~ area, data=foxes)
plot(groupsize ~ avgfood, data=foxes)
plot(avgfood ~ area, data=foxes)
dev.off()

## And it all makes sense.