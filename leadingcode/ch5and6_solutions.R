## Answering Questions from Chapter 5 & 6 in Rethinking
# Questions assigned: 5H2-3 and 6M5-6

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

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

### AND NEITHER IS GROUP SIZE... INTERESTING!

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


# 5H3
# quick check
d <- mutate(d, 
            avgfood.s = normalise(avgfood),
            area.s = normalise(area),
            groupsize.s=normalise(groupsize))

m.fd.gs <- lm(weight ~ avgfood.s + groupsize.s, d)
summary(m.fd.gs)
precis(m.fd.gs, digits=3)

m.fd.gs.a <- lm(weight ~ avgfood.s + groupsize.s + area.s, d)
summary(m.fd.gs.a)
precis(m.fd.gs.a, digits=3)

m.gs.a <- lm(weight ~  groupsize.s + area.s, d)
summary(m.gs.a)
precis(m.gs.a, digits=3)


# group size from area and food
m.f.a <- lm(groupsize ~ avgfood.s + area.s, d)
summary(m.f.a)
precis(m.f.a, digits=3)

gd <- mutate(gd, 
             avgfood.s = normalise(avgfood),
             area.s = normalise(area),
             groupsize.s=normalise(groupsize))
m.f.a2 <- lm(groupsize ~ avgfood.s + area.s, gd)
summary(m.f.a2)
precis(m.f.a2, digits=3)