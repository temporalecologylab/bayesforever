### 4H1
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# fit model
m4.3 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 156 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )

#Rcode 4.54
# define sequence of weights to compute predictions for 4.54
# these values will be on the horizontal axis
#weight.seq <- seq( from=25 , to=70 , by=1 ) #original
weight.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

#Rcode 4.56
# summarize dist of mu
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

# Rcode 4.59
# simulate height by sampling gaussina dist for u of each weight (sd from posterior dist)
sim.height <- sim( m4.3 , data=list(weight=weight.seq) )
str(sim.height)

# R code 4.60
# posterior predicton inverval of observed values across predictor
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# ANSWERS
# expected height
mu.mean
# 89% interval (HPDI)
mu.HPDI
# 89% interval (PI)
height.PI


# FOR PLOTTING
# Rcode  4.61
# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , weight.seq )
# draw PI region for simulated heights
shade( height.PI , weight.seq )

#### 4H2 ####

# Load library
library(rethinking)

# Source data
data(Howell1)
d <- Howell1
# Subset to ages below 18
d2 <- d[d$age < 18, ]
nrow(d2) # should be 192

## PRIOR PREDICTIVE CHECK (OPTIONAL)
## sample_a <- rnorm(n = 10000, mean = mean(d2$height), sd = 100) # assume intercept is centered on mean height
## sample_b <- rnorm(n = 10000, mean = 0, sd = 10) # slope centered on 0
## sample_sigma <- runif(n = 10000, min = 0, max = 50)
## sample_weight <- sample(x = seq(4, 48, by = 1), size = 10000, replace = TRUE) # generate some random weights
## sample_mu <- sample_a + (sample_b * sample_weight) # generate mu using linear model and weights
## sample_height <- rnorm(n = 10000, mean = sample_mu, sd = sample_sigma)
## plot(density(sample_height))
## abline(v = mean(d2$height), lwd = 2, col = "blue")

# Create linear model
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu  <- a + b * weight,
  a ~ dnorm(108, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
)
# Use quadratic approximation
mod.fit <- map(flist, data = d2)
# What are the estimates?
precis(mod.fit)

# Plot raw data with estimated with regression line
par(mfrow = c(1, 2)) # We'll use the 2nd column later
plot(height ~ weight, data = d2)
abline(a = coef(mod.fit)["a"], b = coef(mod.fit)["b"])

# Calculate average height across range of weights
weight.seq <- seq(4, 48, by = 1) # range
mu <- link(mod.fit, data = data.frame(weight = weight.seq)) # estimated average heights
# Calculate HPDI for mu
mu.HPDI <- apply(mu, 2, HPDI, prob = .89) # 89% HPDI for average height
# Add to plot
shade(mu.HPDI, weight.seq)
# Calculate PI interval
sim.height <- sim(mod.fit, data = list(weight = weight.seq))
height.PI <- apply(sim.height, 2, PI, prob = .89)
# Add to plot
shade(height.PI, weight.seq)

# Square root transform weight
d2$weight.tr <- sqrt(d2$weight)
# Square root model
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu  <- a + b * weight.tr,
  a ~ dnorm(108, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
)
mod.fit <- map(flist, data = d2)
precis(mod.fit)
# Plot raw data with estimated with MAP regression line
plot(height ~ weight.tr, data = d2)
abline(a = coef(mod.fit)["a"], b = coef(mod.fit)["b"])
# Calculate average height across range of weights
weight.seq <- sqrt(seq(4, 48, by = 1))
mu <- link(mod.fit, data = data.frame(weight.tr = weight.seq))
# Calculate HPDI for mu
mu.HPDI <- apply(mu, 2, HPDI, prob = .89)
# Add to plot
shade(mu.HPDI, weight.seq)
# Calculate PI interval
sim.height <- sim(mod.fit, data = list(weight.tr = weight.seq))
height.PI <- apply(sim.height, 2, PI, prob = .89)
# Add to plot
shade(height.PI, weight.seq)

