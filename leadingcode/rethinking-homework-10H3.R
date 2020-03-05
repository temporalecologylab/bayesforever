## Load library
library(MASS)
library(rethinking)

## Load data
data(eagles)
dat <- eagles

## Convert letters to numbers
dat$P <- ifelse(dat$P == "S", 0, 1)
dat$A <- ifelse(dat$A == "I", 0, 1)
dat$V <- ifelse(dat$V == "S", 0, 1)
## Create proportion column
dat$prop <- dat$y / dat$n
## Inspect
head(dat)

## Model 1
m.eagles <- map2stan(
    alist(
        y ~ dbinom(n, p),
        logit(p)  <-  a + bp * P + bv * V + ba * A,
        a ~ dnorm(0, 10),
        bp ~ dnorm(0, 5),
        bv ~ dnorm(0, 5),
        ba ~ dnorm(0, 5)
    ),
    data = dat, iter = 4000, chains = 4)
## Obtain estimates
precis(m.eagles)

## Plot model predictions
dat.pred <- expand.grid(P = c(0, 1), A = c(0, 1), V = c(0, 1))
link.m.eagles <- link(m.eagles, data = dat.pred)
pred.p <- apply(link.m.eagles, 2, mean)
pred.p.HPDI <- apply(link.m.eagles, 2, HPDI, prob = .89)
par(mfrow = c(1, 2)) # will be adding another plot later
plot(0, 0, type = "n", xlab = "Pirate/Adult/Victim", ylab = "Success probability",
     ylim = c(0, 1), xlim = c(1, 8), xaxt = "n", main = "m.eagles")
axis(1, at = 1:8, labels = c("0/0/0", "1/0/0", "0/1/0", "1/1/0", "0/0/1", "1/0/1", "0/1/1", "1/1/1"))
p <- by(dat$prop,
        list(dat$P, dat$A, dat$V), mean)
for(i in 1:nrow(dat)){
    points(p[i] ~ c(i - .05), pch = 16, col = "black", cex = 1.2)
    points(pred.p[i] ~ c(i + .05), pch = 16, col = "blue", cex = 1.2)
    lines(x = rep(i + .05, 2), y = pred.p.HPDI[, i], col = rgb(0, 0, 1, alpha = .5), lwd = 2)
}
legend("topright", c("Observed", "Predicted"), col = c("blue", "black"), pch = 16)

## Model 2
m2.eagles <- map2stan(
    alist(
        y ~ dbinom(n, p),
        logit(p)  <-  a + bp * P + bv * V + ba * A + bpa * (A * P),
        a ~ dnorm(0, 10),
        bp ~ dnorm(0, 5),
        bv ~ dnorm(0, 5),
        ba ~ dnorm(0, 5),
        bpa ~ dnorm(0, 5)
    ),
    data = dat, iter = 4000, chains = 4)
## Obtain estimates
precis(m2.eagles)

## Compare to previous model
compare(m.eagles, m2.eagles)

## Plot model predictions
dat.pred <- expand.grid(P = c(0, 1), A = c(0, 1), V = c(0, 1))
link.m2.eagles <- link(m2.eagles, data = dat.pred)
pred.p <- apply(link.m2.eagles, 2, mean)
pred.p.HPDI <- apply(link.m2.eagles, 2, HPDI, prob = .89)
plot(0, 0, type = "n", xlab = "Pirate/Adult/Victim", ylab = "Success probability",
     ylim = c(0, 1), xlim = c(1, 8), xaxt = "n", main = "m2.eagles")
axis(1, at = 1:8, labels = c("0/0/0", "1/0/0", "0/1/0", "1/1/0", "0/0/1", "1/0/1", "0/1/1", "1/1/1"))
p <- by(dat$prop,
        list(dat$P, dat$A, dat$V), mean)
for(i in 1:nrow(dat)){
    points(p[i] ~ c(i - .05), pch = 16, col = "black", cex = 1.2)
    points(pred.p[i] ~ c(i + .05), pch = 16, col = "blue", cex = 1.2)
    lines(x = rep(i + .05, 2), y = pred.p.HPDI[, i], col = rgb(0, 0, 1, alpha = .5), lwd = 2)
}
