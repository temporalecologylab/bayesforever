############################################################
# Initial Setup
############################################################

library(rstan)
rstan_options(auto_write = TRUE)            # Cache compiled Stan programs
options(mc.cores = parallel::detectCores()) # Parallelize chains
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

util <- new.env()
source('stan_utility.R', local=util)

c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

############################################################
# Tune Prior
############################################################

stan(file='gamma_tune.stan', algorithm="Fixed_param", iter=1, warmup=0, refresh=1)

############################################################
# Fit Model
############################################################

input_data <- read_rdump("wine.data.R")

filtered_data <- input_data
filtered_data$N <- 10
filtered_data$obs_tran_time <- input_data$obs_tran_time[1:filtered_data$N]

simu_inits <- function(chain_id) {
  return(list("temp_amp" = rnorm(1, 10.5, 0.5), "temp_per" = rnorm(1, 350, 5), 
              "temp_phase" = rnorm(1, 115, 3), "temp_baseline" = rnorm(1, 10, 0.5),
              "sigma_temp" = rnorm(1, 3, 1),
              "T_min" = rnorm(1, 4, 0.75), "T_max" = rnorm(1, 40, 1.5), "T_opt" = rnorm(1, 26, 2),
              "gamma" = rgamma(1, 4.63, rate=22.07)))
}

fit <- stan(file='fit_pheno_survival_sine.stan', data=filtered_data, init=simu_inits, seed=19389932, refresh=1)

# Check diagnostics
util$check_all_diagnostics(fit)

sapply(1:4, function(c) get_sampler_params(fit, inc_warmup=FALSE)[[c]][,'stepsize__'][1])

table(do.call(rbind, get_sampler_params(fit, inc_warmup=FALSE))[,'n_leapfrog__'])

# Make some plots
samples = extract(fit)

# Temperature interpolation parameters
par(mfrow=c(3, 2))

hist(samples$temp_amp, breaks=seq(5, 15, 0.1), main="",
     col=c_dark, border=c_dark_highlight,
     xlim=c(5, 15), xlab="Temp Amplitude", yaxt='n', ylab="")

hist(samples$temp_per, breaks=seq(300, 400, 1), main="",
     col=c_dark, border=c_dark_highlight,
     xlim=c(300, 400), xlab="Temp Period", yaxt='n', ylab="")

hist(samples$temp_phase, breaks=seq(100, 130, 0.25), main="",
     col=c_dark, border=c_dark_highlight,
     xlim=c(100, 130), xlab="Temp Phase", yaxt='n', ylab="")

hist(samples$temp_baseline, breaks=seq(5, 15, 0.1), main="",
     col=c_dark, border=c_dark_highlight,
     xlim=c(5, 15), xlab="Temp Baseline", yaxt='n', ylab="")

hist(samples$temp_sigma, breaks=seq(0, 5, 0.01), main="",
     col=c_dark, border=c_dark_highlight,
     xlim=c(0, 5), xlab="Recorded Temp Variability", yaxt='n', ylab="")

# Temperature interpolation
par(mfrow=c(1, 1))

probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cred <- sapply(1:length(input_data$temp_time),
               function(n) quantile(samples$latent_temp[,n], probs=probs))

plot(1, type="n", main="Inferred Temperature",
     xlab="Day", ylab="Temperature (C)", xlim=c(1, 365), ylim=c(-10, 30))
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[1,], rev(cred[9,])),
        col = c_light, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[2,], rev(cred[8,])),
        col = c_light_highlight, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[3,], rev(cred[7,])),
        col = c_mid, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[4,], rev(cred[6,])),
        col = c_mid_highlight, border = NA)
lines(input_data$temp_time, cred[5,], col=c_dark, lwd=2)


points(input_data$temp_time, input_data$temp_rec, col="white", pch=16, cex=1.2)
points(input_data$temp_time, input_data$temp_rec, col="black", pch=16, cex=0.8)

# Temperature Residuals
plot(1, type="n", main="Inferred Temperature",
     xlab="Day", ylab="Temperature (C)", xlim=c(1, 365), ylim=c(-10, 10))
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[1,] - cred[5,], rev(cred[9,] - cred[5,])),
        col = c_light, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[2,] - cred[5,], rev(cred[8,] - cred[5,])),
        col = c_light_highlight, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[3,] - cred[5,], rev(cred[7,] - cred[5,])),
        col = c_mid, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[4,] - cred[5,], rev(cred[6,] - cred[5,])),
        col = c_mid_highlight, border = NA)
lines(input_data$temp_time, 0, col=c_dark, lwd=2)


lines(input_data$temp_time, input_data$temp_rec - cred[5,], col="white", pch=16, cex=1.2)
lines(input_data$temp_time, input_data$temp_rec - cred[5,], col="black", pch=16, cex=0.8)

# Temperature Retrodictions
par(mfrow=c(1, 1))

probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cred <- sapply(1:length(input_data$temp_time),
               function(n) quantile(samples$temp_rec_pred[,n], probs=probs))

plot(1, type="n", main="Retrodicted Temperature Recordings",
     xlab="Day", ylab="Recorded Temperature (C)", xlim=c(1, 365), ylim=c(-10, 30))
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[1,], rev(cred[9,])),
        col = c_light, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[2,], rev(cred[8,])),
        col = c_light_highlight, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[3,], rev(cred[7,])),
        col = c_mid, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[4,], rev(cred[6,])),
        col = c_mid_highlight, border = NA)
lines(input_data$temp_time, cred[5,], col=c_dark, lwd=2)

points(input_data$temp_time, input_data$temp_rec, col="white", pch=16, cex=1.2)
points(input_data$temp_time, input_data$temp_rec, col="black", pch=16, cex=0.8)

# Wang-Engels parameters
par(mfrow=c(2, 2))

hist(rnorm(10000, 4, 0.75), breaks=seq(-11, 11, 0.5), main="",
     col=c_light, border=c_light_highlight,
     xlim=c(-10, 10), xlab="T_min(C)", yaxt='n', ylab="", prob=T)
hist(samples$T_min, breaks=seq(-11, 11, 0.5), main="",
     col=c_dark, border=c_dark_highlight, add=T, prob=T)

hist(rnorm(10000, 40, 1.5), breaks=seq(25, 55, 0.5), main="",
     col=c_light, border=c_light_highlight,
     xlim=c(25, 55), xlab="T_max(C)", yaxt='n', ylab="", prob=T)
hist(samples$T_max, breaks=seq(25, 55, 0.5), 
     col=c_dark, border=c_dark_highlight, add=T, prob=T)

hist(rnorm(10000, 26, 2), breaks=seq(10, 50, 0.5), main="",
     col=c_light, border=c_light_highlight,
     xlim=c(10, 50), xlab="T_opt(C)", yaxt='n', ylab="", prob=T)
hist(samples$T_opt, breaks=seq(10, 50, 0.5),
     col=c_dark, border=c_dark_highlight, add=T, prob=T)

hist(abs(rgamma(10000, 4.63, rate=22.07)), breaks=seq(0, 1, 0.01), main="",
     col=c_light, border=c_light_highlight,
     xlim=c(0, 1), xlab="gamma", yaxt='n', ylab="", prob=T)
hist(samples$gamma, breaks=seq(0, 1, 0.01),
     col=c_dark, border=c_dark_highlight, add=T, prob=T)

# Wang-Engels aggregation
par(mfrow=c(1, 1))

probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cred <- sapply(1:length(input_data$temp_time),
               function(n) quantile(samples$integrated_we_pred[, n], probs=probs))

plot(1, type="n", main="Day",
     xlab="Day", ylab="Inferred Wang-Engels Aggregation (WE Units)", xlim=c(1, 365), ylim=c(0, 175))
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[1,], rev(cred[9,])),
        col = c_light, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[2,], rev(cred[8,])),
        col = c_light_highlight, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[3,], rev(cred[7,])),
        col = c_mid, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[4,], rev(cred[6,])),
        col = c_mid_highlight, border = NA)
lines(input_data$temp_time, cred[5,], col=c_dark, lwd=2)

for (n in 1:filtered_data$N) {
  abline(v=filtered_data$obs_tran_time[n], col="white", lty=1, lwd=2)
  abline(v=filtered_data$obs_tran_time[n], col="black", lty=1, lwd=1)
}

# Survival Probability
par(mfrow=c(1, 1))

probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cred <- sapply(1:length(input_data$temp_time),
               function(n) quantile(samples$survival_pred[, n], probs=probs))

plot(1, type="n", main="",
     xlab="Day", ylab="Probability of Not Transitioning", xlim=c(1, 365), ylim=c(0, 1))
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[1,], rev(cred[9,])),
        col = c_light, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[2,], rev(cred[8,])),
        col = c_light_highlight, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[3,], rev(cred[7,])),
        col = c_mid, border = NA)
polygon(c(input_data$temp_time, rev(input_data$temp_time)), c(cred[4,], rev(cred[6,])),
        col = c_mid_highlight, border = NA)
lines(input_data$temp_time, cred[5,], col=c_dark, lwd=2)

for (n in 1:filtered_data$N) {
  abline(v=filtered_data$obs_tran_time[n], col="white", lty=1, lwd=2)
  abline(v=filtered_data$obs_tran_time[n], col="black", lty=1, lwd=1)
}