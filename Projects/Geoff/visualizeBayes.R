
## Load libraries
library(VGAM) # for skewnormal
library(rstan)
library(shinystan)
library(viridis) # for colors

## Set seed
set.seed(20201124)

## Temperature function that returns temperature at time t
## Assumes temperature follows a sine curve 
temperature <- function(t, temp_phase, temp_per, temp_amp, temp_baseline){
    result <- temp_amp * sin(2 * pi * (t - temp_phase) / temp_per) + temp_baseline
}

## Hyper-parameters of the temperature model priors
param <- list(temp_phase_mu = 113,
              temp_phase_sigma = 2,
              temp_per_mu = 350,
              temp_per_sigma = 5,
              temp_amp_mu = 10.4,
              temp_amp_sigma = 0.5,
              temp_baseline_mu = 10.15,
              temp_baseline_sigma = 0.1,
              temp_sigma_mu = 2,
              temp_sigma_sigma = 0.2)

## Simulate 1000 samples from the priors (each sample is for an entire year)
sims <- 1000
### Generate parameter values
param.samples <- list(temp_phase = rnorm(n = sims,
                                            mean = param[["temp_phase_mu"]],
                                            sd = param[["temp_phase_sigma"]]),
                      temp_per = rnorm(n = sims,
                                       mean = param[["temp_per_mu"]],
                                       sd = param[["temp_per_sigma"]]),
                      temp_amp = rnorm(n = sims,
                                       mean = param[["temp_amp_mu"]],
                                       sd = param[["temp_amp_sigma"]]),
                      temp_baseline = rnorm(n = sims,
                                            mean = param[["temp_baseline_mu"]],
                                            sd = param[["temp_baseline_sigma"]]),
                      temp_sigma = rnorm(n = sims,
                                         mean = param[["temp_sigma_mu"]],
                                         sd = param[["temp_sigma_sigma"]]))
### Storage
temperature.store <- matrix(NA, nrow = sims, ncol = 365)
observation.store <- matrix(NA, nrow = sims, ncol = 365)
### Loop
for(i in 1:sims){
    ## Generate true temperatures
    temperature.store[i, ] <- temperature(1:365,
                                          temp_phase = param.samples[["temp_phase"]][i],
                                          temp_per = param.samples[["temp_per"]][i],
                                          temp_amp = param.samples[["temp_amp"]][i],
                                          temp_baseline = param.samples[["temp_baseline"]][i])
    ## Generate observations of true temperature
    observation.store[i, ] <- rnorm(n = ncol(temperature.store),
                                    mean = temperature.store[i, ],
                                    sd = param.samples[["temp_sigma"]][i])
}

## Function for obtaining the 95% highest posterior density (i.e. 95% credible interval)
## Returns lower, upper, and mean
credint <- function ( samp, prob = 0.95 ) {
    vals <- sort(samp)
    nsamp <- length(vals)
    gap <- max(1, min(nsamp - 1, round(nsamp * prob)))
    init <- 1:(nsamp - gap)
    inds <- which.min(vals[init + gap,drop=FALSE] - vals[init, drop=FALSE])
    ans <- cbind(lower=vals[inds], upper=vals[inds + gap], mean = mean(samp))
    return(ans)
}

## Calculate credible interval of the true temperature
creds.temperature <- sapply(1:365, FUN = function(X) credint(temperature.store[, X]))
## Calculate credible interval of the true temperature
creds.observation <- sapply(1:365, FUN = function(X) credint(observation.store[, X]))

## Plot
pdf(file = "Visual_Temperature.pdf", width = 7, height = 11)
par(mfrow = c(2, 1))
## Plot (credible intervals)
plot(creds.temperature[3, ] ~ c(1:365), ylim = c(-5, 25), xlab = "Day of Year", ylab = "Temperature", main = "Temperature model", type = "l", lwd = 2)
### Observation interval
lower.smooth.observation <- ksmooth(x = 1:365, y = creds.observation[1, ], bandwidth = 4)
upper.smooth.observation <- ksmooth(x = 1:365, y = creds.observation[2, ], bandwidth = 4)
polygon(x = c(lower.smooth.observation$x, rev(upper.smooth.observation$x)),
        y = c(lower.smooth.observation$y, rev(upper.smooth.observation$y)),
        col = rgb(0, 0.6, 0, alpha = .3), border = "white")
### True temperature interval
lower.smooth.temperature <- ksmooth(x = 1:365, y = creds.temperature[1, ], bandwidth = 4)
upper.smooth.temperature <- ksmooth(x = 1:365, y = creds.temperature[2, ], bandwidth = 4)
polygon(x = c(lower.smooth.temperature$x, rev(upper.smooth.temperature$x)),
        y = c(lower.smooth.temperature$y, rev(upper.smooth.temperature$y)),
        col = rgb(0, 0, 1, alpha = .3), border = "white")
legend("topleft", legend = c("True temperature (95% CI)", "Observed temperature (95% CI)"), pt.bg = c(rgb(0, 0, 1, alpha = .3), rgb(0, 0.6, 0, alpha = .3)), pch = 22, inset = 0.05, cex = 0.75)
## Plot (samples)
plot(temperature.store[322, ] ~ c(1:365), ylim = c(-5, 25), xlab = "Day of Year", ylab = "Temperature", main = "", type = "l", lwd =2, col = rgb(0, 0, 1, alpha = .8))
points(observation.store[322, ] ~ c(1:365), col = rgb(0, 0.6, 0, alpha = .8))
legend("topleft", legend = c("True temperature (sample)", "Observed temperature (sample)"), col = c(rgb(0, 0, 1, alpha = .8), rgb(0, 0.6, 0, alpha = .8)), lty = c(1, NA), pch = c(NA, 1), inset = 0.05, cex = .75)
## Close pdf
dev.off()

########### Survival model

## Function for sampling from waiting times of a non-homogeneous Poisson process
## Uses the hazard function above
## Involves the inverse transform method (see Legault and Melbourne 2019)
nhpp <- function(initialt = 1, param, inten, maxtime = 500){
    tryCatch(uniroot(function(X, Y) {
        1 - exp(-integrate(Vectorize(function(X){
            do.call(inten, args = append(list(T = initialt + X), param))}), 0, X)$value) - Y},
                     lower = 0, upper = maxtime, tol = 1e-5, Y = runif(1))$root,
        error = function(c) maxtime + 1)
}
## Hazard function for instantaneous probability of ripening
hazard.intensity <- function(T, temp_phase, temp_per, temp_amp, temp_baseline, skew_mu, skew_sigma, skew_shape, gamma, ...){
    temp.x <- temperature(T, temp_phase, temp_per, temp_amp, temp_baseline)
    result <- gamma * dskewnorm(x = temp.x, location = skew_mu, scale = skew_sigma, shape = skew_shape)
    return(result)
}
### CDF function for the survival/ripening process
surv.cdf <-  function(X, param){
    1 - exp(-integrate(Vectorize(function(y){ do.call(temper.inten, args = append(list(T = y), param))}), 0, X)$value)
}

## Append hyper-parameters for survival model to parameter list
param <- append(param, list(skew_mu_mu = 25,
                            skew_mu_sigma = 0.5,
                            skew_sigma_mu = 4.2,
                            skew_sigma_sigma = .1,
                            skew_shape_mu = -3,
                            skew_shape_sigma = .5,
                            gamma_mu = 15.2,
                            gamma_sigma = .5))

### Append samples of survival model parameters to parameter sample list
param.samples <- append(param.samples, list(skew_mu = rnorm(n = sims,
                                                            mean = param[["skew_mu_mu"]],
                                                            sd = param[["skew_mu_sigma"]]),
                                            skew_sigma = rnorm(n = sims,
                                                            mean = param[["skew_sigma_mu"]],
                                                            sd = param[["skew_sigma_sigma"]]),
                                            skew_shape = rnorm(n = sims,
                                                               mean = param[["skew_shape_mu"]],
                                                            sd = param[["skew_shape_sigma"]]),
                                            gamma = rnorm(n = sims,
                                                          mean = param[["gamma_mu"]],
                                                          sd = param[["gamma_sigma"]])))

## Generate 1000 samples of the temperature response
### What temperatures?
temp.range <- seq(10, 35, by = .1)
### Storage
tempresponse.store <- matrix(NA, nrow = sims, ncol = length(temp.range))
### Loops
for(i in 1:sims){
    tempresponse.store[i, ] <- dskewnorm(x = temp.range,
                                         location = param.samples[["skew_mu"]][i],
                                         scale = param.samples[["skew_sigma"]][i],
                                         shape = param.samples[["skew_shape"]][i])
}
### Calculate credible interval of temperature response
creds.tempresponse <- sapply(1:length(temp.range), FUN = function(X) credint(tempresponse.store[, X]))

## Plot
pdf(file = "Visual_TemperatureResponse.pdf", width = 7, height = 7)
## Plot (credible intervals)
plot(creds.tempresponse[3, ] ~ temp.range, ylim = c(0, .2), xlab = "Temperature", ylab = "Hazard rate", main = "Temperature response model", type = "l", lwd = 2)
### Observation interval
lower.smooth <- ksmooth(x = temp.range, y = creds.tempresponse[1, ], bandwidth = 4)
upper.smooth <- ksmooth(x = temp.range, y = creds.tempresponse[2, ], bandwidth = 4)
polygon(x = c(lower.smooth$x, rev(upper.smooth$x)),
        y = c(lower.smooth$y, rev(upper.smooth$y)),
        col = rgb(0, 0.6, 0, alpha = .3), border = "white")
legend("topleft", legend = c("Temperature response (mean)", "Temperature response (95% CI)"), col = c("black"), pt.bg = c(NA, rgb(0, 0.6, 0, alpha = .8)), lty = c(1, NA), pch = c(NA, 22), inset = 0.05, cex = .75)
dev.off()

######

## Simulate ripening
nsims <- 150
eventday <- dim(nsims)
for(i in 1:nsims){
    eventday[i] <- nhpp(param = lapply(param.samples, "[", i), inten = hazard.intensity)
}
### Check simulations
eventday
which(eventday > 500) # No errors
### Round down values and assume daily censusing (creating left and right censoring)
eventday <- floor(eventday)
eventday <- eventday[order(eventday)]
ver.ag <- aggregate(eventday, by = list(eventday), FUN = length)
#### Add year (1 for now)
ver.ag$Group.2 <- 1
