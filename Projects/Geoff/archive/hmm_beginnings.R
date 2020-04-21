## Script for simulating and fitting a Hidden Markov Model
##
## Basic model:
##
## We model a system with hidden (latent) states and observed states
## 
## We assume two possible hidden states, 1 or 2
##
## The observed state is a normal distribution with two possible mean values, mu1 and mu2, determined
## by the hidden state, such that if the system is in state 1, the mean value is mu1, etc.
##
## Further, the system changes state according to a transition matrix, theta
## [ theta1, theta2
##   theta3, theta4 ]
## such that
## theta1 = probability of staying in state 1
## theta2 = probability of switching to state 2
## theta3 = probability of switching to state 1
## theta4 = probability of staying in state 2
##
## We are interested in estimating mu1, mu2, and theta1-theta4

## First, let's simulate this model
## Number of simulations
sims <- 100

## Generate mu's
mu <- c(3, 10)

## Thetas
probs <- c(.8, .2, .7, .3)

## How many observations per sample?
N <- 100
## Create matrix containing hidden states
hidden <- matrix(NA, nrow = sims, ncol = N)
## Create matrix containing observed states
obs <- matrix(NA, nrow = sims, ncol = N)
## Initial state is 1
hidden[, 1] <- 1

## Simulate
for(i in 1:sims){
    transition.matrix <- matrix(data = probs, nrow = 2, ncol = 2, byrow = TRUE)
    obs[i, 1] <- rnorm(n = 1, mean = mu[hidden[i, 1]], sd = 1)
    for(j in 2:N){
        hidden[i, j] <- sample(x = c(1, 2), size = 1, replace = FALSE,
                               prob = transition.matrix[hidden[i, j - 1], ])
        obs[i, j] <- rnorm(n = 1, mean = mu[hidden[i, j]], sd = 1)       
    }
}

## Focus on 1 simulation
par(mfrow = c(2, 1))
plot(x = 1:N, y = hidden[1, ], xlab = "Time", ylab = "State", type = "l", main = "Hidden State", yaxt = "n")
axis(side = 2, at = seq(1, 2, 1), las = 1)
plot(x = 1:N, y = obs[1, ], xlab = "Time", ylab = "Value", type = "p", pch = 16, main = "Observation")
points(x = 1:N, y = obs[1, ], type = "l")

## Now fit a model using stan
#### Model code is not mine!
library(rstan)

stan_data <- list(N = length(obs[1, ]),
                  K = 2,
                  y = obs[1, ])
hmm_fit <- stan("model.stan", data = stan_data, iter = 2000, chains = 4)
## Check the estimates
print(hmm_fit, pars = "z_star", include = FALSE, probs = c(0.05,0.95))


## Can we do a posterior predictive check?
##
## Extract estimates from samples
samples <- as.matrix(hmm_fit)
theta.sample <- samples[, grep("^theta", colnames(samples))]
mu.sample <- samples[, grep("^mu", colnames(samples))]

## Simulate using these samples
hidden.sample <- matrix(NA, nrow = nrow(samples), ncol = N)
obs.sample <- matrix(NA, nrow = nrow(samples), ncol = N)
## Initial state is 1
hidden.sample[, 1] <- 1
## The Loop
for(i in 1:nrow(samples)){
    transition.matrix <- matrix(data = theta.sample[i, ], nrow = 2, ncol = 2, byrow = TRUE)
    obs.sample[i, 1] <- rnorm(n = 1, mean = mu.sample[i, hidden.sample[i, 1]], sd = 1)
    for(j in 2:N){
        hidden.sample[i, j] <- sample(x = c(1, 2), size = 1, replace = FALSE,
                               prob = transition.matrix[hidden.sample[i, j - 1], ])
        obs.sample[i, j] <- rnorm(n = 1, mean = mu.sample[i, hidden.sample[i, j]], sd = 1)       
    }
}

par(mfrow = c(1, 1))
plot(obs[1, ], type = "n",
     main = "Observed vs Predicted",
     ylab = "Observed Value",
     xlab = "Time",
     ylim = c(0,15))
for (i in 1:nrow(samples)) {
  lines(obs.sample[i, ], col = rgb(1, 0, 0, alpha = .01))
}
lines(obs[1, ], lwd = 2)
legend("topleft", c("Observed","Predicted"), lty = "solid", col = c("black", "red"), inset = 0.01, cex = 0.6)

## What are the 95% prediction intervals?
quants <- sapply(1:N, FUN = function(X){
    quantile(obs.sample[, X], probs = c(0.025, 0.975))
})


## Check this: https://github.com/imadmali/bball-hmm
## and: https://mc-stan.org/users/documentation/case-studies/bball-hmm.html
## and: https://mc-stan.org/docs/2_18/stan-users-guide/hmms-section.html
