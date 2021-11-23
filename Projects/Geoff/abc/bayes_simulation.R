## Load libraries
library(ape)
library(phytools)
library(viridis) # colors
library(abc)

## Set seed
set.seed(202155)
## Number of potential host species
nspecies <- 80
## Number of realized hosts
nhosts <- 12
## Number of simulations (data) per flight parameter, alpha
ndata <- 1
## Simulate a random tree with nspecies (hosts)
tree1 <- rcoal(n = nspecies, scale = 1)
## Number of simulations (proposals) for approximate Bayesian computation
nsimulations <- 50000

## Convert tree to adjacency matrix
adjmatrix <- cophenetic(tree1)

## Set flight parameter (to be estimated from data)
param <- list(alpha = .2, 1, 5)

## Create empty lists for storage
results.temp <- list()
results <- list()
## Generate data
for(g in 1:length(param)){
    ## Set parameter
    temp.param <- param[[g]]
    for(h in 1:ndata){
        ## Create vector of all hosts
        hosts <- c(1:nspecies)
        ## ## Set initial host (random)
        ## initial.host <- sample(x = 1:nspecies, size = 1)
        ## Set initial host (fixed)
        initial.host <- 15
        ## Create vector of visited hosts
        visited.hosts <- initial.host
        ## Create vector of not visited hosts
        notvisited.hosts <-hosts[-which(hosts == initial.host)]
        for(i in 1:(nhosts - 1)){
            probs <- rep(0, length(notvisited.hosts))
            for(j in 1:length(visited.hosts)){
                probs <- probs + sapply(X = 1:length(notvisited.hosts), FUN = function(X){
                    adjmatrix[visited.hosts[j], notvisited.hosts[X]] ^ -temp.param})
            }
            new.host <- notvisited.hosts[which(rmultinom(n = 1, size = 1, prob = probs) > 0)]
            visited.hosts <- c(visited.hosts, new.host)
            notvisited.hosts <- notvisited.hosts[-which(notvisited.hosts == new.host)]    
        }
        host.distances <- adjmatrix[tree1$tip.label[visited.hosts], tree1$tip.label[visited.hosts]]
        host.distances[host.distances == 0] <- NA
        results.temp[[h]] <- list(alpha = temp.param,
                           hosts = visited.hosts,
                           summary_stats = matrix(c(mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) mean(X, na.rm = TRUE))),
                                                    mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) var(X, na.rm = TRUE))),
                                                    mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) min(X, na.rm = TRUE))),
                                                    mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) max(X, na.rm = TRUE)))), ncol = 4, nrow = 1))
    }
    results[[g]] <- results.temp
}

## Create storage for proposal simulations
phy.sim <- matrix(NA, ncol = 5, nrow = nsimulations)
## Set prior on parameter
prior.param <- function(i){
    rexp(n = 1, rate = 0.5)
}
## Generate proposal parameter and simulate walk on tree
for(h in 1:nsimulations){
    ## Generate parameter
    temp.param <- prior.param(h)
    ## Create vector of all hosts
    hosts <- c(1:nspecies)
    ## Set initial host (random)
    initial.host <- sample(x = 1:nspecies, size = 1)
    ## Create vector of visited hosts
    visited.hosts <- initial.host
    ## Create vector of not visited hosts
    notvisited.hosts <-hosts[-which(hosts == initial.host)]
    for(i in 1:(nhosts - 1)){
        probs <- rep(0, length(notvisited.hosts))
        for(j in 1:length(visited.hosts)){
            probs <- probs + sapply(X = 1:length(notvisited.hosts), FUN = function(X){
                adjmatrix[visited.hosts[j], notvisited.hosts[X]] ^ -temp.param})
        }
        new.host <- notvisited.hosts[which(rmultinom(n = 1, size = 1, prob = probs) > 0)]
        visited.hosts <- c(visited.hosts, new.host)
        notvisited.hosts <- notvisited.hosts[-which(notvisited.hosts == new.host)]    
    }
    host.distances <- adjmatrix[tree1$tip.label[visited.hosts], tree1$tip.label[visited.hosts]]
    host.distances[host.distances == 0] <- NA
    phy.sim[h, 1] <- temp.param
    phy.sim[h, 2] <- mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) mean(X, na.rm = TRUE)))
    phy.sim[h, 3] <- mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) var(X, na.rm = TRUE)))
    phy.sim[h, 4] <- mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) min(X, na.rm = TRUE)))
    phy.sim[h, 5] <- mean(apply(X = host.distances, MARGIN = 1, FUN = function(X) max(X, na.rm = TRUE)))
}
## Put into data.frame
phy.sim <- as.data.frame(phy.sim)
names(phy.sim) <- c("alpha", "MeanPD", "VarPD", "MinPD", "MaxPD")

inference1 <- abc(target = results[[1]][[1]]$summary_stats, param = data.frame(phy.sim[, "alpha"]), sumstat = phy.sim[, 2:5], trace = TRUE, method = "neuralnet", tol = 0.001)
inference2 <- abc(target = results[[2]][[1]]$summary_stats, param = data.frame(phy.sim[, "alpha"]), sumstat = phy.sim[, 2:5], trace = TRUE, method = "neuralnet", tol = 0.001)

inference3 <- abc(target = results[[3]][[1]]$summary_stats, param = data.frame(phy.sim[, "alpha"]), sumstat = phy.sim[, 2:5], trace = TRUE, method = "neuralnet", tol = 0.1)

pdf(file = "Check Estimates.pdf", width = 6, height = 6)
## Compare estimates to known values
plot.param <- list(
    xmin = 0,
    xmax = 6,
    ymin = 0,
    ymax = 3)
xseq <- seq(plot.param[["xmin"]], plot.param[["xmax"]], by = 0.1)
xx <- dexp(x = xseq, rate = .1)
plot(NA,
     xlim = c(plot.param[["xmin"]], plot.param[["xmax"]]),
     ylim = c(plot.param[["ymin"]], plot.param[["ymax"]]),
     type = "l",
     main = "",
     xlab = expression(alpha),
     ylab = "Density")
points(density(inference1$unadj.values), type = "l", col = "red")
points(density(inference2$unadj.values), type = "l", col = "blue")
points(density(inference3$unadj.values), type = "l", col = "purple")
points(density(inference1$adj.values), type = "l", lty = "dotted", col = "red")
points(density(inference2$adj.values), type = "l", lty = "dotted", col = "blue")
points(density(inference3$adj.values), type = "l", lty = "dotted", col = "purple")
legend("top", lty = c("dotted", "solid"), legend = c("Rejection", "Neural Net Adj."), inset = 0.05)
clip(x1 = 0, x2 = 6, y1 = 0, y2 = 12)
abline(v = param[[1]], lty = "dashed", col = "red")
abline(v = param[[2]], lty = "dashed", col = "blue")
abline(v = param[[3]], lty = "dashed", col = "purple")
dev.off()

crossval <- cv4abc(phy.sim[, 1], phy.sim[, 2:5], nval = 100, method = "rejection", tol = c(0.1, 0.01, 0.001))

pdf(file = "Cross validation.pdf", height = 6, width = 6)
plot(crossval, main = "Cross validation - Tolerance")
legend("bottomright", pch = 16, col = c("red", "orange", "yellow"), legend = c(0.001, 0.01, 0.1), title = "Tolerance", inset = 0.05)
dev.off()

