rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
library(brms)

data(WaffleDivorce, package = "rethinking")
d <- WaffleDivorce
rm(WaffleDivorce)

d <-
  d %>% 
  mutate(D_obs = (Divorce - mean(Divorce)) / sd(Divorce),
         D_sd  = Divorce.SE / sd(Divorce),
         M     = (Marriage - mean(Marriage)) / sd(Marriage),
         A     = (MedianAgeMarriage - mean(MedianAgeMarriage)) / sd(MedianAgeMarriage),
         M_obs = M,
         M_sd  = Marriage.SE / sd(Marriage))

### missing y
dlist <- list(
  D_obs = d$D_obs,
  D_sd  = d$D_sd,
  M     = d$M,
  A     = d$A)

b15.1 <- 
  brm(data = dlist, 
      family = gaussian,
      D_obs | mi(D_sd) ~ 1 + A + M,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 15,
      # note this line
      save_mevars = TRUE)
###both sides
dlist <- list(
  D_obs = d$D_obs,
  D_sd  = d$D_sd,
  M_obs = d$M_obs,
  M_sd  = d$M_sd,
  A     = d$A)

b15.2 <- 
  brm(data = dlist, 
      family = gaussian,
      D_obs | mi(D_sd) ~ 1 + A + me(M_obs, M_sd),
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(normal(0, 1), class = meanme),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 15,
      # note this line
      save_mevars = TRUE)

b15.2 <- 
  brm(data = dlist, 
      family = gaussian,
      D_obs | mi(D_sd) ~ 1 + A + me(M_obs, M_sd),
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(normal(0, 1), class = meanme),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 15,
      # note this line
      save_mevars = TRUE)

stancode(b15.3)


b15.3 <- 
  brm(data = dlist, 
      family = gaussian,
      D_obs | mi(D_sd) ~ me(M_obs, M_sd),
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 1), class = meanme),
                prior(exponential(1), class = sigma)),
      iter = 4000, warmup = 3000, cores = 4, chains = 4,
      seed = 15,
      # note this line
      save_mevars = TRUE)
