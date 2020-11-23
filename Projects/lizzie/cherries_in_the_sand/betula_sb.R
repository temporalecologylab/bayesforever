library("tidyverse")
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#Set temperature window
a <- 32            # Feb 1st, beginning of window for average temperature
b <- 121           # May 31st, end of window for average temperature
alpha_0 <- 32      # plants start storing on FEB 1 when average temperature exceeds freezing 


#################
# Read Eurodata #
#################
setwd("~/Documents/git/teaching/hotstats/altcourses/bayes2020/Projects/lizzie/cherries_in_the_sand")
bp <- read.csv("betpen_decsens_1950-2000.csv")
if(FALSE){ # tmax calculated with base 0 temperatures
bp <- read.csv("betpen_decsens_1950-2000base0.csv")
names(bp) <- c("lo", "year", "lat.lon", "mat", "gdd", "siteslist")
}
#############
# Model Fit #
#############
bp_stan <- subset(bp, siteslist <= 15)
bp_stan <- subset(bp, siteslist <= 17 & year>1951)
n_beta <- bp_stan$lo
M <- (bp_stan$mat60 * (9/5))+32
site <- as.numeric(factor(bp_stan$siteslist))
year <- as.numeric(factor(bp_stan$year))
# year <- ifelse(bp_stan$year<1981, 0, bp_stan$year-1980))

stan_data <- list(N       = length(n_beta),
                  S       = length(unique(site)),
                  Y       = length(unique(year)),
                  site    = site,
                  year    = year,
                  M       = M,
                  n_beta  = n_beta,
                  a       = a,
                  b       = b,
                  alpha_0 = alpha_0)

#set initial values since likelihood is constrained
init <- function () 
  list(alpha_1         = rep(.5, stan_data$Y),
       beta            = rep(b * max(M), stan_data$Y),
       sigma           = rep(100, stan_data$S),
       gamma_0_alpha   = .5,
       gamma_1_alpha   = 0,
       gamma_0_beta    = b * max(M),
       gamma_1_beta    = 0,
       tau_alpha       = 100,    
       tau_beta        = 100    
  )
model <- stan_model("betula_sb.stan")
fit_bayes <- sampling(model, data=stan_data, init = init, iter = 5000)

####################
# Model Evaluation #
####################

#compare estimated sensitivity with gdd
tibble(gdd = bp_stan$gdd,
       year = bp_stan$year,
       site = factor(bp_stan$siteslist, levels = c(1:17, "overall"))) %>%
  left_join(tibble(beta = colMeans(rstan::extract(fit_bayes, "beta")[[1]]),
                   year = unique(bp_stan$year)),
            by = "year") %>%
  bind_rows(bind_cols(beta = colMeans(rstan::extract(fit_bayes, "beta")[[1]]),
                      bp_stan %>% group_by(year) %>% summarize(gdd = mean(gdd))) %>%
              mutate(site = "overall",
                     site = factor(site, levels = c(1:17, "overall"))) %>%
              select(gdd, year, site, beta)) %>%
  group_by(site) %>%
  mutate(corr           = round(cor(beta, gdd), 2),  
         slope_corr     = ifelse(corr < 0, -1, 1) * sd(beta) / sd(gdd),
         intercept_corr = (mean(beta) - ifelse(corr < 0, -1, 1) * sd(beta) / 
                             sd(gdd) * mean(gdd))) %>%
  ggplot() +
  theme_bw() +
  aes(gdd, beta) +
  geom_point(aes(color = factor(site))) +
  geom_abline(aes(intercept = intercept_corr, slope = slope_corr), 
  linetype = 2) +
  geom_text(x = 150, y = 3900, aes(label = corr), alpha = .05) +
  facet_wrap(~ site, labeller = labeller(site = label_both)) +
  theme(legend.position = "none") +
  labs(y = (expression(beta)), title = "Correlation between threshold and GDD")


#test declining sensitivity
alpha_trend <- 
  tibble(
    gamma_0_alpha = rstan::extract(fit_bayes, "gamma_0_alpha")[[1]],
    gamma_1_alpha = rstan::extract(fit_bayes, "gamma_1_alpha")[[1]],
    key = "alpha[1]"
)

beta_trend <- 
  tibble(
    gamma_0_beta = rstan::extract(fit_bayes, "gamma_0_beta")[[1]],
    gamma_1_beta = rstan::extract(fit_bayes, "gamma_1_beta")[[1]],
    key = "beta"
  )

annual <-
  tibble(`alpha[1]` = colMeans(rstan::extract(fit_bayes, "alpha_1")[[1]]),
         beta    = colMeans(rstan::extract(fit_bayes, "beta")[[1]]),
         year    = unique(bp_stan$year)) %>%
  gather(key = "key", value = "value", -year)

ggplot() +
  theme_bw() +
  geom_point(aes(x = year, y = value), data = annual) +
  geom_abline(aes(intercept = gamma_0_alpha - gamma_1_alpha * 1950, 
                  slope = gamma_1_alpha), data = alpha_trend, 
              alpha = .01, color = "red") +
  geom_abline(aes(intercept = gamma_0_beta - gamma_1_beta * 1950, 
                  slope = gamma_1_beta), data = beta_trend, 
              alpha = .01, color = "blue") +
  facet_grid(key ~ ., scales = "free", labeller = label_parsed, switch = "y") +
  geom_abline(aes(intercept = mean_value, slope = 0), linetype = 2,
              data = annual %>% 
                     group_by(key) %>% 
                     summarize(mean_value = mean(value))) +
  theme(strip.text.y = element_text(angle=180)) +
  scale_y_continuous(name = "", position = "right") +
  labs(x = "", 
       title = "Warming (top) and Sensitivity (bottom) for Betula pendula")
