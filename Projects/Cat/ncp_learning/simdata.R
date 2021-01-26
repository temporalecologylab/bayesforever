### Sample simulation code for understanding NCPs and stan model blocks
## Let's pretend to look at migration patterns in animals
# During peak migration season, how far do animals travel per day?
## Predictors: 0/1 if American or Canadian. Canadian's are 1s
 #             0/1 if herbivores or not, 1 if herbivore

set.seed(12321)

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
nspps <- 15
ninds <- 100 
nobs <- nspps*ninds

### These are our response variables
y_dist <- 300  ### mu_a_sp in model output
y_dist_speciessd <- 50 ### sigma_a_sp in model output

## Sigma_y to be added at the end
sigma_y <- 2 

#### Incorporate Predictors 
can_effect <- 20  ## Canadian effect, this is saying that if sites are from 1 degree north, they travel 20 fewer kms
can_sd <- 5 ## Canadian effect sd

herb_effect <- 50
herb_sd <- 10

distspp <- round(rnorm(nspps, y_dist, y_dist_speciessd), digits=0)
df.dist <- as.data.frame(cbind(species=rep(1:nspps, each=ninds), ind=rep(1:ninds), 
                                distspp=rep(distspp, each=ninds)))

df.dist$canadian <- rbinom(nobs, 1, 0.5)
df.dist$herbivore <- rbinom(nobs, 1, 0.5)

df.dist$dist.can <- df.dist$canadian * rep(rnorm(n=nspps, mean=can_effect, sd=can_sd), each=ninds)

df.dist$dist.herb <- df.dist$herbivore * rep(rnorm(n=nspps, mean=herb_effect, sd=herb_sd), each=ninds)

df.dist$distance <- df.dist$distspp + df.dist$dist.can + df.dist$dist.herb + rnorm(n=nobs, mean=0, sd=sigma_y)

##### Clean up the dataframe to prepare for analyses
distall <- df.dist[!duplicated(df.dist),]


