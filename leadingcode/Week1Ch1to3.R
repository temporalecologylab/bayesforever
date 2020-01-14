# Rethinking Chapters 1-3
#---------------------------------

library(rethinking)



#chapter 1
#-------------

#models are like golumn in that they are nether right or wrong, they just do what they are told. They are
#very useful but you need to be careful when interpreting them 

#building your own golumn gives you greater understanding of its strengths and weaknesses, rather than using an
#out of the box golum. You also have way more flexibility. 

#philosophy of science - we dont due strict Popperism becasuse science is messier than that 
#   * hypotheses are not models
#   * falsefying is really difficult, and related to measurement and observation error. Did you REALLY see a black swan???  

#Bayesian probability theory 
# * a probability concept, counting the number of ways things can happen 
#* probability is different from frequency . no imagined resampling
# * multilevel regression deserves to be the defualt option 

#chapter 2
#------------

#small world = self contained logical world of the model
#large world = broader context teh model is deployed into 
#baysian models perform well when their assumptions approximate the large world conditions

#garden of forking data 

#prior probabilities - how much do you trust the person who saw the unicorn?
#https://xkcd.com/1132/ - cartoon frequentis vs bayesian 

#model components
# * likelyhood  - formula that specifies the plausibility of the data. Binomial, gausian exc
#* parameters - adjustable model inputs
#* priors - initial plausibilty. can be varying amounts of useful 
# posterior - probability after the model has run . (Likelyhood * Prior )/Average liklyhood 

#Chapter 3 - sampling the imaginery 
#------------------------------------------




#go through hard practice problem 
data(homeworkch3)
#data indicate if the first and second child (birth 1 and birth 2 respectively)
# were male (1)or female (2)

#explore data 
birth1
birth2
sumBoys <- sum(birth1) + sum(birth2) # total number of boys and girls born 
sumBairns <- sum(length(birth1)+ length(birth2))
sum1stLassies <- 100 - sum(birth1) 

#3H1
#posterior distribution for the probability of being a boy 
birth_p_grid <- seq(from = 0, to = 1, length.out = 1000) #same grid as chapter examples
plot(birth_p_grid)
birth_prior <- seq(1, 1000)# uniform prior 
birth_likelihood <- dbinom(sumBoys, size = sumBairns, prob = birth_p_grid )# binomial likelyhood 
birth_posterior2 <- birth_likelihood * birth_prior # posterior takes two steps to compute  
birth_posterior <- birth_posterior2 / sum(birth_posterior2 )
plot(birth_posterior ~ birth_p_grid)#probability of being a boy probably between 0.4 and 0.7

#3H2 
#its really important to summarise and interpret the posterior distribution 
#for example, we can sum probability that proportion of water is less than 0.5
#we start exploring teh posterior distribution by sampling from the grid of all possible
#probabilities of being a boy, but takes far more samples of that probability if the posterior
#distribution probability values is high. So 0.5 for instance is samples a lot. 
birth_samples <- sample(birth_p_grid, prob = birth_posterior, size = 1e4, replace = TRUE)
#what does it look like?
plot(birth_samples)
dens(birth_samples)

#Highest posterior desnity interval (avoid 95 because of baggage)
rethinking::HPDI(birth_samples, prob = 0.50)#narrowest interval containing 50% of probability mass
HPDI(birth_samples, prob = 0.89)
HPDI(birth_samples, prob = c(0.5, 89, 0.97))
#percentile intervales - probabilities falling within 50% confidence interval 
rethinking::PI(birth_samples,prob =0.5)
#point estimate - maximum a posteriori (MAP). But a single ponit isnt a good way of describing 
# a distribution 
birth_p_grid[which.max(birth_posterior )]# which parameter value (prob of being a boy) 
#maximises the posterior probability?

#samples from teh posterior are also useful for simulating model's implied observations. This
#is good for:
#* model checking - does teh modle tell us what we expect?
#* software validation 
#* Research design 
#* forcasting 

#3H3
#out of 200 babies, how many do we expect to be boys based on our model posterior?
#make a posterior predictive destribution that includes uncertainty around probability (birth_samples)
#rather than just using the most likely probability of being a boy for teh prob value of rbinom
birth_w <- rbinom(1e4, size = 200, prob = birth_samples)#somewhere between 80 and 140 boys
birth_w1 <- rbinom(1e4, size = 200, prob = 0.6)
dens(birth_w1)
dens(birth_w)
mean(birth_w)
median(birth_w)
#model estimates the numvber of boys overall well (111 boys)

#3H4
birth_w1 <- rbinom(1e4, size = 100, prob = birth_samples)
dens(birth_w1)
mean(birth_w1)
median(birth_w1)
sum(birth1)
#model overestimates the number of boys 1st born 


#3H5
birth_w1f <- rbinom(1e4, size = 51, prob = birth_samples)
dens(birth_w1f)#28.5
mean(birth_w1f)#28
median(birth_w1f)
afterGirls <- birth2[birth1 == 0]
sum(afterGirls)
#model is underestiamting the number of boys born after girls. 
#why?
#the probability of getting a boy is probably higher once 
#you have had one girl, because a boy and a girl are the most likely 
#combination?
