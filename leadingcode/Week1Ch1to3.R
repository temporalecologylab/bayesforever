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
birth1
birth2

sumBoys <- sum(birth1) + sum(birth2)
sumBairns <- sum(length(birth1)+ length(birth2))
sum1stLassies <- 100 - sum(birth1) 

#3H1
birth_p_grid <- seq(from = 0, to = 1, length.out = 1000) #same grid
birth_prior <- seq(1, 1000)
birth_likelihood <- dbinom(sumBoys, size = sumBairns, prob = birth_p_grid )
birth_posterior2 <- birth_likelihood * birth_prior
birth_posterior <- birth_posterior2 / sum(birth_posterior2 )
birth_p_grid[which.max(birth_posterior )]


#3H2 
birth_samples <- sample(birth_p_grid, prob = birth_posterior, size = 1e4, replace = TRUE)


HPDI(birth_samples, prob = 0.50)
HPDI(birth_samples, prob = 0.89)
HPDI(birth_samples, prob = c(0.5, 89, 0.97))

plot( birth_p_grid, birth_posterior)

#3H3
birth_w <- rbinom(1e4, size = 200, prob = birth_samples)
dens(birth_w)
mean(birth_w)
median(birth_w)
#model estimates the numvber of boys overall well

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
#model is underestiamting the number of boys born after girls. 
#why?
#teh probability of getting a boy is probably higher once 
#you have had one girl, because a boy and a girl are the most likely 
#combination?
