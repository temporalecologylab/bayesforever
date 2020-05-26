## Very short code

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("/Users/Lizzie/Documents/git/teaching/hotstats/altcourses/bayes2020/Projects/lizzie/code") 
} else if
(length(grep("catchamberlain", getwd()))>0) {setwd("~/Documents/git/pmm/analyses")
}  else
setwd("~/Documents/git/teaching/stan/pmm/analyses")

## load package
library(shinystan)

load("stan/output/nointer_2levelphy.Rda")
launch_shinystan(testme3)
