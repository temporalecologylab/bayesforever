### Running through different models to better understand NCPs and also how to use the Stan model blocks
# Started by Cat on 26 January 2021

### For more information from Stan users manual follow this link: https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html

### Simulation Question:
# During peak migration season, how far do ungulates travel per day?
# Predictor: 0/1 if American or Canadian. Canadian's are 1s


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())


# Load Libraries
library(RColorBrewer)
library(viridis)
library(lme4)
library(ggplot2)
library(gridExtra)
library(rstan)
library(shiny)

# Set Working directory
setwd("~/Documents/git/bayes2020/Projects/Cat/ncp_learning/")

source("simdata.R")

### okay so for model 1, this is simple, no interaction and no NCP

## Check Stan speed on new comp
library(brms)
brmtest <- brm(distance ~ canadian + (canadian|species), data=distall)
brmfulltest <- brm(distance ~ canadian + herbivore + canadian*herbivore + (canadian + herbivore + canadian*herbivore|species), data=distall)


datalist.simple <- with(distall, 
                     list(y = distance, 
                          prov = provlat, ### for simple: 
                          leglength = leglength,
                          sp = as.numeric(as.factor(species)),
                          N = nrow(distall),
                          n_sp = length(unique(distall$species))
                     )
)

urbmethod_fake = stan('stan/ungulates_simple.stan', data = datalist.simple,
                      iter = 2000, warmup=1000, chains=4) ### , control=list(adapt_delta=0.99, max_treedepth=15)

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <-rep(viridis_pal(option="viridis")(9),2)
my.pch <- rep(15:18, each=10)
alphahere = 0.4

modoutput <- summary(urbmethod_fake)$summary
noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]

modelhere <- urbmethod_fake
bball <- isolate(get.data()[[1]])
spnum <- length(unique(bball$species))
quartz()
par(xpd=FALSE)
par(mar=c(5,10,3,10))
plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
     xlab="Model estimate change in growing degree days to budburst", ylab="")
axis(2, at=1:6, labels=rev(c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                             "Sigma Arboretum", "Sigma \nWeather Station", 
                             "Sigma Interaction")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                  "sigma_b_method_sp", "sigma_b_um_sp")
for(i in 1:6){
  pos.y<-(6:1)[i]
  pos.x<-noncps[rownameshere[i],"mean"]
  lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  for(spsi in 1:spnum){
    pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[2:4]
    jitt<-(spsi/40) + 0.08
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
    lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
          col=alpha(my.pal[spsi], alphahere))
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
    
  }
}
par(xpd=TRUE) # so I can plot legend outside
legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
       col=alpha(my.pal[1:spnum], alphahere),
       cex=1, bty="n", text.font=3)
})
#})
