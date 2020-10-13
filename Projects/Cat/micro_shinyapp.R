### Started 23 April 2020 by Cat
## Building new dataframe with fake data to try and better understand hobo logger data versus microclimate data

# Maybe I should use estimates for fstar from real models?

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#### Overall model:
# GDD ~ urban + method + method*urban + (urban + method + method*urban|species) 

library(RColorBrewer)
library(viridis)
library(lme4)
library(ggplot2)
library(gridExtra)
library(rstan)
library(shiny)

source("~/Documents/git/microclimates/analyses/source/shinyapp_sourcedata.R")
#source("~/Documents/git/bayes2020/Projects/Cat/source/shinyapp_sourcedata.R")


ui <- fluidPage(
  
  
  titlePanel("Modeling GDD Accuracy"),
  
  column(4,
         wellPanel(
           selectInput("Hypothesis", "Hypothesis",
                       choices = c("---Choose One---",
                                   "Hypothesis A: more variation in hobo loggers with same climate", 
                                   "Hypothesis B: hobo loggers are more accurate"),
                       selected = ("Hypothesis A: more variation in hobo loggers with same climate")),
           
           selectInput("Question", "Question",
                       choices = c("---Choose One---",
                                   "Urban Model: Arb vs HF", 
                                   "Provenance Model: Provenance latitude"),
                       selected="Urban Model: Arb vs HF"),
           
           sliderInput(inputId = "UrbanEffect",
                       label = "Urban Effect",
                       value = -20, min = -100, max = 100),
           
           sliderInput(inputId = "MethodEffect",
                       label = "Method Effect",
                       value = -25, min = -100, max = 100),
           
           sliderInput(inputId = "UrbMethod",
                       label = "Urban x Method Effect",
                       value = -50, min = -100, max = 100),
           
           sliderInput(inputId = "ArbClimate",
                       label = "Arb Climate",
                       value = 10, min = 0, max = 20),
           
           sliderInput(inputId = "HFClimate",
                       label = "HF Climate",
                       value = 10, min = 0, max = 20),
           
           sliderInput(inputId = "ArbMicroEffect",
                       label = "Arb Micro Effect",
                       value = 0, min = -10, max = 10),
           
           sliderInput(inputId = "HFMicroEffect",
                       label = "HF Micro Effect",
                       value = 0, min = -10, max = 10),
           actionButton("run", "View Plots",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
           actionButton("go" ,"Run Model and View muplot")
         )       
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Climate Data", plotOutput("climtypes"), 
               plotOutput("hist")),#, verbatimTextOutput("use.urban")), 
      tabPanel("GDDs across Species", plotOutput("gddsites")), 
      tabPanel("Method Accuracy", plotOutput("gdd_accuracy")),
      tabPanel("Site Accuracy", plotOutput("site_accuracy")),
      tabPanel("Site x Method", plotOutput("interaction")),
      tabPanel("Model Output", plotOutput("muplot"))
    )
  )
  
)


server <- function(input, output) {
  
  
    get.data <- eventReactive(input$run, {bbfunc(if(input$Hypothesis=="Hypothesis A: more variation in hobo loggers with same climate" |
              input$Hypothesis=="---Choose One---")
    {TRUE}else{FALSE}, 
    if(input$Question=="Urban Model: Arb vs HF"|
       input$Question=="---Choose One---"){TRUE}else{FALSE},
    as.numeric(input$UrbanEffect), as.numeric(input$MethodEffect), 
    as.numeric(input$ArbClimate), as.numeric(input$ArbMicroEffect), 
    as.numeric(input$HFClimate), as.numeric(input$HFMicroEffect))
    })
  
  #output$print_data <- renderPrint(get.data())
  #output$strdata <- renderPrint(str(get.data()))
  
  #observeEvent(input$run, {
    output$gdd_accuracy <- renderPlot({
    bball <- get.data()[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="viridis")(3)
    plot(as.numeric(as.factor(bball$type)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$method)], ylab="GDD accuracy", xaxt="none",xlab="")
    axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$method))), pch=19,
           col=cols[as.factor(bball$method)],
           cex=1, bty="n")
  })
  #})
  
  #observeEvent(input$run, {
    output$site_accuracy <- renderPlot({
    bball <- get.data()[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="plasma")(3)
    plot(as.numeric(as.factor(bball$site)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$site)], xlab="", ylab="GDD accuracy", xaxt="none")
    axis(side=1, at=xtext, labels = c("Arnold Arboretum", "Harvard Forest"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$site))), pch=19,
           col=cols[as.factor(bball$site)],
           cex=1, bty="n")
  })
  #})
  
  #observeEvent(input$run, {
    output$gddsites <- renderPlot({
    bball <- get.data()[[1]]
    #quartz(width=6, height=5)
    par(mfrow=c(1,2))
    my.pal <- viridis_pal(option="magma")(20)
    my.pch <- c(15:16)
    plot(as.numeric(bball$gdd) ~ as.numeric(as.factor(bball$species)), col=my.pal[as.factor(bball$species)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="ws"),], main="Weather Station",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="ws"]), lwd=3)
    
    plot(as.numeric(gdd) ~ as.numeric(as.factor(species)), col=my.pal[as.factor(bball$species)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="hobo"),], main="Hobo Logger",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="hobo"]), lwd=3)
  })
  #})
  
  #observeEvent(input$run, {
    output$climtypes <- renderPlot({
    clim <- get.data()[[2]]
    cols <-viridis_pal(option="viridis")(3)
    ws <- ggplot(clim[(clim$method=="ws"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
      scale_fill_manual(name="Site", values=cols, labels=sort(unique(clim$site))) + ggtitle("Weather Station") +
      coord_cartesian(xlim=c(-10, 25)) + xlab("Mean Temp (C)") + ylab("")
    
    hl <- ggplot(clim[(clim$method=="hobo"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
      scale_fill_manual(name="Site", values=cols, labels=sort(unique(clim$site))) + ggtitle("Hobo Logger") +
      coord_cartesian(xlim=c(-10, 25)) + xlab("Mean Temp (C)") + ylab("")
    
    #quartz(width=6, height=4)
    grid.arrange(ws, hl, ncol=2)
  })
  #})
  
  #observeEvent(input$run, {
    output$hist <- renderPlot({
      bball <- get.data()[[1]]
      cols <-viridis_pal(option="plasma")(3)
      ggplot(bball, aes(x=bb)) + geom_histogram(aes(fill=site)) + theme_classic() + theme(legend.position = "none") +
        scale_fill_manual(name="Site", values=cols, labels=sort(unique(bball$site))) +
        coord_cartesian(xlim=c(0, 100)) + xlab("Day of budburst (C)") + ylab("") +
        geom_text(label=paste0("Arb obs:",nrow(bball[bball$site=="arb",])), col=cols[[1]], aes(x = 80, y = 100)) +
        geom_text(label=paste0("Arb NAs:",nrow(bball[is.na(bball$site=="arb"),])), col=cols[[1]], aes(x = 79, y = 90)) +
        geom_text(label=paste0("HF obs:",nrow(bball[bball$site=="hf",])), col=cols[[2]], aes(x = 80, y = 80)) +
      geom_text(label=paste0("HF NAs:",nrow(bball[is.na(bball$site=="hf"),])), col=cols[[2]], aes(x = 79, y = 70)) 
    })
  #})
  
  #observeEvent(input$run, {
    output$interaction <- renderPlot({
    bball.site <- get.data()[[1]]
    bball.site$methodtype <- ifelse(bball.site$method=="ws", "\nWeather \nStation", "\nHobo \nLogger")
    
    cols <- viridis_pal(option="plasma")(3)
    gddcomparebb <- ggplot(bball.site, aes(x=methodtype, y=gdd, group=as.factor(site), fill=as.factor(site))) + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
                  aes(fill = as.factor(site), group = as.factor(site))) +
      geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.text.align = 0,
            legend.key = element_rect(colour = "transparent", fill = "white"),
            plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
      xlab("") + 
      ylab("Growing degree days to budburst") + 
      scale_fill_manual(name="Site", values=cols,
                        labels=c("Arnold Arboretum", "Harvard Forest")) + 
      coord_cartesian(expand=0, ylim=c(0,700))
    
    gddcomparebb
  })
  #})
  
  use.urban <- eventReactive(input$go,{if(input$Question=="Urban Model: Arb vs HF"|
                          input$Question=="---Choose One---"){TRUE}else{FALSE}})
  
  #observeEvent(input$go, {
    output$muplot <- renderPlot({
    use.urban <- use.urban()[1]
    if(use.urban==TRUE){
      bball <- get.data()[[1]]
      bball$urban <- ifelse(bball$site=="arb", 1, 0)
      bball$type <- ifelse(bball$method=="ws", 1, 0)
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = urban,
                                method = type,
                                sp = as.numeric(as.factor(species)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$species))
                           )
      )
    }
    
    urbmethod_fake = stan('~/Documents/git/microclimates/analyses/stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                          iter = 1000, warmup=500, chains=4)#, control=list(adapt_delta=0.99)) ### 
    
    cols <- adjustcolor("indianred3", alpha.f = 0.3) 
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    
    modelhere <- urbmethod_fake
    bball <- isolate(get.data()[[1]])
    spnum <- length(unique(bball$species))
    par(xpd=FALSE)
    par(mar=c(5,10,3,10))
    plot(x=NULL,y=NULL, xlim=c(-150,50), yaxt='n', ylim=c(0,6),
         xlab="Model estimate change in growing degree days to budburst", ylab="")
    axis(2, at=1:6, labels=rev(c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                 "Sigma Arboretum", "Sigma \nWeather Station", 
                                 "Sigma Interaction")), las=1)
    abline(v=0, lty=2, col="darkgrey")
    rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                      "sigma_b_method_sp", "sigma_b_um_sp")
    ppeffects <- c("b_urban", "b_method", "b_um")
    for(i in 1:6){
      pos.y<-(1:6)[i]
      pos.x<-summary(modelhere)$summary[rownameshere[i],"mean"]
      lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
      points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
      for(spsi in 1:spnum){
        pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[6:11]#[c(2:3,8,5:6,10)]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i[i],"mean"]
        lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) # so I can plot legend outside
    legend(60, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=1, bty="n", text.font=3)
  })
  #})
    
    
    output$muplot <- renderPlot({
      use.urban <- use.urban()[1]
      if(use.urban==FALSE){
        bball <- get.data()[[1]]
        bball$type <- ifelse(bball$method=="ws", 1, 0)
        
        datalist.gdd <- with(bball, 
                             list(y = gdd, 
                                  prov = provenance,
                                  method = type,
                                  sp = as.numeric(as.factor(species)),
                                  N = nrow(bball),
                                  n_sp = length(unique(species))
                             )
        )
        
        
        provmethod_fake = stan('~/Documents/git/microclimates/analyses/stan/provmethod_normal_ncp_inter.stan', data = datalist.gdd,
                                      iter = 1000, warmup=500)#, control=list(adapt_delta=0.99)) ### 
        
      
      cols <- adjustcolor("indianred3", alpha.f = 0.3) 
      my.pal <-rep(viridis_pal(option="viridis")(9),2)
      my.pch <- rep(15:18, each=10)
      alphahere = 0.4
      
      
      modelhere <- provmethod_fake
      bball <- get.data()[[1]]
      spnum <- length(unique(bball$species))
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=c(-150,100), yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(c("Provenance \nLatitude", "Weather Station", "Provenance x\nWeather Station",
                                   "Sigma Provenance", "Sigma \nWeather Station", 
                                   "Sigma Interaction")), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_prov_sp", "mu_b_method_sp", "mu_b_pm_sp", "sigma_b_prov_sp",
                        "sigma_b_method_sp", "sigma_b_pm_sp")
      ppeffects <- c("b_urban", "b_method", "b_um")
      for(i in 1:6){
        pos.y<-(1:6)[i]
        pos.x<-summary(modelhere)$summary[rownameshere[i],"mean"]
        lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[2:7]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i[i],"mean"]
          lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
          
        }
      }
      par(xpd=TRUE) # so I can plot legend outside
      legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
             col=alpha(my.pal[1:spnum], alphahere),
             cex=1, bty="n", text.font=3)
      }
    })
  
}



shinyApp(ui = ui, server = server)


