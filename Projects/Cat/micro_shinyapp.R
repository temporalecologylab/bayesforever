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

#source("~/Documents/git/microclimates/analyses/source/shinyapp_sourcedata.R")
#source("~/Documents/git/microclimates/analyses/source/microurban_muplot.R")
source("~/Documents/git/bayes2020/Projects/Cat/source/shinyapp_sourcedata.R")
source("~/Documents/git/bayes2020/Projects/Cat/source/microurban_muplot.R")


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
                       value = -50, min = -100, max = 100),
           
           sliderInput(inputId = "MethodEffect",
                       label = "Method Effect",
                       value = -30, min = -100, max = 100),
           
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
           submitButton("View Plots")
         )       
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Climate Data", plotOutput("climtypes")),#, verbatimTextOutput("use.urban")), 
      tabPanel("GDDs across Species", plotOutput("gddsites")), 
      tabPanel("Method Accuracy", plotOutput("gdd_accuracy")),
      tabPanel("Site Accuracy", plotOutput("site_accuracy")),
      tabPanel("Site x Method", plotOutput("interaction")),
      tabPanel("Model Output", actionButton("go" ,"Run Model and View muplot"), plotOutput("muplot"))
    )
  )
  
)


server <- function(input, output) {
  
  get.data <- reactive({
    bbfunc(if(input$Hypothesis=="Hypothesis A: more variation in hobo loggers with same climate" |
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
  
  output$site_accuracy <- renderPlot({
    bball <- get.data()[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="viridis")(3)
    plot(as.numeric(as.factor(bball$site)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$site)], xlab="", ylab="GDD accuracy", xaxt="none")
    axis(side=1, at=xtext, labels = c("Arnold Arboretum", "Harvard Forest"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$site))), pch=19,
           col=cols[as.factor(bball$site)],
           cex=1, bty="n")
  })
  
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
  
  use.urban<-reactive({if(input$Question=="Urban Model: Arb vs HF"|
                          input$Question=="---Choose One---"){TRUE}else{FALSE}})
  
  
  #ntext <- eventReactive(input$goButton, {
  # 
  output$muplot <- renderPlot({
    input$go
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
    
    modelhere <- urbmethod_fake
    bball.stan <- get.data()[[1]]
    muplotfx(modelhere, "", 7, 7, c(0,3), c(-150, 50), 60, 3)
  })
  
  
}



shinyApp(ui = ui, server = server)

