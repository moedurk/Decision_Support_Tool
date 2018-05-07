library(rhandsontable)
library(shiny)
library(jagsUI)
library(MASS) #needed for mvrnorm()
library(BB) #needed for dfsane()
library(rmarkdown) #needed for generating downloadable reports
library(ggplot2)



#LOAD VALUES AND FUNCTIONS####


#season hatch/fate calculation functions####
h.ex.calc<-function(parms){
  
  lin.f.ex<-exp(parms$alpha.f)
  lin.a.ex<-exp(parms$alpha.a+ parms$beta.a.ex)
  lin.p.ex<-exp(parms$alpha.p  + parms$beta.p.ex)
  survp.ex<-1/(lin.f.ex+lin.a.ex+lin.p.ex+1)
  pred.p.ex<-((lin.p.ex/(lin.f.ex+lin.a.ex+lin.p.ex+1))/(1-survp.ex)*(1-survp.ex^34))
  a<-((lin.a.ex/(lin.f.ex+lin.a.ex+lin.p.ex+1))/(1-survp.ex)*(1-survp.ex^34))
  flood.p.ex<-((lin.f.ex/(lin.f.ex+lin.a.ex+lin.p.ex+1))/(1-survp.ex)*(1-survp.ex^34))
  
  h<-survp.ex^34   #probability of period survival exclosed nest
  o<-flood.p.ex+pred.p.ex 
  m=parms$m
  r2=parms$r2
  r3=parms$r3
  h.ex<-h + (o*r2*h + a*(1-m)*r2*h) + o*r2*(o*r3*h + a*(1-m)*r3*h) + a*(1-m)*r2*(o*r3*h+a*(1-m)*r3*h)
  output<-list(h.ex=h.ex, a=a)
  return(output)
}

h.un.calc<-function(parms){
  lin.f<-exp(parms$alpha.f)
  lin.a<-exp(parms$alpha.a)
  lin.p<-exp(parms$alpha.p)
  survp<-1/(lin.f+lin.a+lin.p+1)   #daily survival probability
  
  #period fate probabilities for each nest
  pred.p<-((lin.p/(lin.f+lin.a+lin.p+1))/(1-survp)*(1-survp^34))  #predation unexclosed
  a<-((lin.a/(lin.f+lin.a+lin.p+1))/(1-survp)*(1-survp^34))  #abandonment unexclosed
  flood.p<-((lin.f/(lin.f+lin.a+lin.p+1))/(1-survp)*(1-survp^34))  #flooding probability
  o<-flood.p+pred.p
  h<-survp^34  #probability of period survival unexclosed nest
  m=parms$m
  r2=parms$r2
  r3=parms$r3
  h.un <- h + (o*r2*h + a*(1-m)*r2*h) + o*r2*(o*r3*h + a*(1-m)*r3*h) + a*(1-m)*r2*(o*r3*h+a*(1-m)*r3*h) #revised; mistake somewhere in original
  output<-list(h.un=h.un, a=a, p=pred.p)
  return(output)
}
#logodds####
logodds=function(x){log(x/(1-x))}

#projection function####
lambda.calc <- function(parms, sd.parms, eta.a=0, eta.p=0, beta.a.add=0, n.ex=0, n.iter=1000){ #take out etas - not used
  n.iter=n.iter
  eta.a=eta.a
  eta.p=eta.p
  lambda <- rep(NA, n.iter)
  aban.counts.ex <- rep(NA, n.iter)
  aban.counts.un <- rep(NA, n.iter)
  pred.counts.un <- rep(NA, n.iter)
  dead.females.ex <- rep(NA, n.iter)
  
  #run iterations
  for (i in 1:n.iter){
    #draw stochastic values
    parms.iter <- list(
      survmean=plogis(mvrnorm(1,c(qlogis(parms$Phij),qlogis(parms$Phia)),sd.parms$vcovSurv)),
      f = plogis(rnorm(1,qlogis(parms$f),sd.parms$sd.f)),
      ys = rnorm(1,logodds(parms$ys),sd.parms$ys), #probability of second-year bird nesting
      yt = parms$yt, #probability of third-year bird nesting fixed at 0.99
      r2 = rnorm(1, logodds(parms$r2), sd.parms$r2),
      r3 = rnorm(1, logodds(parms$r3), sd.parms$r3),
      m = plogis(rnorm(1,qlogis(parms$m), sd.parms$m)),
      alpha.f = rnorm(1, parms$alpha.f, sd.parms$alpha.f),
      aban.coeff = mvrnorm(1, c(parms$alpha.a, parms$beta.a.ex+beta.a.add), sd.parms$vcovAban),
      pred.coeff = mvrnorm(1, c(parms$alpha.p, parms$beta.p.ex), sd.parms$vcovPred)
    )
    parms.iter$alpha.a <- parms.iter$aban.coeff[1] + eta.a
    parms.iter$beta.a.ex <- parms.iter$aban.coeff[2]
    parms.iter$alpha.p <- parms.iter$pred.coeff[1] + eta.p
    parms.iter$beta.p.ex <- parms.iter$pred.coeff[2]
    
    #baseline abandonment rate (unexclosed)
    a.p.un<-h.un.calc(parms=parms.iter)$a
    
    #hatch and exclosure-related abandonment probabilities
    h.un<-h.un.calc(parms=parms.iter)$h.un
    h.ex<-h.ex.calc(parms=parms.iter)$h.ex
    a.p.ex<-h.ex.calc(parms=parms.iter)$a
    p.p.un <- h.un.calc(parms=parms.iter)$p
    
    #fecundity
    Fa<-parms.iter$yt*(2*parms$E*(n.ex*h.ex + (1-n.ex)*h.un))
    Fs<-parms.iter$ys*(2*parms$E*(n.ex*h.ex + (1-n.ex)*h.un))
    
    #breeding-season mortality with abandonment
    m<-parms.iter$m; r2<-parms.iter$r2; r3<-parms.iter$r3
    m.a.un=parms.iter$yt*(a.p.un*m+a.p.un*(1-m)*r2*(a.p.un*m+a.p.un*(1-m)*r3*a.p.un*m)) 
    m.s.un=parms.iter$ys*(a.p.un*m+a.p.un*(1-m)*r2*(a.p.un*m+a.p.un*(1-m)*r3*a.p.un*m))
    m.a.ex=parms.iter$yt*(a.p.ex*m+a.p.ex*(1-m)*r2*(a.p.ex*m+a.p.ex*(1-m)*r3*a.p.ex*m))  
    m.s.ex=parms.iter$ys*(a.p.ex*m+a.p.ex*(1-m)*r2*(a.p.ex*m+a.p.ex*(1-m)*r3*a.p.ex*m))
    
    aban.counts.ex[i]<-a.p.ex*n.ex*sum(parms$Na0,parms$Ns0)
    aban.counts.un[i]<-a.p.un*(1-n.ex)*sum(parms$Na0,parms$Ns0)
    pred.counts.un[i]<-p.p.un*(1-n.ex)*sum(parms$Na0,parms$Ns0)
    dead.females.ex[i]<- m.s.ex*parms$Ns0 + m.a.ex*parms$Na0
    
    #annual survival rates	
    Phij.w <- parms.iter$survmean[1]^(10/12) #winter post-fledging juvenile survival
    Phia.w <- parms.iter$survmean[2]^(10/12) #winter adult survival
    Phij.b <- parms.iter$survmean[1]^(2/12)  #second-year breeding season survival
    Phia.b <- parms.iter$survmean[2]^(2/12)  #ASY breeding survival
    Phij.b <- Phij.b/(1-m.s.un)*(1-m.s.ex*n.ex)  #add in probability of surviving exclosure-related abandonment
    Phia.b <- Phia.b/(1-m.a.un)*(1-m.a.ex*n.ex)
    Phij.ann <- Phij.b*Phij.w 
    Phia.ann <- Phia.b*Phia.w
    
    #matrix calculations
    f<-parms.iter$f
    Lesmat<-matrix(c(Phij.ann*Fs*f,Fa*Phia.ann,Phij.ann*f,Phia.ann),2,2,byrow=TRUE)  #found mistake in original; entry [2,1] had adult survival, not juv
    lambda[i]<-eigen(Lesmat)$values[1]
  } #n.iter
  output <- list(lambda=lambda, aban.counts.ex=aban.counts.ex, aban.counts.un=aban.counts.un, 
                 dead.females.ex=dead.females.ex, pred.counts.un=pred.counts.un)
  return(output)
}

#mean parameters####

mean.parms <- list(
  yt=0.99,  #probability of third-year bird nesting
  ys=0.68,  #probability of second-year bird nesting
  Phij=0.52,  
  Phia=0.74,
  f=0.4,
  m=0.34,
  E = 0.94,
  r2 = 0.7,
  r3 = 0.7,
  beta.a.ex = 1.284,
  beta.p.ex = -2.532,
  alpha.a = -7.447,
  alpha.p = -4.257,
  alpha.f = -6.040
)	

mean.alpha.a = -7.447
mean.alpha.p = -4.257
CV=0.1 #coefficient of variation for most stochastic parameters

#standard deviations for stochasticity	
sd.parms.init <- list(
  vcovSurv=matrix(c((logodds(mean.parms$Phij)*CV)^2,logodds(mean.parms$Phij)*CV*logodds(mean.parms$Phia)*CV, 
                    logodds(mean.parms$Phij)*CV*logodds(mean.parms$Phia)*CV, (logodds(mean.parms$Phia)*CV))^2,2,2),  #had sd in here earlier, not variance
  sd.f = abs(logodds(mean.parms$f))*0.06,
  yt = logodds(mean.parms$yt)*CV,
  ys = logodds(mean.parms$ys)*CV,
  r2 = abs(logodds(mean.parms$r2))*CV,
  r3 = abs(logodds(mean.parms$r3))*CV,
  m = abs(logodds(mean.parms$m))*1.04,
  beta.a.ex = 0.348,
  beta.p.ex = 0.272,
  alpha.a = 0.386,
  alpha.p = 0.116,
  alpha.f = 0.222,
  vcovAban = matrix(c(0.386^2, -0.092, -0.092, 0.348^2),2,2),
  vcovPred = matrix(c(0.116^2, -0.007, -0.007, 0.272^2),2,2)
)



#Example data for download####
exampleDat <- read.csv("sampleData.csv", header=T)

#SERVER####
shinyServer(
 function(input, output, session) {
   
   #Downloadable instructions####
   output$instructions <- downloadHandler(
     filename = "PiperEx_Instructions.pdf",
     content = function(file) {
       file.copy("www/PiperEx_Instructions.pdf", file)
     }
   )

#upload and edit nest data  
  nestdata <- reactiveValues()
  
  observeEvent(input$file,{
    if (!is.null(input$file)){
    nestdata$nestdata <- read.csv(input$file$datapath, stringsAsFactors = F)
    nestdata$nestdata$Date <- as.Date(nestdata$nestdata$Date, format="%m/%d/%Y")
    nestdata$nestdata$Interval <- as.integer(rep(NA, length(nestdata$nestdata[,1])))
    
    nestdata$nestdata <- nestdata$nestdata[order(nestdata$nestdata$Nest.ID, nestdata$nestdata$Date),]
    
    Interval<-rep(NA, length(nestdata$nestdata$Nest.ID))
    postFate <- rep(0, length(nestdata$nestdata$Nest.ID))
    
    for(i in 2:length(nestdata$nestdata$Nest.ID)){
      if (nestdata$nestdata$Nest.ID[i]==nestdata$nestdata$Nest.ID[i-1] && !is.na(nestdata$nestdata$Date[i]) &&
          !is.na(nestdata$nestdata$Date[i-1])) {Interval[i]=(as.numeric(nestdata$nestdata$Date[i]-nestdata$nestdata$Date[i-1]))}
      else {Interval[i]=NA}
      
      if(nestdata$nestdata$Nest.ID[i]==nestdata$nestdata$Nest.ID[i-1] && nestdata$nestdata$Status[i-1]==6)
        postFate[i] <- 1
      if(nestdata$nestdata$Nest.ID[i]==nestdata$nestdata$Nest.ID[i-1] && nestdata$nestdata$Status[i-1]==2)
        postFate[i] <- 1
      if(nestdata$nestdata$Nest.ID[i]==nestdata$nestdata$Nest.ID[i-1] && nestdata$nestdata$Status[i-1]==4)
        postFate[i] <- 1
    }
    
    nestdata$nestdata$Interval <- Interval
    nestdata$nestdata <- nestdata$nestdata[postFate==0,] #remove post-fate rows
    nestdata$nestdata <- subset(nestdata$nestdata, select=c("Nest.ID","Date","Status","Exclosed","Interval"))
    
    }
  })
  
  observeEvent(input$hot, {
    if(!is.null(input$hot)){
      nestdata$nestdata <- hot_to_r(input$hot)
    }
    
  })
  
#NEST FATE ANALYSIS####   
 
  survival <- eventReactive(input$analyze.go,{
    validate(
      need(!is.null(nestdata$nestdata), "Please upload data"),
      need(length(which(is.na(nestdata$nestdata$Date)))<1, "Warning: missing dates detected. Double-check date formatting.")
    )
    withProgress(message = "Calculating, this may take a few minutes...",{
     nestdata <- nestdata$nestdata
     nest.count <- length(unique(nestdata$Nest.ID))
     nestdata<-nestdata[!is.na(nestdata$Interval[]),] #remove first nest check
     nestdata<-nestdata[nestdata$Interval[]>=1,] #removes any 0 or negative (=error) nest check intervals
     
     n <- length(nestdata[,1])
     
     Surv=rep(0,n)
     Aban=rep(0,n)
     Flood=rep(0,n)
     Pred=rep(0,n)
     
     for (i in 1:n){
       Surv[i][nestdata[i,"Status"]==1|nestdata[i,"Status"]==6]<-1
       Aban[i][nestdata[i,"Status"]==4]<-1
       Pred[i][nestdata[i,"Status"]==2]<-1
       Flood[i][nestdata[i,"Status"]==3]<-1
     }			
     Fate<-cbind(Surv, Aban, Pred, Flood)
     
     for (i in 1:length(Fate[,1])){
       if (sum(Fate[i,])==0) {Fate[i,]<-NA}
     }
     #replace Y/N in exclosed column to 0 and 1 if necessary
     if(!is.numeric(nestdata$Exclosed)){
       nestdata$Exclosed<-gsub(".*(n).*","0",nestdata$Exclosed, ignore.case=T)
       nestdata$Exclosed<-gsub(".*(y).*", "1", nestdata$Exclosed, ignore.case=T)
       nestdata$Exclosed<-as.numeric(nestdata$Exclosed)
     }
     
     #data validation
     validate(
       need(!is.na(max(nestdata$Exclosed)), "Cannot Proceed: Missing data in exclosure column. Please fill in missing data.")

     )
     
     #bundle data and prep for jags
     win.data<-list(ex = nestdata[,"Exclosed"],n=n,interval=nestdata[,"Interval"],Fate=Fate,
                    mean.alpha.a = mean.alpha.a, mean.alpha.p=mean.alpha.p)
     inits<-function(){list(alpha.p=rnorm(1,0,1), alpha.a=rnorm(1,0,1), alpha.f=rnorm(1,0,1), beta.a.ex=rnorm(1,0,1),
                            beta.p.ex=rnorm(1,0,1))}
     
     params<-c("alpha.p","alpha.f","alpha.a", "beta.a.ex","beta.p.ex", "Hatchp.ex", "Hatchp.un",
               "Survp.ex","Survp.un","Abanp.ex","Abanp.un", "Predp.ex","Predp.un", "Flood.p")
     
     nc<-3
     
     out<-autojags(win.data, inits, params, "single_site_model.txt", n.chains=nc, parallel=TRUE)
     
     #extract estimates for later use
     #Period fate probabilities:
     Fate.est <- list(Hatch.Ex = out$mean$Hatchp.ex,
                      Hatch.Un = out$mean$Hatchp.un,
                      Aban.Ex = out$mean$Abanp.ex,
                      Aban.Un = out$mean$Abanp.un,
                      Pred.Ex = out$mean$Predp.ex,
                      Pred.Un = out$mean$Predp.un,
                      Flood = out$mean$Flood.p)
     
     #standard deviations for fate probabilities
     Fate.SD <- list(Hatch.Ex = out$sd$Hatchp.ex,
                     Hatch.Un = out$sd$Hatchp.un,
                     Aban.Ex = out$sd$Abanp.ex,
                     Aban.Un = out$sd$Abanp.un,
                     Pred.Ex = out$sd$Predp.ex,
                     Pred.Un = out$sd$Predp.un,
                     Flood = out$sd$Flood.p)
     
     #Model Estimates:
     alpha.p <- out$mean$alpha.p
     alpha.a <- out$mean$alpha.a
     alpha.f <- out$mean$alpha.f
     beta.a.ex <- out$mean$beta.a.ex
     beta.p.ex <- out$mean$beta.p.ex

     #covariances
     vcovAban <- cov(cbind(out$sims.list$alpha.a, out$sims.list$beta.a.ex))
     vcovPred <- cov(cbind(out$sims.list$alpha.p, out$sims.list$beta.p.ex))
     
     alpha.f.sd <- out$sd$alpha.f
     
     #prepare output
     list(alpha.p=alpha.p, alpha.a=alpha.a, alpha.f=alpha.f, beta.a.ex=beta.a.ex,  beta.p.ex=beta.p.ex, vcovAban=vcovAban,
          vcovPred=vcovPred, Fate.est=Fate.est,Fate.SD=Fate.SD, out=out, alpha.f.sd=alpha.f.sd) 
    })
  }) #survival()
  
output$SmallSampleWarn <- renderText({ 
  if (input.summary()$nest.count<=10){
    paste("Warning: Data Entered Do Not Meet Minimum Recommended Sample Size of 10 Nests")
  }
  })  
  
#update params to use in lambda calculations	####
  parms <- reactive({
    list(
      Na0 = (input$Pairs)/2,
      Ns0 = (input$Pairs)/2,
      yt=0.99,  #probability of third-year bird nesting
      ys=0.68,  #probability of second-year bird nesting
      Phij=0.52,  
      Phia=0.74,
      f=0.4,
      m=0.34,
      E = 0.94,
      r2 = 0.7,
      r3 = 0.7,
      beta.a.ex = survival()$beta.a.ex,
      beta.p.ex = survival()$beta.p.ex,
      alpha.a = survival()$alpha.a,
      alpha.p = survival()$alpha.p,
      alpha.f = survival()$alpha.f
    ) })
  
  sd.parms <- reactive({
    list(
      vcovSurv=matrix(c((logodds(parms()$Phij)*CV)^2,logodds(parms()$Phij)*CV*logodds(parms()$Phia)*CV, 
                        logodds(parms()$Phij)*CV*logodds(parms()$Phia)*CV, (logodds(parms()$Phia)*CV))^2,2,2),
      sd.f = abs(logodds(parms()$f))*0.06,
      yt = logodds(parms()$yt)*CV,
      ys = logodds(parms()$ys)*CV,
      r2 = abs(logodds(parms()$r2))*CV,
      r3 = abs(logodds(parms()$r3))*CV,
      m = abs(logodds(parms()$m))*1.04,  
      alpha.f = survival()$out$sd$alpha.f,
      vcovAban = survival()$vcovAban,
      vcovPred = survival()$vcovPred
    ) })

  ###################################################################################### lambda
  
#LAMBDA####
  
  trajectory<-reactive( {
   
    withProgress(message = "Calculating Projections ..", {

      traj.noEx <- lambda.calc(parms=parms(), sd.parms=sd.parms(), n.ex=0, n.iter=10000) #lambda without exclosures
      traj.Ex <- lambda.calc(parms=parms(), sd.parms=sd.parms(), n.ex=1, n.iter=10000) #lambda with exclosures
      
      #calculate probabilities of decline and growth
      decline.ex <- mean(traj.Ex$lambda<1)
      decline.un <- mean(traj.noEx$lambda<1)
      
      decline.steep.ex <- mean(traj.Ex$lambda<0.95)
      decline.steep.un <- mean(traj.noEx$lambda<0.95)
      
      growth.ex <- mean(traj.Ex$lambda>1)
      growth.un <- mean(traj.noEx$lambda>1)
      
      growth.steep.ex <-mean(traj.Ex$lambda>1.05)
      growth.steep.un <-mean(traj.noEx$lambda>1.05)
      
      #prepare output
      lambda.plot <- data.frame(
        Growth = c( traj.Ex$lambda[traj.Ex$lambda<2], traj.noEx$lambda[traj.noEx$lambda<2]), #remove extremes
        Exclosures = c(rep("Yes", length(traj.Ex$lambda[traj.Ex$lambda<2])), rep("No", length(traj.noEx$lambda[traj.noEx$lambda<2])))
      )
      #reorder factor level Yes/No for plotting
      lambda.plot$Exclosures <- factor(lambda.plot$Exclosures, levels(lambda.plot$Exclosures)[c(2,1)])
 
      list(lambda.un=traj.noEx$lambda, lambda.ex=traj.Ex$lambda, aban.counts.ex = traj.Ex$aban.counts.ex,
           decline.ex=decline.ex, decline.un=decline.un, decline.steep.ex=decline.steep.ex, decline.steep.un=decline.steep.un,
           growth.ex=growth.ex, growth.un=growth.un, growth.steep.ex=growth.steep.ex, growth.steep.un=growth.steep.un,
           lambda.plot=lambda.plot)
      
    })#withProgress
    
  })#trajectory
  
  lambda.summary <- reactiveValues(lambda.ex=NA, lambda.un=NA)

 observeEvent(input$analyze.go, {
    if(input$analyze.go) {
      lambda.summary$lambda.un <- trajectory()$lambda.un; lambda.summary$lambda.ex <- trajectory()$lambda.ex

    }
  })
 
  ################################################################################################### lamdba
 
#THRESHOLDS####
 #Abandonment####
  n.vals <- 200 #number of aban values to draw for use in simulations
  ni<-200 #number of iterations per draw
  thresholds<-eventReactive(input$threshold.data, {
    withProgress(message = "Calculating Abandonment Thresholds...", {
      #draw abandonment values
      beta.a.vec <- runif(n.vals,-2,4)
      beta.a.vec <- beta.a.vec[sort.list(beta.a.vec)] #sort from low to high; makes plotting later easier
      #define stuff
      lambda.mat.ex <- lambda.mat.un <- abans.ex <- matrix(NA, nrow=ni, ncol=n.vals)
      
      #run projection models
      for(i in 1:n.vals){ 
        growth.ex <- lambda.calc(parms=parms(), sd.parms=sd.parms(),n.ex=1, 
                                 n.iter=ni, beta.a.add=beta.a.vec[i])
        lambda.mat.ex[,i] <- growth.ex$lambda
        abans.ex[,i] <- growth.ex$aban.counts.ex
        incProgress(1/n.vals) #progress bar for simulations 
      }
      #summarize data  
      ref.line <- lambda.calc(parms=parms(), sd.parms=sd.parms(),   
                              n.ex=0, n.iter=ni) #unexclosed reference
      aban.ex.means <- colMeans(abans.ex)
      prob.decline <- prob.decline.ref <- rep(NA, n.vals)
      prob.decline.ref <- mean(ref.line$lambda[]<1)
      for (i in 1:n.vals){
        prob.decline[i] <- length(which(lambda.mat.ex[,i]<1))/ni 
      }
      prob.curv <- smooth.spline(aban.ex.means, prob.decline)
      diffs <- abs(prob.curv$y-prob.decline.ref)
      aban.tolerance <- round(prob.curv$x[min(which(diffs[]==min(diffs)))],0)
      #package data for output  
      list(prob.decline=prob.decline, prob.decline.ref=prob.decline.ref, aban.ex.means=aban.ex.means, prob.curv=prob.curv,
           aban.tolerance=aban.tolerance)
    }) #thresholds withProgress
  }) #thresholds 
  
  #Predation####
  pred.thresholds<-eventReactive(input$threshold.pred.data, {
    withProgress(message = "Calculating Predation Thresholds...", {
      #draw abandonment values
      eta.p.vec <- runif(n.vals,-2,2)
      eta.p.vec <- eta.p.vec[sort.list(eta.p.vec)] #sort from low to high; makes plotting later easier
      #define stuff
      lambda.mat.ex <- lambda.mat.un <- preds.un <- matrix(NA, nrow=ni, ncol=n.vals)
      
      #run projection models
      for(i in 1:n.vals){ 
        growth.un <- lambda.calc(parms=parms(), sd.parms=sd.parms(), n.ex=0, 
                                 eta.p = eta.p.vec[i], n.iter=ni)
        lambda.mat.un[,i] <- growth.un$lambda
        preds.un[,i] <- growth.un$pred.counts.un
        incProgress(1/n.vals) #progress bar for simulations 
      }
      #summarize data  
      ref.line <- lambda.calc(parms=parms(), sd.parms=sd.parms(),   
                              n.ex=1, n.iter=ni) #exclosed reference
      preds.un.means <- colMeans(preds.un)
      prob.decline <- prob.decline.ref <- rep(NA, n.vals)
      prob.decline.ref <- mean(ref.line$lambda[]<1)
      for (i in 1:n.vals){
        prob.decline[i] <- length(which(lambda.mat.un[,i]<1))/ni 
      }
      prob.curv <- smooth.spline(preds.un.means, prob.decline)
      diffs <- abs(prob.curv$y-prob.decline.ref)
      pred.tolerance <- round(prob.curv$x[min(which(diffs[]==min(diffs)))],0)
      #package data for output  
      list(prob.decline=prob.decline, prob.decline.ref=prob.decline.ref, preds.un.means=preds.un.means, prob.curv=prob.curv,
           pred.tolerance=pred.tolerance)
    }) #thresholds withProgress
  }) #thresholds.pred 
#########################################################################################################

thresh.aban.summary <- reactiveValues(prob.decline=NA, prob.decline.ref=NA, aban.ex.means=NA, prob.curv=NA,aban.tolerance=NA)  
thresh.pred.summary <- reactiveValues(prob.decline=NA, prob.decline.ref=NA, preds.un.means=NA, prob.curv=NA,pred.tolerance=NA)  
  
observeEvent(input$threshold.data, {
  if(input$threshold.data) {
    thresh.aban.summary$prob.decline <- thresholds()$prob.decline; thresh.aban.summary$prob.decline.ref <- thresholds()$prob.decline.ref;
    thresh.aban.summary$aban.ex.means <- thresholds()$aban.ex.means; thresh.aban.summary$prob.curv <- thresholds()$prob.curv;
    thresh.aban.summary$aban.tolerance <- thresholds()$aban.tolerance
    

  }
}) 

observeEvent(input$threshold.pred.data,{
  if(input$threshold.pred.data){
    thresh.pred.summary$prob.decline <- pred.thresholds()$prob.decline; 
    thresh.pred.summary$prob.decline.ref <- pred.thresholds()$prob.decline.ref;
    thresh.pred.summary$preds.un.means <- pred.thresholds()$preds.un.means; 
    thresh.pred.summary$prob.curv <- pred.thresholds()$prob.curv;
    thresh.pred.summary$pred.tolerance <- pred.thresholds()$pred.tolerance
  }
})
    
#SCENARIO MODELING####

  
#back-transform inputs to get alpha.p and beta.a.ex values
back.calc <- function(x){
  alpha.a <- mean.parms$alpha.a
  alpha.p <- x[2]
  alpha.f <- mean.parms$alpha.f
  beta.a.ex <- x[1]
  beta.p.ex <- mean.parms$beta.p.ex
  a <- x[3]
  p <- x[4]
  y <- rep(NA,2)
  #linear predictors:
  linp.ex <- exp(alpha.p + beta.p.ex)
  linp <- exp(alpha.p)
  linf <- exp(alpha.f)
  lina.ex <- exp(alpha.a + beta.a.ex)
  lina <- exp(alpha.a)
  #denominator
  den.ex <- 1 + linp.ex + linf + lina.ex
  den <- 1 + linp + linf + lina
  surv.ex <- 1/(den.ex) #daily survival probability for exclosed nests
  surv <- 1/den #daily survival probability for unexclosed nests
  #solve for beta.a.ex given a, with y[1] set to 0:
  y[1] <- ((lina.ex/den.ex)/(1-surv.ex))*(1-surv.ex^34)-a 
  #take equation p = (stuff) and substract p (make y[2] 0)
  y[2] <- ((linp/den)/(1-surv))*(1-surv^34)-p
  y[3] <- a-a
  y[4] <- p-p
  y
}

#supply 1 and -4 as initial parameter value guesses
parm.est <- reactive({
  list(
  beta.a.ex = dfsane(c(1,-4,input$abanRisk/100,input$predRisk/100),back.calc)$par[1],
  alpha.p = dfsane(c(1,-4,input$abanRisk/100,input$predRisk/100),back.calc)$par[2]
  )})


parms.scen <- reactive({
  list(
    Na0 = (input$ScenPairs)/2,
    Ns0 = (input$ScenPairs)/2,
    yt=0.99,
    ys=0.68,
    Phij=0.52,  
    Phia=0.74,
    f=input$fledge/100,
    m=input$mortality/200,
    E = 0.94,
    r2 = 0.7,
    r3 = 0.7,
    beta.a.ex = parm.est()$beta.a.ex,
    beta.p.ex = mean.parms$beta.p.ex,
    alpha.a = mean.parms$alpha.a,
    alpha.p = parm.est()$alpha.p,
    alpha.f = mean.parms$alpha.f
  )})

#back-transform inputs to get alpha.p and beta.a.ex values
back.calc.import <- function(x){
  alpha.a <- survival()$alpha.a
  alpha.p <- x[2]
  alpha.f <- survival()$alpha.f
  beta.a.ex <- x[1]
  beta.p.ex <- survival()$beta.p.ex
  a <- x[3]
  p <- x[4]
  y <- rep(NA,2)
  #linear predictors:
  linp.ex <- exp(alpha.p + beta.p.ex)
  linp <- exp(alpha.p)
  linf <- exp(alpha.f)
  lina.ex <- exp(alpha.a + beta.a.ex)
  lina <- exp(alpha.a)
  #denominator
  den.ex <- 1 + linp.ex + linf + lina.ex
  den <- 1 + linp + linf + lina
  surv.ex <- 1/(den.ex) #daily survival probability for exclosed nests
  surv <- 1/den #daily survival probability for unexclosed nests
  #solve for beta.a.ex given a, with y[1] set to 0:
  y[1] <- ((lina.ex/den.ex)/(1-surv.ex))*(1-surv.ex^34)-a 
  #take equation p = (stuff) and substract p (make y[2] 0)
  y[2] <- ((linp/den)/(1-surv))*(1-surv^34)-p
  y[3] <- a-a
  y[4] <- p-p
  y
}

parm.est.import <- reactive({
  list(
    beta.a.ex = dfsane(c(1,-4,input$abanRisk/100,input$predRisk/100),back.calc.import)$par[1],
    alpha.p = dfsane(c(1,-4,input$abanRisk/100,input$predRisk/100),back.calc.import)$par[2]
  )})


parms.import <- reactive({
  list(
    Na0 = (input$ScenPairs)/2,
    Ns0 = (input$ScenPairs)/2,
    yt=0.99,
    ys=0.68,
    Phij=0.52,  
    Phia=0.74,
    f=input$fledge/100,
    m=input$mortality/200,
    E = 0.94,
    r2 = 0.7,
    r3 = 0.7,
    beta.a.ex = parm.est.import()$beta.a.ex,
    beta.p.ex = survival()$beta.p.ex,
    alpha.a = survival()$alpha.a,
    alpha.p = parm.est.import()$alpha.p,
    alpha.f = survival()$alpha.f
  )})

sd.parms.scen <- reactive({
  list(
    vcovSurv=matrix(c((logodds(parms.scen()$Phij)*CV)^2,logodds(parms.scen()$Phij)*CV*logodds(parms.scen()$Phia)*CV, 
                      logodds(parms.scen()$Phij)*CV*logodds(parms.scen()$Phia)*CV, (logodds(parms.scen()$Phia)*CV))^2,2,2),
    sd.f = abs(logodds(parms.scen()$f))*0.06,
    yt = logodds(parms.scen()$yt)*CV,
    ys = logodds(parms.scen()$ys)*CV,
    r2 = abs(logodds(parms.scen()$r2))*CV,
    r3 = abs(logodds(parms.scen()$r3))*CV,
    m = min(abs(logodds(parms.scen()$m))*1.04,abs(logodds(parms.scen()$m+0.01))*1.04),  #use 0.01 if value of 0 entered
    alpha.f = sd.parms.init$alpha.f,
    vcovAban = sd.parms.init$vcovAban,
    vcovPred = sd.parms.init$vcovPred
  )})

sd.parms.import <- reactive({
  list(
    vcovSurv=matrix(c((logodds(parms.import()$Phij)*CV)^2,logodds(parms.import()$Phij)*CV*logodds(parms.import()$Phia)*CV, 
                      logodds(parms.import()$Phij)*CV*logodds(parms.import()$Phia)*CV, (logodds(parms.import()$Phia)*CV))^2,2,2),
    sd.f = abs(logodds(parms.import()$f))*0.06,
    yt = logodds(parms.import()$yt)*CV,
    ys = logodds(parms.import()$ys)*CV,
    r2 = abs(logodds(parms.import()$r2))*CV,
    r3 = abs(logodds(parms.import()$r3))*CV,
    m = min(abs(logodds(parms.import()$m))*1.04,abs(logodds(parms.import()$m+0.01))*1.04),  #use 0.01 if value of 0 entered
    alpha.f = sd.parms.init$alpha.f,
    vcovAban = sd.parms.init$vcovAban,
    vcovPred = sd.parms.init$vcovPred
  )})

#scenario modeling function  
  scenario <- eventReactive(input$scenario,
                            {withProgress(message = "Calculating ..",{
                         #if values imported, import all parameters, not just aban.ex and pred.un
                              if(input$import){
                                validate(
                                  need(!is.null(nestdata$nestdata), "Warning: No Data File Detected")
                                                                  )
                                traj.noEx <- lambda.calc(parms=parms.import(), sd.parms=sd.parms.import(), n.ex=0, n.iter=1000)
                                traj.Ex <- lambda.calc(parms=parms.import(), sd.parms=sd.parms.import(), n.ex=1, n.iter=1000)
                                
                              } else {
                                traj.noEx <- lambda.calc(parms=parms.scen(), sd.parms=sd.parms.scen(), n.ex=0, n.iter=1000)
                                traj.Ex <- lambda.calc(parms=parms.scen(), sd.parms=sd.parms.scen(), n.ex=1, n.iter=1000)
                                
                              }
                                
                              #calculate probabilities of decline and growth
                              decline.ex <- mean(traj.Ex$lambda<1)
                              decline.un <- mean(traj.noEx$lambda<1)
                              
                              decline.steep.ex <- mean(traj.Ex$lambda<0.95)
                              decline.steep.un <- mean(traj.noEx$lambda<0.95)
                              
                              growth.ex <- mean(traj.Ex$lambda>1)
                              growth.un <- mean(traj.noEx$lambda>1)
                              
                              growth.steep.ex <-mean(traj.Ex$lambda>1.05)
                              growth.steep.un <-mean(traj.noEx$lambda>1.05)
                              
                              #prepare output
                              lambda.plot <- data.frame(
                                Growth = c(traj.Ex$lambda[traj.Ex$lambda<2], traj.noEx$lambda[traj.noEx$lambda<2]), #remove extremes
                                Exclosures = c(rep("Yes", length(traj.Ex$lambda[traj.Ex$lambda<2])), rep("No", length(traj.noEx$lambda[traj.noEx$lambda<2])))
                              )
                              
                              #reorder factor level Yes/No for plotting
                              lambda.plot$Exclosures <- factor(lambda.plot$Exclosures, levels(lambda.plot$Exclosures)[c(2,1)])
                              
                              
                              list(lambda.un=traj.noEx$lambda, lambda.ex=traj.Ex$lambda, decline.ex=decline.ex, 
                                   decline.un=decline.un, decline.steep.ex=decline.steep.ex, decline.steep.un=decline.steep.un,
                                   growth.ex=growth.ex, growth.un=growth.un, growth.steep.ex=growth.steep.ex, 
                                   growth.steep.un=growth.steep.un, parm.est=parm.est, lambda.plot=lambda.plot)
                            })
                        }#with progress
  )#event reactive
  

####################################
  
  #THRESHOLDS FOR SCENARIO####
  parms.scen.t <- reactive({
    list(
      Na0 = (input$ScenPairs)/2,
      Ns0 = (input$ScenPairs)/2,
      yt=0.99,  #probability of third-year bird nesting
      ys=0.68,  #probability of second-year bird nesting
      Phij=0.52,  
      Phia=0.74,
      f=input$fledge/100,
      m=input$mortality/200,
      E = 0.94,
      r2 = 0.7,
      r3 = 0.7,
      beta.a.ex = parm.est()$beta.a.ex,
      beta.p.ex = mean.parms$beta.p.ex,
      alpha.a = mean.parms$alpha.a,
      alpha.p = parm.est()$alpha.p,
      alpha.f = mean.parms$alpha.f
    ) })
  
  sd.parms.scen.t <- reactive({
    list(
      vcovSurv=matrix(c((logodds(parms.scen.t()$Phij)*CV)^2,logodds(parms.scen.t()$Phij)*CV*logodds(parms.scen.t()$Phia)*CV, 
                        logodds(parms.scen.t()$Phij)*CV*logodds(parms.scen.t()$Phia)*CV, (logodds(parms.scen.t()$Phia)*CV))^2,2,2),  #had sd in here earlier, not variance
      sd.f = abs(logodds(parms.scen.t()$f))*0.06,
      yt = logodds(parms.scen.t()$yt)*CV,
      ys = logodds(parms.scen.t()$ys)*CV,
      r2 = abs(logodds(parms.scen.t()$r2))*CV,
      r3 = abs(logodds(parms.scen.t()$r3))*CV,
      m = min(abs(logodds(parms.scen.t()$m))*1.04,abs(logodds(parms.scen.t()$m+0.01))*1.04),  #use 0.01 if value of 0 entered
      alpha.f = sd.parms.init$alpha.f,
      vcovAban = sd.parms.init$vcovAban,
      vcovPred = sd.parms.init$vcovPred
    ) })
  
  scen.thresholds<-eventReactive(input$Scen.threshold.data, {
    withProgress(message = "Calculating Abandonment Thresholds...", {
      
      n.vals <- 200 #number of aban values to draw for use in simulations
      ni<-200 #number of iterations per draw
      #draw abandonment values
      beta.a.vec <- runif(n.vals,0,4)
      beta.a.vec <- beta.a.vec[sort.list(beta.a.vec)] #sort from low to high; makes plotting later easier
      #define stuff
      lambda.mat.ex <- lambda.mat.un <- abans.ex <- matrix(NA, nrow=ni, ncol=n.vals)
      
      #run projection models
      for(i in 1:n.vals){ 
        growth.ex <- lambda.calc(parms=parms.scen.t(), sd.parms=sd.parms.scen.t(), n.ex=1, 
                                 n.iter=ni, beta.a.add=beta.a.vec[i])
        lambda.mat.ex[,i] <- growth.ex$lambda
        abans.ex[,i] <- growth.ex$aban.counts.ex
        incProgress(1/n.vals) #progress bar for simulations 
      }
      #summarize data  
      ref.line <- lambda.calc(parms=parms.scen.t(), sd.parms=sd.parms.scen.t(),   
                              n.ex=0, n.iter=ni) #unexclosed reference
      aban.ex.means <- colMeans(abans.ex)
      prob.decline <- prob.decline.ref <- rep(NA, n.vals)
      prob.decline.ref <- mean(ref.line$lambda[]<1)
      for (i in 1:n.vals){
        prob.decline[i] <- length(which(lambda.mat.ex[,i]<1))/ni 
      }
      prob.curv <- smooth.spline(aban.ex.means, prob.decline)
      diffs <- abs(prob.curv$y-prob.decline.ref)
      aban.tolerance <- round(prob.curv$x[min(which(diffs[]==min(diffs)))],0)

      #package data for output  
      list(prob.decline=prob.decline, prob.decline.ref=prob.decline.ref, aban.ex.means=aban.ex.means, prob.curv=prob.curv,
           aban.tolerance=aban.tolerance)
    }) #scen.thresholds withProgress
  }) #scen.thresholds - abandonment
  

#Scenario - Predation Threshold####
  scen.pred.thresholds<-eventReactive(input$scen.threshold.pred.data, {
    withProgress(message = "Calculating Predation Thresholds...", {
      n.vals <- 200 #number of aban values to draw for use in simulations
      ni<-200 #number of iterations per draw
      
      #draw values
      eta.p.vec <- runif(n.vals,-2,2)
      eta.p.vec <- eta.p.vec[sort.list(eta.p.vec)] #sort from low to high; makes plotting later easier
      #define stuff
      lambda.mat.ex <- lambda.mat.un <- preds.un <- matrix(NA, nrow=ni, ncol=n.vals)
      
      #run projection models
      for(i in 1:n.vals){ 
        growth.un <- lambda.calc(parms=parms.scen.t(), sd.parms=sd.parms.scen.t(), n.ex=0, 
                                 eta.p = eta.p.vec[i], n.iter=ni)
        lambda.mat.un[,i] <- growth.un$lambda
        preds.un[,i] <- growth.un$pred.counts.un
        incProgress(1/n.vals) #progress bar for simulations 
      }
      #summarize data  
      ref.line <- lambda.calc(parms=parms.scen.t(), sd.parms=sd.parms.scen.t(),   
                              n.ex=1, n.iter=ni) #exclosed reference
      preds.un.means <- colMeans(preds.un)
      prob.decline <- prob.decline.ref <- rep(NA, n.vals)
      prob.decline.ref <- mean(ref.line$lambda[]<1)
      for (i in 1:n.vals){
        prob.decline[i] <- length(which(lambda.mat.un[,i]<1))/ni 
      }
      prob.curv <- smooth.spline(preds.un.means, prob.decline)
      diffs <- abs(prob.curv$y-prob.decline.ref)
      pred.tolerance <- round(prob.curv$x[min(which(diffs[]==min(diffs)))],0)
      #package data for output  
      list(prob.decline=prob.decline, prob.decline.ref=prob.decline.ref, preds.un.means=preds.un.means, prob.curv=prob.curv,
           pred.tolerance=pred.tolerance)
    }) #thresholds withProgress
  }) #scen.thresholds.pred 
  
 
  #########################################################################################################
#Scenario thresholds summaries####
  
  scen.thresh.summary <- reactiveValues(prob.decline=NA, prob.decline.ref=NA, 
                                        aban.ex.means=NA, prob.curv=NA,aban.tolerance=NA)
  scen.thresh.pred.summary <- reactiveValues(prob.decline=NA, prob.decline.ref=NA, preds.un.means=NA, 
                                             prob.curv=NA,pred.tolerance=NA)  
  
  observeEvent(input$Scen.threshold.data, {
    if(input$Scen.threshold.data) {
      scen.thresh.summary$prob.decline <- scen.thresholds()$prob.decline; scen.thresh.summary$prob.decline.ref <- scen.thresholds()$prob.decline.ref;
      scen.thresh.summary$aban.ex.means <- scen.thresholds()$aban.ex.means; scen.thresh.summary$prob.curv <- scen.thresholds()$prob.curv;
      scen.thresh.summary$aban.tolerance <- scen.thresholds()$aban.tolerance
    }
  })  
  
  observeEvent(input$scen.threshold.pred.data,{
    if(input$scen.threshold.pred.data){
      scen.thresh.pred.summary$prob.decline <- scen.pred.thresholds()$prob.decline; 
      scen.thresh.pred.summary$prob.decline.ref <- scen.pred.thresholds()$prob.decline.ref;
      scen.thresh.pred.summary$preds.un.means <- scen.pred.thresholds()$preds.un.means; 
      scen.thresh.pred.summary$prob.curv <- scen.pred.thresholds()$prob.curv;
      scen.thresh.pred.summary$pred.tolerance <- scen.pred.thresholds()$pred.tolerance
    }
  })
    
###############################################################################################################
  
#PACKAGE OUTPUT#### 
  #example data download####
  #note; this feature does not work in Rstudio window, must open in browser first
  output$example <- downloadHandler(
    filename = "exampleData.csv",
    content = function(file) {
      write.csv(exampleDat, file, row.names=F)
    }
  )

  #download cleaned data####  
  output$downloadClean <- downloadHandler(
    filename = "cleanedData.csv",
    content = function(file){
      write.csv(nestdata$nestdata, file, row.names=F)
    }
  )
  
  #input summary####
  input.summary<- reactive({
    if(is.null(nestdata$nestdata)) {nest.count <- 0; hatches <- 0; preds <- 0; abans <- 0; floods <- 0;
                        uknfail <- 0; uknfate <- 0; otherfail <- 0}
    else {nest.count<-length(unique(as.factor(nestdata$nestdata$Nest.ID))) 
    
    hatches <- sum(aggregate(nestdata$nestdata$Status, list(nestdata$nestdata$Nest.ID),max)$x==6)
    preds <- sum(nestdata$nestdata$Status==2, na.rm=T)
    abans <- sum(nestdata$nestdata$Status==4, na.rm=T)
    floods <- sum(nestdata$nestdata$Status==3, na.rm=T)
    uknfail <- sum(nestdata$nestdata$Status==5, na.rm=T)
    uknfate <- sum(nestdata$nestdata$Status==7, na.rm=T)
    otherfail <- sum(nestdata$nestdata$Status==8, na.rm=T)}
    input.summary <- as.data.frame(cbind(nest.count,  hatches, preds, abans, floods, uknfail, uknfate, otherfail))
    input.summary
  })

  output$nests<-renderText({paste("You have entered data for ", input.summary()$nest.count, "nests")})
  output$hatches<-renderText({paste("Number of hatches = ", input.summary()$hatches)})  
  output$preds<-renderText({paste("Number of depredations = ", input.summary()$preds)})
  output$abans<-renderText({paste("Number of abandonments = ", input.summary()$abans)})
  output$floods<-renderText({paste("Number of tidal/weather-caused failures = ", input.summary()$floods)})
  output$uknfail<-renderText({paste("Number of failures due to unknown cause = ", input.summary()$uknfail)})
  output$uknfate<-renderText({paste("Number of unknown fates = ", input.summary()$uknfate)})
  output$otherfail<-renderText({paste("Number of failures from other causes =", input.summary()$otherfail)})
  
#editable data table####
  output$hot <- renderRHandsontable({
    DF = nestdata$nestdata
    if (!is.null(DF)) 
      rhandsontable(DF, rowheaders = NULL, height = 900, width = 1000) %>%
     hot_col(col="Nest.ID", type="dropdown", source=unique(nestdata$nestdata$Nest.ID), strict=F, allowInvalid=T) %>%
      hot_col(col="Exclosed",type="dropdown", source=c("Y","N"),allowInvalid=F) %>%
      hot_col(col="Status", type="dropdown", source=c("1","2","3","4","5","6","7","8"), allowInvalid=F) %>%
      hot_col(col='Interval', readOnly=T) %>%
      hot_cols(columnSorting=T) %>%
      hot_context_menu(allowRowEdit = T, allowColEdit=F)  #allows user to add or delete rows
    
  })
  
#Bayesian nest fate analysis summary output####  
  output$hatch.ex<-renderText({paste(round(survival()$Fate.est$Hatch.Ex, digits=2)*100,"(",
                                     round(survival()$Fate.SD$Hatch.Ex, 2)*100, ") %")})
  output$hatch.un<-renderText({paste(round(survival()$Fate.est$Hatch.Un,2)*100, "(",
                                    round(survival()$Fate.SD$Hatch.Un,2)*100,") %")})
  output$aban.ex<-renderText({paste(round(survival()$Fate.est$Aban.Ex,2)*100, "(",
                                    round(survival()$Fate.SD$Aban.Ex, 2)*100, ") %")})
  output$aban.un<-renderText({paste(round(survival()$Fate.est$Aban.Un,2)*100,"(",
                                    round(survival()$Fate.SD$Aban.Un, 2)*100, ") %")})
  output$pred.ex<-renderText({paste(round(survival()$Fate.est$Pred.Ex,2)*100,"(",
                                    round(survival()$Fate.SD$Pred.Ex, 2)*100, ") %")})
  output$pred.un<-renderText({paste(round(survival()$Fate.est$Pred.Un,2)*100,"(",
                                    round(survival()$Fate.SD$Pred.Un, 2)*100, ") %" )})
  output$flood.ex<-renderText({paste(round(survival()$Fate.est$Flood,2)*100,"(",
                                     round(survival()$Fate.SD$Flood, 2)*100, ") %")}) 
  output$flood.un<-renderText({paste(round(survival()$Fate.est$Flood,2)*100,"(",
                                     round(survival()$Fate.SD$Flood, 2)*100, ") %")}) #can't show same output in two places
  
  Fate.summary <- reactiveValues(
    mat = matrix(rep(NA,8), nrow=4, ncol = 2, byrow=T, dimnames = list(c("Hatch", "Predation", "Abandonment", "Tide/Weather"),
                                                          c("Exclosed", "Not Exclosed")))
  )
  

  observeEvent(input$analyze.go, {
    if(!is.null(input$analyze.go)) {
      Fate.summary$mat[1,1] <- paste(round(survival()$Fate.est$Hatch.Ex, digits=2)*100,"(",
                                 round(survival()$Fate.SD$Hatch.Ex, 2)*100, ") %")
      Fate.summary$mat[1,2] <- paste(round(survival()$Fate.est$Hatch.Un,2)*100, "(",
                                 round(survival()$Fate.SD$Hatch.Un,2)*100,") %")
      Fate.summary$mat[2,1] <- paste(round(survival()$Fate.est$Pred.Ex,2)*100,"(",
                                  round(survival()$Fate.SD$Pred.Ex, 2)*100, ") %")
      Fate.summary$mat[2,2] <- paste(round(survival()$Fate.est$Pred.Un,2)*100,"(",
                                 round(survival()$Fate.SD$Pred.Un, 2)*100, ") %" )
      Fate.summary$mat[3,1] <- paste(round(survival()$Fate.est$Aban.Ex,2)*100, "(",
                                 round(survival()$Fate.SD$Aban.Ex, 2)*100, ") %")
      Fate.summary$mat[3,2] <- paste(round(survival()$Fate.est$Aban.Un,2)*100,"(",
                                 round(survival()$Fate.SD$Aban.Un, 2)*100, ") %")
      Fate.summary$mat[4,1] <- paste(round(survival()$Fate.est$Flood,2)*100,"(",
                                                      round(survival()$Fate.SD$Flood, 2)*100, ") %")
      Fate.summary$mat[4,2] <- paste(round(survival()$Fate.est$Flood,2)*100,"(",
                                     round(survival()$Fate.SD$Flood, 2)*100, ") %")
    }
  })


  #lambda plot####

  output$lambdaplot<-renderPlot({
    ggplot(trajectory()$lambda.plot, aes(x=Exclosures, y=Growth)) +
      geom_violin(fill="grey") + 
      stat_summary(fun.y=mean, geom="point", shape=18, size=4) +
      theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14, face="bold")) +
      geom_hline(yintercept = 1)
  })
  
  #lambda probabilities####
  output$declineEx <- renderText({paste(round(trajectory()$decline.ex*100,0),"%")})
  output$declineUn <- renderText({ paste(round(trajectory()$decline.un*100,0), "%")})
  output$declineSteepEx <- renderText({paste(round(trajectory()$decline.steep.ex*100,0), "%")})
  output$declineSteepUn <- renderText({paste(round(trajectory()$decline.steep.un*100,0), "%")})
  output$growthEx <- renderText({paste(round(trajectory()$growth.ex*100,0), "%")})
  output$growthUn <- renderText({paste(round(trajectory()$growth.un*100,0), "%")})
  output$growthSteepEx <- renderText({paste(round(trajectory()$growth.steep.ex*100,0), "%")})
  output$growthSteepUn <- renderText({paste(round(trajectory()$growth.steep.un*100,0), "%")})
  
  traj.probs <- reactiveValues(
    mat = matrix(rep(NA,8), nrow=4, ncol = 2, byrow=T, 
                 dimnames = list(c("Rapid Growth", "Growth", "Decline", "Rapid Decline"), c("With Exclosures", "Without Exclosures")))
  )
  
  observeEvent(input$analyze.go, {
    if(input$analyze.go) {
      traj.probs$mat[1,1] <- paste(round(trajectory()$growth.steep.ex*100,0), "%")
      traj.probs$mat[1,2] <- paste(round(trajectory()$growth.steep.un*100,0), "%")
      traj.probs$mat[2,1] <- paste(round(trajectory()$growth.ex*100,0), "%")
      traj.probs$mat[2,2] <- paste(round(trajectory()$growth.un*100,0), "%")
      traj.probs$mat[3,1] <- paste(round(trajectory()$decline.ex*100,0),"%")
      traj.probs$mat[3,2] <- paste(round(trajectory()$decline.un*100,0), "%")
      traj.probs$mat[4,1] <- paste(round(trajectory()$decline.steep.ex*100,0), "%")
      traj.probs$mat[4,2] <- paste(round(trajectory()$decline.steep.un*100,0), "%")
    }
  })
  
  
  #thresholds plots####
  
  output$thresh.abans <- renderPlot({
    par(mar=c(5,5,4,2)+0.1)
    plot(x=NULL, y=NULL, xlab="Number of Abandonments",
         ylab="Probability of Population Decline", ylim=c(0,1), xlim=c(0, max(thresholds()$aban.ex.means)), 
         xaxt = "n")
    axis(1, at=seq(0,max(thresholds()$aban.ex.means),by=1))
    prob.curv <- smooth.spline(thresholds()$aban.ex.means, thresholds()$prob.decline)
    lines(thresholds()$prob.curv, lwd=2)
    if (length(thresholds()$prob.curv$y)>=n.vals) {y.coord <- thresholds()$prob.curv$y[1:200]} else 
    {y.coord <- c(thresholds()$prob.curv$y[1:(n.vals-length(thresholds()$prob.curv$y))], thresholds()$prob.curv)}
    polygon(c(thresholds()$aban.ex.means[1], thresholds()$aban.ex.means, thresholds()$aban.ex.means[n.vals]), 
            c(0,y.coord,0), density=10, angle=45)
    abline(h=thresholds()$prob.decline.ref, lty=2, lwd=2)
    par(xpd=T)
    legend("topright", c("Exclosed", "Unexclosed Reference"), xpd=T, lty=c(1,2), lwd=c(2,2), adj=c(0,NA)) 
    
  })
  
  output$reassess <- renderText({paste("Reassess or pull exclosures after ", thresholds()$aban.tolerance, 
                                       "observed nest abandonments")})
  
  output$thresh.preds <- renderPlot({
    par(mar=c(5,5,4,2)+0.1)
    plot(x=NULL, y=NULL, xlab="Number of Predations",
         ylab="Probability of Population Decline", ylim=c(0,1), xlim=c(0, max(pred.thresholds()$preds.un.means)), 
         xaxt = "n")
    axis(1, at=seq(0,max(pred.thresholds()$preds.un.means),by=1))
    prob.curv <- smooth.spline(pred.thresholds()$preds.un.means, pred.thresholds()$prob.decline)
    lines(pred.thresholds()$prob.curv, lwd=2)
    if (length(pred.thresholds()$prob.curv$y)>=n.vals) {y.coord <- pred.thresholds()$prob.curv$y[1:200]} else 
    {y.coord <- c(pred.thresholds()$prob.curv$y[1:(n.vals-length(pred.thresholds()$prob.curv$y))], pred.thresholds()$prob.curv)}
    polygon(c(pred.thresholds()$preds.un.means[1], pred.thresholds()$preds.un.means, pred.thresholds()$preds.un.means[n.vals]), 
            c(0,y.coord,0), density=10, angle=45)
    abline(h=pred.thresholds()$prob.decline.ref, lty=2, lwd=2)
    par(xpd=T)
    legend("topright", c("Unexclosed", "Exclosed Reference"), xpd=T, lty=c(1,2), lwd=c(2,2), adj=c(0,NA)) 
    
  })
  
  output$reassess.pred <- renderText({paste("Reassess or put up exclosures after ", pred.thresholds()$pred.tolerance, 
                                            "observed nest predations")})
  #scenario modeling output####
  #resettable slider values####
 
  start.val <- reactiveValues(Pred=36, Aban=6, M=70, f=40)
  
  observeEvent(input$import,{
    start.val$Pred <- survival()$Fate.est$Pred.Un*100
    start.val$Aban <- survival()$Fate.est$Aban.Ex*100
  })
  
  observeEvent(input$reset_input,{
    start.val$Pred <- 36
    start.val$Aban <- 6
    start.val$M <- 70
   start.val$f <- 40
  })
  
  output$resettableScenarioValues <- renderUI({
    div(id="values",
        sliderInput("predRisk", "Predation Probability without Exclosures",
                    min = 0, max = 99, value = start.val$Pred), 
        sliderInput("abanRisk", "Abandonment Probability with Exclosures", 
                    min = 0, max = 99, value = start.val$Aban),
        sliderInput("mortality", "Mortality Probability Given Abandonment", 
                    min = 0, max = 100, value = start.val$M),
        sliderInput("fledge","Chick Survival Probability (Hatch to Fledge)",
                    min=1, max=99, value=start.val$f) )
  })
  

  #lambda plot
  output$scenLambdaPlot<-renderPlot({
    ggplot(scenario()$lambda.plot, aes(x=Exclosures, y=Growth)) +
      geom_violin(fill="grey") + 
      stat_summary(fun.y=mean, geom="point", shape=18, size=4) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14, face="bold")) +
      geom_hline(yintercept = 1)
  })
  
  scen.summary <- reactiveValues(lambda.ex=NA, lambda.un=NA, Exclosures = NA, Growth = NA)

  observeEvent(input$scenario, {
    if(input$scenario) {
      scen.summary$lambda.un <- scenario()$lambda.un; scen.summary$lambda.ex <- scenario()$lambda.ex
    }
  })
  
  #growth probabilities
  output$SCENdeclineEx <- renderText({paste(round(scenario()$decline.ex*100,0),"%")})
  output$SCENdeclineUn <- renderText({ paste(round(scenario()$decline.un*100,0), "%")})
  output$SCENdeclineSteepEx <- renderText({paste(round(scenario()$decline.steep.ex*100,0), "%")})
  output$SCENdeclineSteepUn <- renderText({paste(round(scenario()$decline.steep.un*100,0), "%")})
  output$SCENgrowthEx <- renderText({paste(round(scenario()$growth.ex*100,0), "%")})
  output$SCENgrowthUn <- renderText({paste(round(scenario()$growth.un*100,0), "%")})
  output$SCENgrowthSteepEx <- renderText({paste(round(scenario()$growth.steep.ex*100,0), "%")})
  output$SCENgrowthSteepUn <- renderText({paste(round(scenario()$growth.steep.un*100,0), "%")})
  
  scen.traj.probs <- reactiveValues(
    mat = matrix(rep(NA,8), nrow=4, ncol = 2, byrow=T, dimnames = list(c("Rapid Growth", "Growth", 
                                                 "Decline", "Rapid Decline"), c("With Exclosures", "Without Exclosures")))
  )
  
  observeEvent(input$scenario, {
    if(input$scenario) {
      scen.traj.probs$mat[1,1] <- paste(round(scenario()$growth.steep.ex*100,0), "%")
      scen.traj.probs$mat[1,2] <- paste(round(scenario()$growth.steep.un*100,0), "%")
      scen.traj.probs$mat[2,1] <- paste(round(scenario()$growth.ex*100,0), "%")
      scen.traj.probs$mat[2,2] <- paste(round(scenario()$growth.un*100,0), "%")
      scen.traj.probs$mat[3,1] <- paste(round(scenario()$decline.ex*100,0),"%")
      scen.traj.probs$mat[3,2] <- paste(round(scenario()$decline.un*100,0), "%")
      scen.traj.probs$mat[4,1] <- paste(round(scenario()$decline.steep.ex*100,0), "%")
      scen.traj.probs$mat[4,2] <- paste(round(scenario()$decline.steep.un*100,0), "%")
    }
  })
  
  
  #scenario thresholds plots####
  
  output$scen.thresh.abans <- renderPlot({
    par(mar=c(5,5,4,2)+0.1)
    plot(x=NULL, y=NULL, xlab="Number of Abandonments",
         ylab="Probability of Population Decline", ylim=c(0,1), xlim=c(0, max(scen.thresholds()$aban.ex.means)), 
         xaxt = "n")
    axis(1, at=seq(0,max(scen.thresholds()$aban.ex.means),by=1))
    prob.curv <- smooth.spline(scen.thresholds()$aban.ex.means, scen.thresholds()$prob.decline)
    lines(scen.thresholds()$prob.curv, lwd=2)
    if (length(scen.thresholds()$prob.curv$y)>=n.vals) {y.coord <- scen.thresholds()$prob.curv$y[1:200]} else 
    {y.coord <- c(scen.thresholds()$prob.curv$y[1:(n.vals-length(scen.thresholds()$prob.curv$y))], scen.thresholds()$prob.curv)}
    polygon(c(scen.thresholds()$aban.ex.means[1], scen.thresholds()$aban.ex.means, scen.thresholds()$aban.ex.means[n.vals]), 
            c(0,y.coord,0), density=10, angle=45)
    abline(h=scen.thresholds()$prob.decline.ref, lty=2, lwd=2)
    par(xpd=T)
    legend("topright", c("Exclosed", "Unexclosed Reference"), xpd=T, lty=c(1,2), lwd=c(2,2), adj=c(0,NA)) 
    
  })
  
  output$SCENreassess <- renderText({paste("Reassess or pull exclosures after ", scen.thresholds()$aban.tolerance, 
                                       "observed nest abandonments")})
  
  output$scen.thresh.preds <- renderPlot({
    par(mar=c(5,5,4,2)+0.1)
    plot(x=NULL, y=NULL, xlab="Number of Predations",
         ylab="Probability of Population Decline", ylim=c(0,1), xlim=c(0, max(scen.pred.thresholds()$preds.un.means)), 
         xaxt = "n")
    axis(1, at=seq(0,max(scen.pred.thresholds()$preds.un.means),by=1))
    prob.curv <- smooth.spline(scen.pred.thresholds()$preds.un.means, scen.pred.thresholds()$prob.decline)
    lines(scen.pred.thresholds()$prob.curv, lwd=2)
    if (length(scen.pred.thresholds()$prob.curv$y)>=n.vals) {y.coord <- scen.pred.thresholds()$prob.curv$y[1:200]} else 
    {y.coord <- c(scen.pred.thresholds()$prob.curv$y[1:(n.vals-length(scen.pred.thresholds()$prob.curv$y))], scen.pred.thresholds()$prob.curv)}
    polygon(c(scen.pred.thresholds()$preds.un.means[1], scen.pred.thresholds()$preds.un.means, scen.pred.thresholds()$preds.un.means[n.vals]), 
            c(0,y.coord,0), density=10, angle=45)
    abline(h=scen.pred.thresholds()$prob.decline.ref, lty=2, lwd=2)
    par(xpd=T)
    legend("topright", c("Uxclosed", "Exclosed Reference"), xpd=T, lty=c(1,2), lwd=c(2,2), adj=c(0,NA)) 
    
  })
  
  output$scen.reassess.pred <- renderText({paste("Reassess or put up exclosures after ", scen.pred.thresholds()$pred.tolerance, 
                                                 "observed nest predations")})
  
#REPORT####
  output$Report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)
      
    #  if(input$NestSum) {sum.include = 1} else {sum.include = 0}
        summary<-as.data.frame(input.summary())
     # if(input$NestFate) {fate.include=1} else {fate.include=0}
        
      if(!is.na(lambda.summary$lambda.ex)) {plot.include=1} 
        else {plot.include=0}
      if(!is.na(lambda.summary$lambda.un)){
        lambda.export <- as.data.frame(trajectory()$lambda.plot)
      }
       
        
      if(!is.na(thresh.aban.summary$prob.curv)) {thresh.include=1} else {thresh.include=0}
      if(!is.na(thresh.pred.summary$prob.curv)) {thresh.pred.include=1} else {thresh.pred.include=0}
        
      params <- list(summary=summary, 
                     plot.include=plot.include, thresh.include=thresh.include, thresh.pred.include=thresh.pred.include,
                     thresholds=thresh.aban.summary, lambda.export=lambda.export,
                      Fate.summary=Fate.summary$mat,  traj.probs=traj.probs$mat, N0=input$Pairs,
                     pred.thresholds=thresh.pred.summary)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) #report
  
  output$ReportWord <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "ReportWord.Rmd")
      file.copy("ReportWord.Rmd", tempReport, overwrite = TRUE)
      
      #  if(input$NestSum) {sum.include = 1} else {sum.include = 0}
      summary<-as.data.frame(input.summary())
      # if(input$NestFate) {fate.include=1} else {fate.include=0}
      
      if(!is.na(lambda.summary$lambda.ex)) {plot.include=1} else {plot.include=0}
      if(!is.na(lambda.summary$lambda.un)){
        lambda.export <- as.data.frame(trajectory()$lambda.plot)
      }
      
      if(!is.na(thresh.aban.summary$prob.curv)) {thresh.include=1} else {thresh.include=0}
      if(!is.na(thresh.pred.summary$prob.curv)) {thresh.pred.include=1} else {thresh.pred.include=0}
      
      
      params <- list(summary=summary, lambda.export=lambda.export, plot.include=plot.include,
                      thresh.include=thresh.include, thresh.pred.include=thresh.pred.include,
                     thresholds=thresh.aban.summary, 
                     Fate.summary=Fate.summary$mat,  traj.probs=traj.probs$mat, N0=input$Pairs,
                     pred.thresholds=thresh.pred.summary)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) #report word
  
  output$ScenReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "ScenReport.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ScenReport.Rmd")
      file.copy("ScenReport.Rmd", tempReport, overwrite = TRUE)

      if(!is.na(scen.thresh.summary$prob.curv)) {scen.thresh.include=1} else {scen.thresh.include=0}
      if(!is.na(scen.thresh.pred.summary$prob.curv)) {thresh.pred.include=1} else {thresh.pred.include=0}
      
      if(!is.na(scen.summary$lambda.ex)) {scen.include=1} else {scen.include=0}
      
      if(!is.na(scen.summary$lambda.un)){
        lambda.export <- as.data.frame(scenario()$lambda.plot)
      }
      
      params <- list(aban.choose=input$abanRisk, fledge.choose=input$fledge,
                     pred.choose=input$predRisk, mort.choose = input$mortality, scen.include=scen.include,
                     scen.traj.probs=scen.traj.probs$mat, N0=input$ScenPairs,
                     thresh.include=scen.thresh.include, thresholds=scen.thresh.summary,
                     lambda.export=lambda.export, thresh.pred.include=thresh.pred.include, 
                     pred.thresholds = scen.thresh.pred.summary)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) #scenreport
  
  output$ScenReportWord <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "ScenReport.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ScenReportWord.Rmd")
      file.copy("ScenReportWord.Rmd", tempReport, overwrite = TRUE)
      
      if(!is.na(scen.thresh.summary$prob.curv)) {scen.thresh.include=1} else {scen.thresh.include=0}
      if(!is.na(scen.thresh.pred.summary$prob.curv)) {thresh.pred.include=1} else {thresh.pred.include=0}
      
      
      if(!is.na(scen.summary$lambda.ex)) {scen.include=1} else {scen.include=0}
      if(!is.na(scen.summary$lambda.un)){
        lambda.export <- as.data.frame(scenario()$lambda.plot)
      }
      
      params <- list(aban.choose=input$abanRisk, fledge.choose=input$fledge,
                     pred.choose=input$predRisk, mort.choose = input$mortality, scen.include=scen.include,
                     scen.traj.probs=scen.traj.probs$mat, N0=input$ScenPairs,
                     thresh.include=scen.thresh.include, thresholds=scen.thresh.summary,
                     lambda.export=lambda.export, thresh.pred.include=thresh.pred.include, 
                     pred.thresholds = scen.thresh.pred.summary
                     )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) #scenario report word
  
  
 }
)#shinyServer
