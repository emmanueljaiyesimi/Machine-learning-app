library(shiny)
library(pdfetch)
library(MASS)
library(sqldf)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(lubridate)
library(reshape2)
library(gridExtra)
library(gtable)
library(grid)
library(plyr)
library(qcc)
library(DT)
library(shinyjs)
library(gtools)
library(V8)
library(VIM)
library(corrplot)
library(tabplot)
library(e1071)
library(vcd)
library(caret)
library(caretEnsemble)
library(randomForest)
library(kernlab)
library(rpart)
library(glmnet)
library(xgboost)
library(gbm)


shinyServer(
  function(input, output, session) {
    RMSE <- 0
    
    values <- reactiveValues()
    
    myData <- reactive({
      
      switch(input$ds,
             file = {
               file1 <- input$datafile
               if (is.null(file1)){
                 return()
               }
               data = read.csv(file=file1$datapath)
             },
             url = {
               #setwd("D:\\Graduate Programs\\Appliet Stats and ML\\CA One")
               fileurl = input$link
               data = read.csv(fileurl, header=TRUE)
               #write.csv(data1, file="newFile.csv", row.names=FALSE)
               #data = read.csv("D:\\Graduate Programs\\Appliet Stats and ML\\CA One\\newFile.csv")
             },
             ib = {
               data = data.frame(get(input$ib))
             },
             yf = {
               ticker <-input$yf
               
               first.date <- Sys.Date() - 1*365
               last.date <- Sys.Date()
               
               freq.data <- '1d'
               
               
               out <- pdfetch_YAHOO (ticker, fields = c("open", "high", "low", "close", "volume"), from = first.date, to = last.date, interval= freq.data)
               stockdata_d <- data.frame(out)
               
               tick_open <- paste(ticker,sep = "", ".open")
               tick_high <- paste(ticker,sep = "", ".high")
               tick_low <- paste(ticker,sep = "", ".low")
               tick_volume <- paste(ticker,sep = "", ".volume")
               tick_close <- paste(ticker,sep = "", ".close")
               
               # remaining volume
               names(stockdata_d)[names(stockdata_d) == tick_open] <-"Open"
               names(stockdata_d)[names(stockdata_d) == tick_high] <-"High"
               names(stockdata_d)[names(stockdata_d) == tick_low] <-"Low"
               names(stockdata_d)[names(stockdata_d) == tick_volume] <-"Volume"
               names(stockdata_d)[names(stockdata_d) == tick_close] <-"Close"
               
               data <-na.omit(stockdata_d)
               
             }
      )
      return(data)
      
    })
    observe({
      updateSelectInput(session, "indvar",
                        choices = colnames(myData()))
      
      updateSelectInput(session, "tarvar",
                        choices = colnames(myData()))
      updateSelectInput(session, "attr",
                        choices = colnames(myData()))
    })
    
    output$extdata = DT::renderDataTable({
      
      DT::datatable(myData(), options = list(lengthChange = TRUE))
      
    })
    output$ExploreWay = renderUI({
      radioButtons("ExploreWays", label = "Explore",
                   choices = list("Summary" = 1, "Structure" = 2,"Missing"=3,"Correlation"=4,"Skewness"=5,"Kurtosis"=6), 
                   selected = 1)
    })
    
    
    
    
    
    output$Summary = renderPrint(if(input$Go){
      DF<-myData()
      if(isolate(input$ExploreWays)==1){
        summary(DF)
      }else if(isolate(input$ExploreWays)==2){
        str(DF)
      }else if(isolate(input$ExploreWays)==3){
        sort(colMeans(is.na(DF))[colMeans(is.na(DF))>0]*100,decreasing = T)
      }else if(isolate(input$ExploreWays)==4){
        round(cor(DF[,sapply(DF, is.numeric)], use="pairwise", method="pearson"),3)
      }else if(isolate(input$ExploreWays)==5){
        t<-sapply(DF,is.numeric)
        sort(sapply(DF[,t], skewness),decreasing = T)
      }else if(isolate(input$ExploreWays)==6){
        #sort(round(kurtosis(DF[,sapply(DF, is.numeric)]),3))
        t<-sapply(DF,is.numeric)
        sort(sapply(DF[,t], kurtosis),decreasing = T)
        
      }else{}
      
    })
    
    # Choose Histogram Parameter
    
    output$HistParams = renderUI(if(input$PlotType=="Histogram"){
      nums <- sapply(myData(), is.numeric)
      numericcols<-as.list(colnames(myData()[,nums]))
      selectInput(
        "HistParam", 
        label = "Plot Histogram",
        "",selectize=TRUE,multiple=FALSE,choices=numericcols
      )
    })
    # Choose BoxPlot Parameter
    
    output$BoxParams = renderUI(if(input$PlotType=="Box"){
      nums <- sapply(myData(), is.numeric)
      numericcols<-as.list(colnames(myData()[,nums]))
      selectInput(
        "BoxParam", 
        label = "Box Plot",
        "",selectize=TRUE,multiple=FALSE,choices=numericcols
      )
    })
    
    
    
    
    
    output$PlotTypes = renderUI({
      selectInput(
        "PlotType", 
        label = "Select Plot",
        "",selectize=TRUE,multiple=FALSE,choices=c("Histogram","Box")
      )
    })
    # Histogram
    output$Hist<-renderPlotly(if(input$PlotType=="Histogram"){
      DF<-myData()
      
      H <- hist(DF[,input$HistParam], plot = FALSE)
      minimum<-min(H$breaks,na.rm=TRUE)
      maximum<-max(H$breaks,na.rm=TRUE)
      step<-H$breaks[2]-H$breaks[1]
      
      ggplot(DF,aes_string(x=input$HistParam)) + 
        stat_bin(binwidth=step,colour="blue",fill="pink") +  
        stat_bin(binwidth=step, geom="text", aes(label=scales::percent((..count../sum(..count..)))), vjust=-1.5)+
        scale_x_continuous(breaks=seq(minimum,maximum, by=step))+theme_bw()
      
      
      
    })
    
    # Box Plot
    output$BoxPlot<-renderPlotly(if(input$PlotType=="Box"){
      DF<-myData()
      ggplot(DF,aes_string(x=input$GrByBoxs,y=input$BoxParam,fill=input$GrByBoxs)) +geom_boxplot()+theme_bw()
      
    })
    
    output$Plots<-renderUI({
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Processing is going on..Kindly wait")
      
      if(input$PlotType=="Box"){
        box(uiOutput("BoxParams"),
            uiOutput("GrByBox"),
            #fluidRow(column(6,actionButton("Go1", "Plot"))),
            plotlyOutput("BoxPlot",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }else if(input$PlotType=="Histogram"){
        box(uiOutput("HistParams"),
            #fluidRow(column(6,actionButton("Go1", "Plot"))),
            plotlyOutput("Hist",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
      }
      else {}
    })
    
    output$ProbPlot <- renderPlot({
      
      
      
      # binomial 
      if (input$dismodel == 'binomial') {
        x = table(input$attr)
        p = x/sum(x)
        par(mfrow=c(1,2)) 
        d <- density(rbinom(1000,input$n,p)) 
        plot(d, main="Kernel Density of generated data") 
        polygon(d, col="red", border="blue")
        x=0:input$n 
        plot(x,dbinom(x,input$n,p)) 
        
      }
      
      
      # poisson
      
      if (input$dismodel == 'poisson') {
        x = table(input$attr)
        lam = 1/mean(x)
        par(mfrow=c(1,2))  
        D=rpois(input$s, lam) 
        tab=table(D) 
        barplot(tab,col='blue') 
        x1=0:input$max 
        y1=dpois(x1,lam) 
        plot(x1,y1,type='b') 
      }
      ## 
      
      
      
      # geometric 
      if (input$dismodel == 'geometric') {
        x = table(input$attr)
        p = x/sum(x)
        par(mfrow=c(1,2))
        D=rgeom(input$s, p) 
        d <- density(D)
        
        plot(d, "Kernel Density")
        x2=0:input$max 
        y2=dgeom(x2,p) 
        plot(x2,y2,type='b') 
      }
      
      # bernoulli 
      if (input$dismodel == 'bernoulli') {
        x = table(input$attr)
        p = x/sum(x)
        par(mfrow=c(1,2))
        D=rbinom(input$s,1,p)
        d <- density(D)
        
        plot(d, "Kernel Density")
        tab=table(D) 
        barplot(tab,col='blue') 
        x2=0:input$max 
        y2=dbinom(x2,1,p) 
        plot(x2,y2,type='b') 
      }
      
      # multinomial 
      if (input$dismodel == 'multinomial') {
        x = table(input$attr)
        p <- table(x/sum(x))
        length(p)
        
        par(mfrow=c(1,2))
        
        D=rmultinom(input$s,input$n, p ) 
        tab=table(D)
        
        barplot(tab,col='blue') 
        x2=0:input$max
        
        length(x2)
        y2=dmultinom(x2,input$n,p) 
        plot(x2,y2,type='b') 
      }
      
      # hypergeometric 
      if (input$dismodel == 'hypergeometric') {
        par(mfrow=c(1,2))
        D=rhyper(input$s, input$k, input$N - input$k, input$n) 
        tab=table(D) 
        barplot(tab,col='blue') 
        x2=0:input$max 
        y2=dhyper(x2,input$k, input$N - input$k, input$n) 
        plot(x2,y2,type='b') 
      }
      
      
    })
    
    output$ModelSelection = renderUI({
      radioButtons("ModelSelections", label = "Select a Classification Model for your target variable",
                   choices = list("Poisson Regression" = 1, "Logistic Regression" = 2,"Naive Baiyes"=3), 
                   selected = 1)
    })
    
    output$glmperf <- renderPlot({
      if(isolate(input$ModelSelections)==1){      
        df <- na.omit(myData())
        TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
        colnames(TarIndData)= c(input$tarvar, input$indvar)      
        colnames(TarIndData)[1] <- "Y"
        set.seed(199)
        n=nrow(TarIndData)
        indexes = sample(n,n*(input$ratio/100))
        trainset = data.frame(TarIndData[indexes,])
        testset = data.frame(TarIndData[-indexes,])
        actual <- testset$Y
        pred_test <- data.frame(testset)      
        
        full.model.poisson <- glm(Y ~., data = trainset, family ='poisson')            
        values$full <- full.model.poisson      
        pred_full <-predict(full.model.poisson, testset[, input$indvar])      
        rmse_full = sqrt(sum((pred_full-actual)^2)/(nrow(testset)))      
        reduced.model= stepAIC(full.model.poisson)      
        values$full <- full.model.poisson      
        values$reduced <- reduced.model      
        pred_red = predict(reduced.model, testset[,input$indvar])      
        rmse_red = sqrt(sum((pred_red -actual)^2)/(nrow(testset)))      
        values$rmse <- data.frame('Full'=rmse_full, 'Reduce'=rmse_red)
        par(mfrow=c(1,2))      
        plot(actual, type = "o", col ="red", xlab = "Observations", ylab = input$tarvar,main = "FULL")
        lines(pred_full, type = "o", col = "blue")
        legend(
          "topleft",
          lty = c(1,1),
          col = c("red", "blue"),
          legend = c("Real", "Predicted")
        )
        plot(actual, type = "o", col ="red", xlab = "Observations", ylab = input$tarvar,
             main = "Reduced")
        lines(pred_red, type = "o", col = "blue")
        legend(
          "topleft",
          lty = c(1,1),
          col = c("red", "blue"),
          legend = c("Real", "Predicted")
          
        )
      }
      else if(isolate(input$ModelSelections)==2){        
        df <- na.omit(myData())
        TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
        colnames(TarIndData)= c(input$tarvar, input$indvar)        
        colnames(TarIndData)[1] <- "Y"
        set.seed(199)
        n=nrow(TarIndData)
        indexes = sample(n,n*(input$ratio/100))
        trainset = data.frame(TarIndData[indexes,])
        testset = data.frame(TarIndData[-indexes,])
        actual <- testset$Y
        pred_test <- data.frame(testset)
        full.model.binomial <- glm(Y ~., data = trainset, family ='binomial')  
        
        values$full <- full.model.binomial
        
        pred_fullBi <-predict(full.model.binomial, testset[, input$indvar], type='response')
        predictedvalues_fullBi=rep(0,length(pred_fullBi))
        predictedvalues_fullBi[pred_fullBi>0.5]=1                 
        accuracy_fullBi = mean(predictedvalues_fullBi == actual)      
        
        DF=data.frame(actual,predictedvalues_fullBi)
        
        
        
        confusion_matrix <- as.table(confusionMatrix( predictedvalues_fullBi, actualvalues=actual, nrow = 2, byrow = TRUE))
        fourfoldplot(confusion_matrix, color = c("#CC6666", "#99CC99"),
                     conf.level = 0, margin = 1, main = "Confusion Matrix")
        
        
        values$accuracyBi <- data.frame(1,accuracy_fullBi)
        
      }
      else if(isolate(input$ModelSelections)==3){
        
        df <- na.omit(myData())
        TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
        colnames(TarIndData)= c(input$tarvar, input$indvar)
        
        colnames(TarIndData)[1] <- "Y"
        set.seed(199)
        n=nrow(TarIndData)
        indexes = sample(n,n*(input$ratio/100))
        trainset = data.frame(TarIndData[indexes,])
        testset = data.frame(TarIndData[-indexes,])
        actual <- testset$Y
        pred_test <- data.frame(testset)      
        
        full.model.NB <-naiveBayes(Y ~., data = trainset)        
        values$full <- full.model.NB        
        pred_fullNB <-predict(full.model.NB, testset[, input$indvar], type='response')
        predictedvalues_fullNB=rep(0,length(pred_fullNB))
        predictedvalues_fullNB[pred_fullNB>0.5]=1        
        
        accuracy_fullNB = mean(predictedvalues_fullNB == actual)
        
        
        
        
        
        DF=data.frame(actual,predictedvalues_fullNB)
        
        
        confusion_matrix=table( predictedvalues_fullNB, actualvalues=actual, nrow = 2, byrow = TRUE)
        fourfoldplot(confusion_matrix, color = c("#CC6666", "#99CC99"),
                     conf.level = 0, margin = 1, main = "Confusion Matrix")
        
        values$full <- full.model.NB
        
        values$accuracyNB <- data.frame(1,accuracy_fullNB)
        
      }
      else{}
    })   
    
    output$selData = DT:: renderDataTable({
      df <- myData()
      TarIndData <- cbind(df[,input$tarvar], df[,input$indvar])
      colnames(TarIndData) = c(input$tarvar, input$indvar)
      
      DT::datatable(TarIndData, options = list(lengthChange = TRUE))
    })
    
    output$RMSE <- DT::renderDataTable({
      if(isolate(input$ModelSelections)==1){
        
        DT::datatable(values$rmse, options = list(lengthChange = TRUE))
      }
      else if(isolate(input$ModelSelections)==2){
        DT::datatable(values$accuracyBi, options = list(lengthChange = TRUE))
      }
      else if(isolate(input$ModelSelections)==3){
        DT::datatable(values$accuracyNB, options = list(lengthChange = TRUE))
      }else{}
    })
    
    output$Input_Ind <- renderUI({
      Var_count <- 0
      Var_count <- length(input$indvar)
      
      if(Var_count != 0){
        lapply(1:Var_count, function(i){
          numericInput(inputId = paste0 (inputId = input$indvar[i]),input$indvar[i], value=0)
        })
      }
    })
    
    forecast_out <- reactive({
      Var_Count <- length(input$indvar)
      
      new_data <- as.numeric(paste(lapply(1:Var_Count, function(i){
        inputName<- paste0(input$indvar[i])
        input[[inputName]]
      })))
      input_data <- data.frame(t(new_data))
      for (i in 1:Var_Count)
      {
        colnames(input_data)[i]<- input$indvar[i]
      }    
      
      new_predict_full <-predict(values$full,input_data)
      new_predict_red <-predict(values$reduced,input_data)
      
      pred_data_new <- data.frame(new_predict_full,new_predict_red)
      
      colnames(pred_data_new)[1] <- paste('Full Mode - ',input$tarvar)
      colnames(pred_data_new)[2] <- paste('Reduced Mode - ',input$tarvar)        
      
      return(pred_data_new)
    })
    
    forecast_outBi <- reactive({
      Var_Count <- length(input$indvar)
      
      new_data <- as.numeric(paste(lapply(1:Var_Count, function(i){
        inputName<- paste0(input$indvar[i])
        input[[inputName]]
      })))
      input_data <- data.frame(t(new_data))
      for (i in 1:Var_Count)
      {
        colnames(input_data)[i]<- input$indvar[i]
      }
      new_predict_bi <- predict(values$full,input_data)
      prediction_log <- data.frame(1, new_predict_bi) 
      colnames(prediction_log)[2] <- paste('Full Mode - ',input$tarvar)
      return(prediction_log)
    })
    
    
    output$Prediction <- DT::renderDataTable({
      DT::datatable(forecast_out(), options = list(lengthChange = TRUE))
    })
    
    output$PredictionLogistic <- DT::renderDataTable({
      
      DT::datatable(forecast_outBi(), options = list(lengthChange = TRUE))
    })
    
  }
)