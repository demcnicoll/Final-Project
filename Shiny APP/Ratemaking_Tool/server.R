library(shiny)
library(ggplot2)
library(DBI)
library(tidyverse)
library(haven)
require(leaps)
library(gbm)
library(caret)
library(tidyr)
require(leaps)
library(gbm)
library(readr)

shinyServer(function(input, output, session) {

  #read in Premium and Loss Data
  load("C:/Users/demcn/Desktop/Ratemaking APP/MN & AL Loss Sample.Rdata")
  load("C:/Users/demcn/Desktop/Ratemaking APP/MN & AL Prem Sample.RData")
  load("C:/Users/demcn/Desktop/Ratemaking APP/stateData.RData")
  #read in model data

  na.omit(stateData)
  train <- sample(1:nrow(stateData), size = nrow(stateData)*0.8)
  test <- dplyr::setdiff(1:nrow(stateData), train)
  dataTrain <- stateData[train, ]
  dataTest <- stateData[test, ]
  trctrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 3)
  
  #set dependent variable.  In the future this would be dynamic
  depen <- "PD_Ultincurred"

  stateDataSave <- reactive({
  stateDataSave %>% (Rating_State == input$state)
})
  
  #title for first page
  output$title <- renderUI({ 
    text <- paste0("One-Way Analysis of Varible")
                   h1(text)
  })
  
ranges <- reactiveValues(x = NULL, y = NULL)

#create plot
output$varPlot <- renderPlot({
  #get filtered data
  variable <- input$var
  grp <- lapply(c("PolicyStateCode", "AY",input$var), as.symbol)
 
  varPremAY <- samplePrem %>% mutate(AY=as.numeric(substr(AccountingDate,1,4))) %>% 
    group_by_(.dots=grp) %>% summarize(EEdiv12 = sum(EEdiv12),EarnedPremium =sum(EarnedPremium))
  
  varLossAY <- lossData %>% group_by_(.dots=grp) %>% 
    summarize(IncPlusDCC = sum(IncPlusDCC), ClaimCount = sum(ClaimCount))
  
  AYMerge <- inner_join(varPremAY,varLossAY, by = c("PolicyStateCode","AY",input$var))
  varData <- AYMerge %>%
    group_by_(.dots=grp)%>%  mutate(PurePremium= IncPlusDCC/EEdiv12) %>% 
    mutate(Severity = IncPlusDCC/ClaimCount) %>% 
    mutate(Frequecy = ifelse(EEdiv12==0,0,ClaimCount/EEdiv12)) %>% 
    summarize("Incurred + DCC" = sum(IncPlusDCC), Exposure =
                sum(EEdiv12), Premium = sum(EarnedPremium), "Claim Count" = sum(ClaimCount),
              Severity = sum(Severity), Frequency = sum(Frequecy))
                   
  #create plot
ggplot(data=varData, aes(x=AY, y= Severity, group=get(input$var))) + 
                     geom_line(aes( color=get(input$var))) +
                     geom_point(aes(shape=get(input$var), color=get(input$var))) +
                     coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand=FALSE) +
                     theme(legend.position = "bottom")
                     })


observeEvent(input$varPlot_dblclick, {
  brush <- input$varPlot_brush
  if (!is.null(brush)) {
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
    
  } else {
    ranges$x <- NULL
    ranges$y <- NULL
  }
})


  output$tableSum <- renderTable({
    variable <- input$var
    grp <- lapply(c("PolicyStateCode", "AY",input$var), as.symbol)
    
    varPremAY <- samplePrem %>% mutate(AY=as.numeric(substr(AccountingDate,1,4))) %>% 
      group_by_(.dots=grp) %>% summarize(EEdiv12 = sum(EEdiv12),EarnedPremium =sum(EarnedPremium))
    
    varLossAY <- lossData %>% group_by_(.dots=grp) %>% 
      summarize(IncPlusDCC = sum(IncPlusDCC), ClaimCount = sum(ClaimCount))
    
    AYMerge <- inner_join(varPremAY,varLossAY, by = c("PolicyStateCode","AY",input$var))
    varData <- AYMerge %>%
      group_by_(.dots=grp)%>%  mutate(PurePremium= IncPlusDCC/EEdiv12) %>% 
      mutate(Severity = IncPlusDCC/ClaimCount) %>% 
      mutate(Frequecy = ifelse(EEdiv12==0,0,ClaimCount/EEdiv12)) %>% 
      summarize("Incurred + DCC" = sum(IncPlusDCC), Exposure =
                sum(EEdiv12), Premium = sum(EarnedPremium), "Claim Count" = sum(ClaimCount),
                Severity = sum(Severity), Frequency = sum(Frequecy))
    as.tibble(varData)
   
  })
  output$table <- DT::renderDataTable({
    stateData <- stateData %>% filter(Rating_State == input$state)
   DT::datatable(stateData)
                   })
  
  #output the inputed variables so the user can see what variables they have selected in the model
  output$out1 <- renderPrint(input$in1)

  output$glm <- renderPrint({
    
    modelOpt <- switch(input$modOps,
                       glmOp ="glm",
                       gbmOp="gbm")
    
    factors1 <- input$in1
    if(modelOpt == "glm"){
      mod <- glm(as.formula(paste(depen,"~", paste(factors1, collapse="+"))) ,
                family=gaussian, data=dataTrain)
      }
    if(modelOpt == "gbm"){
      #reduce data just for the purpose of making the app go fasted for grading
      stateData2 <- sample_n(stateData, 1000)
      mod <- train(as.formula(paste(depen,"~", paste(factors1, collapse="+"))),
                   data = stateData2, method = "gbm", na.action= na.omit,
                   preProcess = c("center", "scale"), verbose = FALSE)
    }
    summary(mod)

  })
  
  output$tableSum <- renderTable({
    variable <- input$var
    grp <- lapply(c("PolicyStateCode", "AY",input$var), as.symbol)
    
    varPremAY <- samplePrem %>% mutate(AY=as.numeric(substr(AccountingDate,1,4))) %>% 
      group_by_(.dots=grp) %>% summarize(EEdiv12 = sum(EEdiv12),EarnedPremium =sum(EarnedPremium))
    
    varLossAY <- lossData %>% group_by_(.dots=grp) %>% 
      summarize(IncPlusDCC = sum(IncPlusDCC), ClaimCount = sum(ClaimCount))
    
    AYMerge <- inner_join(varPremAY,varLossAY, by = c("PolicyStateCode","AY",input$var))
    varData <- AYMerge %>%
      group_by_(.dots=grp)%>%  mutate(PurePremium= IncPlusDCC/EEdiv12) %>% 
      mutate(Severity = IncPlusDCC/ClaimCount) %>% 
      mutate(Frequecy = ifelse(EEdiv12==0,0,ClaimCount/EEdiv12)) %>% 
      summarize("Incurred + DCC" = sum(IncPlusDCC), Exposure =
                  sum(EEdiv12), Premium = sum(EarnedPremium), "Claim Count" = sum(ClaimCount),
                Severity = sum(Severity), Frequency = sum(Frequecy))
    as.tibble(varData)
    
  })
  
  #output the variables so the user can see what they've selected
  output$out1 <- renderPrint(input$in1)
  
  #create a static cluster graph
  output$clusPlot <- renderPlot({
    
    stateData <- stateData %>% sample_n(400) %>% na.omit()

    frame <- stateData %>% select(Payment_Type,drvr_age) %>% data.frame()
    
    clust <- hclust(dist(frame))

    plot(clust, xlab="")
  })
  
  output$modPlot <- renderPlot({
    modelOpt <- switch(input$modOps,
                       glmOp ="glm",
                       gbmOp="gbm")
    
    factors1 <- input$in1
    if(modelOpt == "glm"){
      mod <- glm(as.formula(paste(depen,"~", paste(factors1, collapse="+"))) ,
                 family=gaussian, data=dataTrain)
    }
    if(modelOpt == "gbm"){
      #reduce data just for the purpose of making the app go fasted for grading

      stateData2 <- sample_n(stateData, 1000)
      mod <- train(as.formula(paste(depen,"~", paste(factors1, collapse="+"))),
                   data = stateData2, method = "gbm", na.action= na.omit,
                   preProcess = c("center", "scale"), verbose = FALSE)
    }
#draw a output plot from the model
    plot1<- plot(mod)
    print(plot1)
  })
  

  output$download <- downloadHandler(
    
    filename = function() {
      "stateData.csv"
    },
    content = function(file){
      write.csv(stateData, file)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot","png",sep=".")
    },
    content = function(file){
      png(file)
      plot1
      dev.off()
    }
  )
  #create text
  output$info <- renderText({
    paste("The predicted Incurred loss is:")
  })
  
  #output the prediction for a given policy
  output$polPred <- renderPrint({
    modelOpt <- switch(input$modOps,
                       glmOp ="glm",
                       gbmOp="gbm")
    
    factors1 <- input$in1
    if(modelOpt == "glm"){
      mod <- glm(as.formula(paste(depen,"~", paste(factors1, collapse="+"))) ,
                 family=gaussian, data=dataTrain)
    }
    if(modelOpt == "gbm"){
      #reduce data just for the purpose of making the app go fasted for grading

      stateData2 <- sample_n(stateData, 1000)
      mod <- train(as.formula(paste(depen,"~", paste(factors1, collapse="+"))),
                   data = stateData2, method = "gbm", na.action= na.omit,
                   preProcess = c("center", "scale"), verbose = FALSE)
    }
    
    pred<-data.frame(Membership_Type = input$membership,Payment_Type= input$payment,
                     drvr_marital_status = input$marital,NFA_CODE=input$nfa,
                     veh_collEMLC_RB_20= input$collLoc, veh_PDEMLC_RB_20 = input$pdLoc,
                     drvr_Student_Info = input$student,drvr_Education= input$education,
                     pol_multiple_product= input$multi,pol_tenure= input$tenure,
                     EQFXCCS_psps5078 = input$credit)
    
    data.frame(predict(mod,pred))
  })


output$appAbout <- renderText({
  paste("Welcome to Devyn's Ratemaking tool.  The main function of this tool is to assess Insurance
        data to help determine what variables are important when predicting future losses.  The data
        is randomized from a real insurer's dataset because it contains some confidential info.  The
        original data consists of 2 years of loss and premium data that also has the following policy 
        characteristics:
        Year, Membership Type, Payment Type, Count of No Fault Accidents, Location Score, Education Level, 
        Student Information, Tenure, and credit score.  From the data set we compute average Premium, 
        loss frequeny, severity, losses and loss ratio (Loss/Premium).
        
        The first tab allows the user to look at one variable at a time.  By changing the variable 
        selection in the panel, we can see the average loss severity for that variable by each level 
        for each of the years included in the data set.  In addition the graph has the functionality to 
        brush and zoom.

        The second tab shows a cluster analysis between two variables, for purposes of grading, the 
        inputs are not dynamic because it takes a while to load and other graphs are dynamic within
        the project.  A dendogram is provided to assess the clustering.
        
        The third tab is the modeling tab.  The user can pick as many variables as desired and see 
        the resulting GLM and GBM outputs as well as some basic graphs on the plot tab.  The table tab
        contains a sample of the data in question and can be filtered by state if the user desires.  
        Both the plot and table can be saved externally by the user by clicking the Download buttons in 
        the main panel.

        The fifth and final tab allows the user to select policy inputs to predict an overall loss amount 
        from that policy holder.
        
        Oh, and one more thing, click here for proof that we can use MathJax in Shiny!:")
})
output$mj <- renderUI({
  if (!input$question) return()
    withMathJax(
    helpText('\\(\\pi\\):
               $$\\frac2\\pi = \\frac{\\sqrt2}2\\cdot
             \\frac{\\sqrt{2+\\sqrt2}}2\\cdot
             \\frac{\\sqrt{2+\\sqrt{2+\\sqrt2}}}2\\cdots$$')
  )
})

})

