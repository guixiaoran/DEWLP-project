library(shiny)
library(data.table)
library(ROCR)
library(randomForest)
library(ggplot2)
library(easycsv)
options(shiny.maxRequestSize=300*1024^2)


rf_model <- function(data){
  
  #data$RELIABILITY_TXT = as.factor(data$RELIABILITY_TXT)
  #Add pseudo absence data
  #newData = SMOTE(RELIABILITY_TXT ~ ., data,perc.over=100,perc.under = 50)
  newData=data
  #newData=data2 #using original data
  
  set.seed(28764838) 
  #Student ID as random seed   
  train.row = sample(1:nrow(newData), 0.7*nrow(newData))  
  species.train = newData[train.row,]  
  species.test = newData[-train.row,] 
  
  library(randomForest)
  species.rf <- randomForest(RELIABILITY_TXT~. , data =species.train, na.action = na.exclude)
  
  #species_predrf <- predict(species.rf, data2)
  # t3=table(Predicted_Class = species_predrf, Actual_Class = species.test$RELIABILITY_TXT)
   # cat("\n#Random Forest Confusion\n")
  # print(t3)
  return(species.rf)
  
  # species_pred.rf<- predict(species.rf, species.test)
  # species_Fpred <- prediction( species_pred.rf[,2], species.test$RELIABILITY_TXT)
  # species_Fperf <- performance(species_Fpred,"tpr","fpr")
  # return(species_Fperf)
  #plot(species_Fperf) #Plot Roc
  #abline(0,1)
  
  #Accuracy when pseudo absence added is 0.9625
  #Accuracy of original data is 0.8692
  #speciesperf.acc <- performance(species_Fpred,"auc")
  #print(as.numeric(speciesperf.acc@y.values)) 
}

rf_predict<- function(model,data){
  library(caret)
  species_predrf <- predict(model, data)
  result <- cbind(data,species_predrf )
  assign("Result_table",result,.GlobalEnv)
  return(result)
}

rf_plot <- function(data){
  library(dismo)
  coordinates(data) = c("LONGITUDEDD_NUM","LATITUDEDD_NUM")
  return(data)
}

buttonpresspred <- eventReactive(input$buttonleo, {
  data <- reactive_data2()
  cleaned <- leoclean(data)
  
  traindata = reactive_data()
  leomodel <- leorf_model(traindata)
  
  rf_output <- rf_predict(leomodel,cleaned)
  head(rf_output)
})

shinyServer(function(input,output){
  
  setwd(easycsv::choose_dir())
  vba_data<- read.csv("combinedPointValue.csv")
  vba_data<-vba_data[,c(3,5,8,17,26:39)]
  vba_data$RELIABILITY_TXT=ifelse(vba_data$RELIABILITY_TXT=="High reliability","High reliability","Low reliability")
  vba_data$RELIABILITY_TXT = as.factor(vba_data$RELIABILITY_TXT)
  model <- rf_model(vba_data)
  Result_table <- 1
  
  
  
  reactive_data <- reactive({
    print(input$file1$datapath)
    data <- read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote)
    data<-data[,c(3,5,8,17,26:39)]
    data$RELIABILITY_TXT=ifelse(data$RELIABILITY_TXT=="High reliability","High reliability","Low reliability")
    data$RELIABILITY_TXT = as.factor(data$RELIABILITY_TXT)
    return(data)

  })###################################
  
  #####################################
  
  
  #data2$RELIABILITY_TXT = as.factor(data2$RELIABILITY_TXT)
  #Add pseudo absence data
  #newData = SMOTE(RELIABILITY_TXT ~ ., data2,perc.over=300,perc.under = 50)
  
  #newData=data2 #using original data
  
  
  #####################################
  
  # output$graph <- renderTable({
  #   data <- reactive_data()
  # 
  #   input$Action
  #   plot <- isolate(train.row = sample(1:nrow(data), 0.7*nrow(data)),
  #           species.train = data[train.row,] ,
  #           species.test = data[-train.row,] ,
  #           species.rf <- randomForest(RELIABILITY_TXT~. , data =species.train, na.action = na.exclude),
  #           species_predrf <- predict(species.rf, species.test),
  #           t3=table(Predicted_Class = species_predrf, Actual_Class = species.test$RELIABILITY_TXT),
  #           species_pred.rf<- predict(species.rf, species.test, type="prob"),
  #           species_Fpred <- prediction( species_pred.rf[,2], species.test$RELIABILITY_TXT),
  #           species_Fperf <- performance(species_Fpred,"tpr","fpr"),
  #           return(species_Fperf)
  #   )
  # 
  # })
  
  
  # TPR_plot <- eventReactive(input$button,{
  #   data <- reactive_data()
  #   train.row = sample(1:nrow(data), 0.7*nrow(data))
  #   species.train = data[train.row,] 
  #   species.test = data[-train.row,] 
  #   species.rf <- randomForest(RELIABILITY_TXT~. , data =species.train, na.action = na.exclude)
  #   species_predrf <- predict(species.rf, species.test)
  #   t3=table(Predicted_Class = species_predrf, Actual_Class = species.test$RELIABILITY_TXT)
  #   species_pred.rf<- predict(species.rf, species.test, type="prob")
  #   species_Fpred <- prediction( species_pred.rf[,2], species.test$RELIABILITY_TXT)
  #   species_Fperf <- performance(species_Fpred,"tpr","fpr")
  #   return(species_Fperf)
  # })
  # 
  # output$graph <- renderPlot({
  #   input$button
  #   
  #   ROC <- rf_model(reactive_data)
  #   plot(ROC)
  # })
  
  output$contents <- renderTable({
    # load your data
    data <- reactive_data()
    
    
    if(input$disp == "head") {
      return(head(data))
    }
    else {
      return(data)
    }
  
  })###################################
  
  output$contents2 <- renderTable({
    
    
    input$predict_button
    isolate({
      Result_table <<- rf_predict(model,reactive_data())
      head(Result_table)
    
    })
    
  })###################################
  
  
  
  # output$contents3 <- renderTable({
  # 
  # 
  #   input$Plot_button2
  #   isolate(Result_table <-isolate(rf_predict(model,reactive_data())))
  # 
  # })###################################

  output$ggplot <- renderPlot({


    input$Plot_button
    isolate({

      Result_table<<-Result_table[which(Result_table$species_predrf == "High reliability"),]
      Graph_Plot <- rf_plot(Result_table)
      plot(Graph_Plot)})
    

  })###################################
 
  output$downloadData <- downloadHandler(
    
    filename = function(){
      paste("Prediction_data", "csv", sep=".")
    },
    
    content = function(file){
      write.table(Result_table,file,sep=",")
    }
  )
  
  
  
  
})