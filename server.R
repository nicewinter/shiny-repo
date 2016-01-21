#server.R (prediction iris species)
library(caret)

irisSpecies<-function(sl,sw,pl,pw) {
  df1<-matrix(c(sl,sw,pl,pw,0),nrow=1)
  df1<-data.frame(df1)
  names(df1)<-names(iris)
  modFit<-train(Species~.,method="rf", trControl=trainControl(method ="cv"), data=iris)
  prediction<-predict(modFit, newdata=df1)
  as.character(prediction)
}

shinyServer(
  function(input,output){
    output$inputValue_sl<-renderPrint({input$sepalLength})
    output$inputValue_sw<-renderPrint({input$sepalWidth})
    output$inputValue_pl<-renderPrint({input$petalLength})
    output$inputValue_pw<-renderPrint({input$petalWidth})
    
    output$prediction<-renderText({
      input$goButton
      isolate(irisSpecies(input$sepalLength,input$sepalWidth,input$petalLength,input$petalWidth))
    })
    
  }
)