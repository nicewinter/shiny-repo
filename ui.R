#ui.R for iris species prediction
library(shiny)
shinyUI(
  pageWithSidebar(
    #application title
    headerPanel("Iris Species Prediction"),
    sidebarPanel(
      numericInput('sepalLength', 'Sepal Length',median(iris$Sepal.Length),min=min(iris
                                                                                   $Sepal.Length),max=max(iris$Sepal.Length),step=0.1),
      numericInput('sepalWidth', 'Sepal Width',median(iris$Sepal.Width),min=min(iris
                                                                                $Sepal.Width),max=max(iris$Sepal.Width),step=0.1),
      numericInput('petalLength', 'Petal Length',median(iris$Petal.Length),min=min(iris
                                                                                   $Petal.Length),max=max(iris$Petal.Length),step=0.1),
      numericInput('petalWidth', 'Petal Width',median(iris$Petal.Width),min=min(iris
                                                                                $Petal.Width),max=max(iris$Petal.Width),step=0.1),
      #submitButton('Submit')
      actionButton("goButton","Go!")
    ),
    mainPanel(
      h3('Results of Prediction'),
      h4('You entered Sepal Length'),
      verbatimTextOutput("inputValue_sl"),
      h4('You entered Sepal Width'),
      verbatimTextOutput("inputValue_sw"),
      h4('You entered Petal Length'),
      verbatimTextOutput("inputValue_pl"),
      h4('You entered Petal Width'),
      verbatimTextOutput("inputValue_pw"),
      h4('According to my prediction, the Iris Species is'),
      verbatimTextOutput("prediction")
    )
  ))
