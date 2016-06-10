# This file creates the UI for ShinyApp
# Chao Liu
# 06/2016

library(shiny)
shinyUI(
    pageWithSidebar(
        headerPanel("Natural Laguage Processing Model to Predict Next Word"),
        sidebarPanel(
            h4("Please enter the sentence that you want to predict: "),
            textInput('enterwords','enterwords', "Please enter something:)"),
            actionButton("goButton", "Submit!"),
            h4("You can see the attributes of words if you want: "),
            checkboxGroupInput("checkbox", "Histograms of freq of woreds", 
                               c("Freq of total word" = "im1",
                                 "Freq of words that are relative to the input" = "im2",
                                 "Word cloud of all the words" = "im3",
                                 "Word cloud of relative words" = "im4"))
        ),
        mainPanel(
            h3('Results of Prediction'),
            h4('Your Inputs:'),
            verbatimTextOutput("text1"),
            h4('According to our natural language processing
               algorithm, it is highly likely that the next word is:'),
            verbatimTextOutput("prediction"),
            h3("Figures of words:"),
            plotOutput('oim1'),
            plotOutput('oim2'),
            plotOutput('oim3'),
            plotOutput('oim4')
        )
    )
)