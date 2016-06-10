# This file creates the Server for ShinyApp
# Chao Liu
# 06/2016

source("Functionality.R")
library(shiny)
shinyServer(
    function(input, output){
        output$prediction <- renderPrint(
            if(input$goButton == 0) "Waiting for your input"
            else {
                pred_model(input$enterwords)
            }
        )
        output$oim1 <- renderPlot(
            if("im1" %in% input$checkbox){
                hist(training$freq, freq = FALSE, col = "lightblue", xlab = "Freq", main = "Freq of total word")
                lines(density(training$freq), col = "green")
            }
        )
        output$oim2 <- renderPlot(
            if("im2" %in% input$checkbox){
                freq <- sapply(pred_model(input$enterwords), count_word_freq)
                hist(freq, col = "red", freq = FALSE,xlab = "Freq", main = "Freq of words that are relative to the input")
                lines(density(freq), col = "green")
            }
        )
        output$oim3 <- renderPlot(
            if("im3" %in% input$checkbox){
                pal <- brewer.pal(5,"Dark2")
                wordcloud(training$name, freq = training$freq, min.freq = sort(training$freq,decreasing = T)[30], 
                          colors = pal,main = "Word cloud of all the words")
            }
        )
        output$oim4 <- renderPlot(
            if("im4" %in% input$checkbox){
                pal <- brewer.pal(5,"Dark2")
                freq <- sapply(pred_model(input$enterwords), count_word_freq)
                wordcloud(pred_model(input$enterwords), freq = freq, min.freq = 1, 
                          colors = pal,main = "Word cloud of relative words")
            }
        )
        output$text1 <- renderPrint(
            if(input$goButton == 0) " "
            else input$enterwords
        )
    }
)