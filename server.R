library(splitstackshape)
library(e1071)
library(tm)
library(RWeka)
library(reshape2)
library(stringr)
library(knitr)
library(stringi)
library(SnowballC)
library(openNLP)
library(googleVis)
library(qdap)
library(wordcloud)
library(pbapply)
library(dplyr)
library(knitr)

# Data are coming from https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

source("DummyPrediction.R")

shinyServer(function(input, output) {
    
    # You can access the value of the widget with input$text, e.g.
    output$value <- renderPrint({ 
        predicted_text <- DummyPrediction(input$text)
        as.character(
            if (ncol(predicted_text) == 1) {
                predicted_text[1,1]
            }
            else { 
                predicted_text[1,ncol(predicted_text)-1]
            }
        )
    })
    output$table <- renderTable({ DummyPrediction(input$text) })
    
    output$plot <- renderPlot({
        predicted_text <- DummyPrediction(input$text)
        ggplot(
            data=predicted_text[1:5,], 
            aes_string(
                x=colnames(predicted_text)[ncol(predicted_text)-1],
                y=colnames(predicted_text)[ncol(predicted_text)],
                fill = colnames(predicted_text)[ncol(predicted_text)]
                )) + geom_bar(stat="identity")
    })
})
