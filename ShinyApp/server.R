#library(shinysky)


source("initialize.R")
source("functions.R")

shinyServer(
  function(input, output, session) {
   
    values <- reactiveValues(
      predictions = predictNextWordKN(input=""),
      counter = 0
    )
    
    
    recalculateNextWords <- function(x){
      values$predictions <- predictNextWordKN(x)
      
      updatePredictionOptions()
    }
    
    updatePredictionOptions <- function() {
      output$prediction1 <- renderUI({
        actionButton("inputPrediction1", label = values$predictions[1])
      })
      
      output$prediction2 <- renderUI({
        actionButton("inputPrediction2", label = values$predictions[2])
      })
      
      output$prediction3 <- renderUI({
        actionButton("inputPrediction3", label = values$predictions[3])
      })
    }
    
    predictionSelected <- function(value) {
      #strip off any partially entered text for the next word
      partial <- getPartial(input$ngram)
      text <- input$ngram
      text <- substr(text, 1, nchar(text) - nchar(partial))
      
      #paste the selected value after the existing text (after cleaning)
      x <- trim(paste(trim(text), value, sep=" "))
      
      #add a space at the end to indiciate it is a complete entry
      x <- paste(x," ", sep="")
      
      updateTextAreaInput(session, "ngram", value=x)
      recalculateNextWords(x)
    }
    
    
    observeEvent(input$ngram, {
      
      recalculateNextWords(input$ngram)
      values$counter <- values$counter + 1
      updateTextInput(session, "testing", value=values$counter)
        
    })
    
    #set the initial button values for the predictions
    updatePredictionOptions()
    
     ##when the selected line target changes, update the list of controls that can be selected
    observeEvent(input$recalculate, {
        recalculateNextWords(input$ngram)
      })
    
    
    observeEvent(input$inputPrediction1, {
      predictionSelected(values$predictions[1])
    }
    )
    
    observeEvent(input$inputPrediction2, {
      predictionSelected(values$predictions[2])
    }
    )
    
    observeEvent(input$inputPrediction3, {
      predictionSelected(values$predictions[3])
    }
    )
  }
)