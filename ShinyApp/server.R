source("initialize.R")
source("functions.R")

shinyServer(
  function(input, output, session) {
   
    reac <- reactiveValues(redraw = FALSE, 
                           ngram = isolate(input$ngram),
                           predictions=data.table())
    
    
    
    observeEvent(input$ngram, {
        reac$redraw <- FALSE
      
    })
    
    observe({
      invalidateLater(100, session)
      input$ngram
      
     
      if (isolate(reac$redraw)) {
        
        reac$ngram <- input$ngram
        
      } else {
        isolate(reac$redraw <- TRUE)
      }
    })
    
    observe({
      reac$ngram
      isolate({
        
        reac$predictions <- predictNextWordKN(reac$ngram)
        
        #if (is.na(reac$predictions[1])){
          session$sendCustomMessage(type="toggleVisible",
                                    message=list(NULL))
        #}
        
        output$prediction1 <- renderUI({
          actionButton("inputPrediction1", label = reac$predictions[1])
        })
        
        output$prediction2 <- renderUI({
          actionButton("inputPrediction2", label = reac$predictions[2])
        })
        
        output$prediction3 <- renderUI({
          actionButton("inputPrediction3", label = reac$predictions[3])
        })
        
        if (nchar(reac$ngram)>0) {
          session$sendCustomMessage(type="refocus",message=list(NULL))
        }
        
      })
    })
    
     
    predictionSelected <- function(value) {
      #strip off any partially entered text for the next word
      partial <- getPartial(isolate(reac$ngram))
      text <- isolate(reac$ngram)
      text <- substr(text, 1, nchar(text) - nchar(partial))
      
      #paste the selected value after the existing text (after cleaning)
      x <- trim(paste(trim(text), value, sep=" "))
      
      #add a space at the end to indiciate it is a complete entry
      x <- paste(x," ", sep="")
      
      updateTextAreaInput(session, "ngram", value=x)
      
    }
    
    
    observeEvent(input$inputPrediction1, {
      predictionSelected(reac$predictions[1])
    }
    )
    
    observeEvent(input$inputPrediction2, {
      predictionSelected(reac$predictions[2])
    }
    )
    
    observeEvent(input$inputPrediction3, {
      predictionSelected(reac$predictions[3])
    }
    )
    
    observe({
     
    })
    
  }
  
)