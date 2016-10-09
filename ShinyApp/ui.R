pageWithSidebar(
  headerPanel('Word Prediction'),
  sidebarPanel(
    h4('Type some text followed by a space to get the next word predictions.'),
    h4('Start typing the next word to refine the predictions.'),
    h4('Select a predicted word to append it to your input text.')
  ),
  mainPanel(
    singleton(tags$head(tags$script(
      'function moveCaretToEnd(el) {
              if (typeof el.selectionStart == "number") {
                  el.selectionStart = el.selectionEnd = el.value.length;
              } else if (typeof el.createTextRange != "undefined") {
                  el.focus();
                  var range = el.createTextRange();
                  range.collapse(false);
                  range.select();
              }
          }'))),
      singleton(tags$head(tags$script(
        'Shiny.addCustomMessageHandler("refocus",
           function(message) {
           var textarea = document.getElementById("ngram");
           textarea.focus();
          moveCaretToEnd(textarea);
           });'))),
    
    fluidRow(
           column(12, textAreaInput("ngram", "Type Some Text", 
                                    placeholder="Enter space at end to predict next word",
                                    rows=5), #tags$textarea(id="ngram", rows=3, cols=40, autofocus = "autofocus"),
                  
             fluidRow(
               column(2,uiOutput("prediction1")),
               column(2,uiOutput("prediction2")),
               column(2,uiOutput("prediction3"))
             )
            
           ),
           column(12, uiOutput("noselections"))
         )
  )
)

# fluidPage(
#   singleton(
#     tags$head(tags$script(
#       'Shiny.addCustomMessageHandler("refocus",
#       function(message) {
#       var textarea = document.getElementById("ngram");
#       textarea.focus();
#       var range = textarea.createTextRange();
#       range.collapse(false);
#       range.select();
#       });'))),
#   fluidRow(
#     column(12, textAreaInput("ngram", "Type Some Text"), #tags$textarea(id="ngram", rows=3, cols=40, autofocus = "autofocus"),
#            
#       fluidRow(
#         column(2,uiOutput("prediction1")),
#         column(2,uiOutput("prediction2")),
#         column(2,uiOutput("prediction3"))
#       )
#     )
#   )
# )
