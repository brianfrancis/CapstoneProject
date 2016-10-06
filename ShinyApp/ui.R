pageWithSidebar(
  headerPanel('Word Prediciton'),
  sidebarPanel(
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    singleton(
      tags$head(tags$script(
        'Shiny.addCustomMessageHandler("refocus",
           function(message) {
           var textarea = document.getElementById("ngram");
           textarea.focus();
           var range = textarea.createTextRange();
           range.collapse(false);
           range.select();
           });'))),
    
    fluidRow(
           column(12, textAreaInput("ngram", "Type Some Text"), #tags$textarea(id="ngram", rows=3, cols=40, autofocus = "autofocus"),
                  
             fluidRow(
               column(2,uiOutput("prediction1")),
               column(2,uiOutput("prediction2")),
               column(2,uiOutput("prediction3"))
             )
           )
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
