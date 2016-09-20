library(shinysky)


fluidPage(
  fluidRow(
    column(6,textAreaInput("ngram", "Type Some Text"),
      fluidRow(
        column(2,uiOutput("prediction1")),
        column(2,uiOutput("prediction2")),
        column(2,uiOutput("prediction3"))
      )
    ),
    column(2,actionButton("recalculate", "Update"))
    ),
  textInput("testing", "type something"),
  textAreaInput("testing123", "type something")
)
              
              #)
 