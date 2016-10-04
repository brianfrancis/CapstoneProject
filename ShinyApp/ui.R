library(shinysky)


fluidPage(
  fluidRow(
    column(12,textAreaInput("ngram", "Type Some Text"),
      fluidRow(
        column(2,uiOutput("prediction1")),
        column(2,uiOutput("prediction2")),
        column(2,uiOutput("prediction3"))
      )
    )
  )
)
 