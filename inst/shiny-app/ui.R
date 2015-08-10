library(shiny)
library(shinyBS)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("BayesX"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("Upload", "Upload"),
      selectInput("Model", "Model", ""),
      uiOutput("Slider")
      # Only show this panel if model chosen
    ),
    
    mainPanel(
      bsAlert("Dialog"),
      tabsetPanel(
        tabPanel("Parameter",
                 selectInput("Parameter", "Parameter", ""),
                 plotOutput("Parameter")
        )
      )
    )
  )
))
