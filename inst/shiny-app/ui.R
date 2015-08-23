library(shiny)
library(shinyBS)
library(shinythemes)


shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  # make nice title
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h2 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }

    "))
  ),
  
  # Application title
  titlePanel("BayesX"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("Upload", "Upload"),
      selectInput("Model", "Model", ""),
      # Only show this panel if model chosen
      uiOutput("Slider"),
      downloadButton("GetRCode", "Get RCode")
    ),
    
    mainPanel(
      bsAlert("Dialog"),
      tabsetPanel(
        tabPanel("Parameter",
                 selectInput("Parameter", "Parameter", ""),
                 plotOutput("Parameter"),
                 fluidRow(
                   column(3, numericInput("xmin", "xmin", NULL)),
                   column(3, numericInput("xmax", "xmax", NULL))
                 )
        ),
        tabPanel("Custom",
                 textInput("RExpression", "R Expression", "", "100%"),
                 actionButton("Plot", "Plot", icon("line-chart")),
                 plotOutput("RExpression")
        )
      )
    )
  )
))
