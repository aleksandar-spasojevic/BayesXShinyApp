library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyFiles)
library(BayesXShinyApp)

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
      shinyDirButton('Folder', 'Select Folder', 'Please select a folder', FALSE),
      shinyFilesButton('Program', 'Select Program', 'Please select a program', FALSE),
      selectInput("Model", "Model", ""),
      # Only show this panel if model chosen
      uiOutput("Slider"),
      downloadButton("GetRCode", "Get RCode")
    ),
    
    mainPanel(
      bsAlert("Dialog"),
      bsModal("SpecifyCovariates","Covariates",trigger = NULL,
              selectInput("CovaritesAsVarying", "Varying", NULL,
                          multiple = TRUE),
              uiOutput("CovariatesToFix"),
              footer = bsButton("SaveModel", label = "Save"), size = "large"),
      tabsetPanel(
        tabPanel("Density",
                 bsButton("Matplot", "all Densities"),
                 plotOutput("Density"),
                 uiOutput("AxisRanges"),
                 # column(3, numericInput("xmin", "xmin", NULL)),
                 # column(3, numericInput("xmax", "xmax", NULL)),
                 # column(3, numericInput("ymin", "ymin", NULL)),
                 # column(3, numericInput("ymax", "ymax", NULL))
                 plotOutput("Densities")
        ),
        tabPanel("Moment",
                 # textInput("Moment", "Moment", "", "100%"),
                 selectInput("Moment", "Moment", c("mean","var","median","mod","cor")),
                 fluidRow(
                   column(3, selectizeInput("CovariateVarying", "Varying", NULL)),
                   column(6, textInput("Range", "Range", value = "seq(0,1,length.out = 100)"))
                 ),
                 actionButton("Plot", "Plot", icon("line-chart")),
                 plotOutput("MomentPlot")
        )
      )
    )
  )
))
