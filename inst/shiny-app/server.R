library(shiny)
library(shinyBS)
library(BayesXShinyApp)

shinyServer(function(input, output, session) {
  app_values <- reactiveValues(models = NULL)
  current <- reactiveValues(model = NULL)
  observe({
    updateSelectInput(session, "Model", choices = names(app_values$models))
  })
  observeEvent(input$Model, {
    current$model <- app_values$models[[input$Model]]
    })

  output$Slider <- renderUI({
    if ( is.null(current$model) )
      return( NULL )
    ranges <- current$model$ranges
    variables <- current$model$variables
    lapply(variables, function(v) { # index slider
      sliderInput(v, v, ranges[[v]][1], ranges[[v]][2], ranges[[v]][1], 
                  step = ((ranges[[v]][2] - ranges[[v]][1])/100),
                  ticks = FALSE, sep = "", animate = TRUE)
    })
  })

  observeEvent(input$Upload, {
    # run bayesX CLI
    tryCatch({
      withProgress(message = "BayesX fitting", value = 0, {
        result <- bayesX(input$Upload$datapath)

        output <- bayesXOutput(result)
        variables <- variables(output)
        data <- data_(result)
        ranges <- ranges(output)
        sequences <- sequences(output)
        parameters <- parameters(output) # calc parameter samples on default grid

        # save result to 'models' list
        app_values$models <- append(app_values$models,
                                    structure(list(list(result = result,
                                                        output = output,
                                                        variables = variables,
                                                        data = data,
                                                        ranges = ranges,
                                                        parameters = parameters,
                                                        sequences = sequences)),
                                              names = input$Upload$name))
      })
    },
    warning = function(w) {
      createAlert(session, "Dialog", title = "Warning", content = w$message)
    },
    error = function(e) {
      createAlert(session, "Dialog", title = "Error", content = e$message)
    })
  })

  observe({
    updateSelectInput(session, "Parameter",
                      choices = names(current$model$parameters))
  })

  output$Parameter <- renderPlot({
    if ( is.null(current$model) )
      return( NULL )
    
    # create expression to find row-index in parameters
    obj <- "current$model$parameters"
    logical_expr <- list()
    for ( var in current$model$variables )
      logical_expr[[var]] <- paste(var, input[[var]], sep = "==")
    combined <- paste(logical_expr, collapse = " & ")
    subset <- eval(parse(text = paste0(obj, "[", combined, "]")))
    param <- subset[[input$Parameter]]
  
    hist(param)
  })

})
