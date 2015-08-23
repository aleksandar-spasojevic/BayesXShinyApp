library(shiny)
library(shinyBS)
library(BayesXShinyApp)


shinyServer(function(input, output, session) {
  app_values <- reactiveValues(models = NULL)
  current <- reactiveValues(model = NULL, param_sample = NULL)
  commands <- reactiveValues(Rcode = NULL)
  
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
                  ticks = FALSE, sep = "", 
                  animate = animationOptions(interval = 300))
    })
  })
  
  observeEvent(input$Upload, {
    # run bayesX CLI
    tryCatch({
      withProgress(message = "BayesX fitting", value = 0, {
        result <- bayesX(input$Upload$datapath)
        
        output <- bayesXOutput(result)
        variables <- variables(output)
        ranges <- ranges(output)
        parameters <- parameters(output) # calc parameter samples on default grid
        
        # save result to 'models' list
        app_values$models <- append(app_values$models,
                                    structure(list(list(result = result,
                                                        output = output,
                                                        variables = variables,
                                                        ranges = ranges,
                                                        parameters = parameters)),
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
  
  observe({
    if( is.null(input$Parameter) )
      return( NULL )
    if( is.null(current$model) )
      return( NULL )
    variables <- current$model$variables
    if ( any(sapply(variables, function(v) is.null(input[[v]]))) )
      return( NULL )
    
    # create expression to find row-index in parameters: param[x == ? & ...] or all.equal(...) ...
    # TODO: subset already here with input$Parameter 
    obj <- "current$model$parameters"
    cond_expr <- list()
    for ( var in variables )
      cond_expr[[var]] <- paste(var, input[[var]], sep = "==")
    # cond_expr[[var]] <- paste("(sapply(", var, ",all.equal,", input[[var]], ") %in% 'TRUE')", sep = "")
    cond <- paste(cond_expr, collapse = " & ")
    subset <- eval(parse(text = sprintf("%s[%s]", obj, cond)))
    current$param_sample <- subset[[input$Parameter]]
  })
  
  output$Parameter <- renderPlot({
    if ( is.null(input$Parameter) | length(current$param_sample) == 0 )
      return( NULL )
    
    hist(current$param_sample, main = "", xlab = input$Parameter,
         xlim = c(isolate(input$xmin), isolate(input$xmax)))
  })
  
  output$Parameter_Summary <- renderText({
    if ( is.null(current$model) )
      return( NULL )
  })
  
  output$RExpression <- renderPlot({
    input$Plot # make dependency on 'Plot' Button
    
    if( is.null(current$model$parameters) | input$Plot == 0 )
      return( NULL )
    
    # Use isolate() to avoid dependency on input$RExpression
    isolate({
      # save command to commands$Rcode. If below there is an error, we will delete
      # the command
      tmpl <- "with(%s, %s)"
      data <- "do.call(append, list(params, attr(params, 'X')))"
      rcode <- sprintf(tmpl, data, input$RExpression)
      commands$Rcode[[input$Model]] <- append(commands$Rcode[[input$Model]], 
                                              list(rcode))
    })
    
    # Use isolate() to avoid dependency on input$RExpression
    tryCatch({
      isolate({
        eval(parse(text = input$RExpression), 
             do.call(append, list(x = current$model$parameters, 
                                  values = attr(current$model$parameters, "X"))))
      })
    },
    warning = function(w) {
      # delete command added previously
      commands$Rcode[[input$Model]] <- head(commands$Rcode[[input$Model]], -1)
      createAlert(session, "Dialog", title = "Warning", content = w$message)
    },
    error = function(e) {
      # delete command added previously
      commands$Rcode[[input$Model]] <- head(commands$Rcode[[input$Model]], -1)
      createAlert(session, "Dialog", title = "Error", content = e$message)
    })
    
  })
  
  output$GetRCode <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      sprintf("%s_%s.R", Sys.Date(), input$Model)
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write script to a file specified by the 'file' argument
      script <- create_RScript(Rcode = paste(commands$Rcode[[input$Model]], 
                                             collapse = "\n"), 
                               prg_path = input$Model)
      writeLines(script, file)
    }
  )
})
