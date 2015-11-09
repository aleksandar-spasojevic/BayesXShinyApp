library(shiny)
library(shinyBS)
library(shinyFiles)
library(BayesXShinyApp)


shinyServer(function(input, output, session) {
  app_values <- reactiveValues(models = NULL)
  current <- reactiveValues(model = NULL, param_sample = NULL, density = NULL)
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
                  animate = animationOptions(interval = 500))
    })
  })
  
  
  volumes <- c(root = "~")
  shinyDirChoose(input, 'Folder', roots = volumes, session = session)
  shinyFileChoose(input, 'Program', roots = volumes, session = session)
  
  observeEvent(input$Folder, {
    tryCatch({
      withProgress(message = "BayesX fitting", value = 0, {

        path <- parseDirPath(volumes, input$Folder)
        output <- bayesXOutput(path)
        variables <- variables(output)

        # default ranges (linked to grid of covariates! (see below))
        ranges <- sapply(variables, function(var) c(0,1), simplify = FALSE, USE.NAMES = TRUE)

        # create default grid of covariates
        X <- expand.grid(sapply(variables, function(var) seq(0, 1, length.out = 101), simplify = FALSE, USE.NAMES = TRUE))
        parameters <- parameters(output, X) # calc parameter samples on default grid

        # save result to 'models' list
        app_values$models <- append(app_values$models,
                                    structure(list(list(
                                      output = output,
                                      variables = variables,
                                      ranges = ranges,
                                      parameters = parameters)),
                                      names = path))
      })
    },
    warning = function(w) {
      createAlert(session, "Dialog", title = "Warning", content = w$message)
    },
    error = function(e) {
      createAlert(session, "Dialog", title = "Error", content = e$message)
    })
  })

  observeEvent(input$Program, {
    tryCatch({
      withProgress(message = "BayesX fitting", value = 0, {
        
        path <- parseFilePaths(volumes, input$Program)
        result <- bayesX( as.character(path$datapath) )

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
                                              names = as.character(path$name)))
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
    
    params <- lapply(subset, exp)
    attributes(params) <- attributes(subset)
    current$param_sample <- params
    
    xlim <- list(from = input$xmin, to = input$xmax)
    if( any(is.na(xlim)) ) {
      sequence <- list(from = 0.05, to = 1)
    } else {
      sequence <- xlim
    }
    x <- do.call(seq, append(sequence, list(length.out = 500)))
    current$density <- BayesXShinyApp:::density.parameters(params, x = x)
  })
  
  output$Density <- renderPlot({
    if( is.null(current$density) )
      return( NULL )
    
    ylim <- c(input$ymin, input$ymax)
    if( any(is.na(ylim)) )
      ylim <- NULL
    
    BayesXShinyApp:::plot.density(current$density, ylim = ylim)
  })
  
  output$Densities <- renderPlot({
    # make dependency on Matplot Button
    input$Matplot
    dens <- isolate(current$density)
    if( is.null(dens) )
      return( NULL )
    
    xlim <- c(input$xmin, input$xmax)
    if( any(is.na(xlim)) )
      xlim <- NULL
    ylim <- c(input$ymin, input$ymax)
    if( any(is.na(ylim)) )
      ylim <- NULL
    BayesXShinyApp:::matplot(dens, ylim = ylim, xlim = xlim)
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
    tryCatch({
      isolate({
        eval(parse(text = input$RExpression), 
             do.call(append, list(x = current$model$parameters, 
                                  values = attr(current$model$parameters, "X"))))
        # save command to commands$Rcode
        tmpl <- "with(%s, %s)"
        rcode <- sprintf(tmpl, "Data", input$RExpression)
        commands$Rcode[[input$Model]] <- append(commands$Rcode[[input$Model]], 
                                                list(rcode))
      })
    },
    warning = function(w) {
      createAlert(session, "Dialog", title = "Warning", content = w$message)
    },
    error = function(e) {
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
