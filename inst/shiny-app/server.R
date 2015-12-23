library(shiny)
library(shinyBS)
library(shinyFiles)
library(BayesXShinyApp)


shinyServer(function(input, output, session) {
  app_values <- reactiveValues(models = NULL)
  selected <- reactiveValues(model = NULL, param_sample = NULL, density = NULL)
  commands <- reactiveValues(Rcode = NULL)
  
  observe({
    models <- names(app_values$models)
    updateSelectInput(session, "Model", choices = models,
                      selected = tail(models,1))
  })
  
  observeEvent(input$Model, {
    selected$model <- app_values$models[[input$Model]]
  })
  
  # update axes ui components
  output$AxisRanges <- renderUI({
    if ( is.null(selected$model) )
      return( NULL )
    
    is_bivariate <- grepl("bivariate",selected$model$dim)
    
    if(is_bivariate){
      dens_axis <- c("y1","y2")
    } else {
      dens_axis <- c("y1")
    }
    
    lapply(dens_axis, function(axs){
      id_min <- paste0("AxisMin",axs)
      id_max <- paste0("AxisMax",axs)
      list(numericInput(id_min, label = paste0(axs, " Min"), value = NULL),
           numericInput(id_max, label = paste0(axs, " Max"), value = NULL))
    })
  })
  
  output$Slider <- renderUI({
    if ( is.null(selected$model) )
      return( NULL )
    
    variables <- selected$model$varyingCovariates
    lapply(variables, function(v) { # index slider
      numericInput(v, v, NULL)
    })
  })
  
  output$CovariatesToFix <- renderUI({
    variablesToFix <- setdiff(Variables, input$CovaritesAsVarying)
    lapply(variablesToFix, function(v){
      numericInput(paste0(v,"fixed"), v, NULL)
    })
  })
  
  observeEvent(selected$model$variables, {
    updateSelectizeInput(session, 'CovariateVarying', 
                         choices = selected$model$variables)
  })
  
  volumes <- c(root = "~")
  shinyDirChoose(input, 'Folder', roots = volumes, session = session)
  shinyFileChoose(input, 'Program', roots = volumes, session = session)
  
  Path <- NULL
  Output <- NULL
  Variables <- NULL
  Dimension <- NULL
  
  observeEvent(input$Folder, {
    tryCatch({
      withProgress(message = "BayesX fitting", value = 0, {
        
        Path <<- parseDirPath(volumes, input$Folder)
        Output <<- BayesXShinyApp:::bayesXOutput.character(Path)
        Dimension <<- BayesXShinyApp:::distribution.bayesXOutput(Output)$class
        Variables <<- BayesXShinyApp:::variables(Output)
        
        updateSelectizeInput(session, "CovaritesAsVarying", choices = Variables)
        
        # choose which variables should varying and which should be fixed
        toggleModal(session, "SpecifyCovariates", "open")
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
        
        Path <<- parseFilePaths(volumes, input$Program)
        result <- BayesXShinyApp:::bayesX( as.character(Path$datapath) )
        Output <<- BayesXShinyApp:::bayesXOutput.bayesXResult(result)
        Dimension <<- BayesXShinyApp:::distribution.bayesXOutput(Output)$class
        Variables <<- BayesXShinyApp:::variables(Output)
        
        updateSelectizeInput(session, "CovaritesAsVarying", choices = Variables)
        
        # choose which variables should varying and which should be fixed
        toggleModal(session, "SpecifyCovariates", "open")
      })
    },
    warning = function(w) {
      createAlert(session, "Dialog", title = "Warning", content = w$message)
    },
    error = function(e) {
      createAlert(session, "Dialog", title = "Error", content = e$message)
    })
  })
  
  observeEvent(input$SaveModel, {
    fixedGrid <- as.list(unlist(lapply(names(input),function(name) {
      if(grepl("fixed",name) & !is.na(input[[name]][1]))
        structure(input[[name]],names = gsub("fixed","",name))
    })))
    fixedCovariates <- names(fixedGrid)
    
    app_values$models <- append(app_values$models,
                                structure(list(list(
                                  output = Output,
                                  variables = Variables,
                                  fixedCovariates = fixedCovariates,
                                  varyingCovariates = setdiff(Variables,fixedCovariates),
                                  fixedGrid = fixedGrid,
                                  dim = Dimension
                                )),
                                names = as.character(Path[[1]])))
    
    toggleModal(session, "SpecifyCovariates", "close")
  })
  
  observe({
    if( is.null(selected$model) )
      return( NULL )
    
    vCovariates <- selected$model$varyingCovariates
    vGrid <- sapply(vCovariates, function(v) input[[v]])
    
    if ( any(sapply(vGrid,is.null)) | any(sapply(vGrid,is.na)) )
      return( NULL )
    
    grid <- append(selected$model$fixedGrid, as.list(vGrid))
    withProgress(message = "calculating parameters", value = 0, {
      selected$param_sample <<- BayesXShinyApp:::parameters.bayesXOutput(selected$model$output,grid)
    })
  })
  
  observe({
    if(is.null(selected$param_sample) || any(is.na(attr(selected$param_sample,"X"))))
      return(NULL)
    
    AxisInputs <- names(input)[grepl("Axis",names(input))]
    ranges <- structure(lapply(AxisInputs, function(axs) input[[axs]]),
                        names = AxisInputs)
    
    if(any(is.na(ranges)))
      return(NULL)
    
    variable <- gsub(pattern = ".{7}(y\\d)", "\\1", AxisInputs)
    sequences <- tapply(AxisInputs, variable, function(axisRange){
      max <- grepl("Max", axisRange)
      seq.default(from = ranges[[axisRange[!max]]], 
                  to = ranges[[axisRange[max]]],
                  length.out = 100)
    })
    
    if(length(sequences) == 1){
      grid <- unlist(sequences)
    } else {
      grid <- expand.grid(sequences)
    }
    
    # # save command to commands$Rcode
    # subsetting_code <- sprintf("param = %s[%s]", "params", cond)
    # sequence_code <- sprintf("x = seq(%s, length.out = 200)",
    #                          paste(names(sequence), sequence, sep = "=", collapse = ","))
    # dens_code <- "dens = density(param, x = x)"
    # rcode <- paste(subsetting_code, sequence_code, dens_code, sep = "\n") # combine all codes
    # isolate(commands$Rcode[[input$Model]] <- append(commands$Rcode[[input$Model]], list(rcode)))
    tryCatch({
      withProgress(message = "calculating density", value = 0, {
        selected$density <<- BayesXShinyApp:::density.parameters(selected$param_sample, 
                                                                 x = grid)
      })
    },
    warning = function(w) {
      createAlert(session, "Dialog", title = "Warning", content = w$message)
    },
    error = function(e) {
      createAlert(session, "Dialog", title = "Error", content = e$message)
    })
  })
  
  output$Density <- renderPlot({
    if( is.null(selected$density) )
      return( NULL )
    
    plot(selected$density)
  })
  
  output$Densities <- renderPlot({
    # make dependency on Matplot Button
    input$Matplot
    dens <- isolate(selected$density)
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
  
  output$MomentPlot <- renderPlot({
    input$Plot # make dependency on 'Plot' Button
    
    if( input$Plot == 0 )
      return( NULL )
    
    # Use isolate() to avoid dependency on input$RExpression
    tryCatch({
      isolate({
        moment_fun <- input$Moment
        varying <- input$CovariateVarying
        range <- eval(parse(text = input$Range))
        
        fixed <- setdiff(selected$model$varyingCovariates,varying)
        fixed_value <- as.list(sapply(fixed,function(v) input[[v]]))
        
        covariates <- append(fixed_value,
                             structure(list(range), names = varying))
        
        # extend grid if there are model based fixed Covariates!
        if(length(selected$model$fixedGrid) > 0)
          covariates <- append(covariates, selected$model$fixedGrid)
        
        grid <- expand.grid(covariates)
        
        withProgress(message = "calculating parameters", value = 0, {
          params <- BayesXShinyApp:::parameters.bayesXOutput(selected$model$output,grid)
        })
        
        rcode <- sprintf("lines(%s(params))", moment_fun)
        eval(parse(text = rcode))
        # save command to commands$Rcode
        # commands$Rcode[[input$Model]] <- append(commands$Rcode[[input$Model]],
        #                                         list(rcode))
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
