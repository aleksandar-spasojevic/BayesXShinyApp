raw_paths <- list.files(system.file("extdata", package = "BayesXShinyApp"), 
                        pattern = "*.raw", full.names = TRUE)

res_paths <- list.files(system.file("extdata", package = "BayesXShinyApp"), 
                        pattern = "*.res", full.names = TRUE)

# TODO: make dependent on OS
bayesX_path <- system.file("bayesx", package = "BayesXShinyApp")
