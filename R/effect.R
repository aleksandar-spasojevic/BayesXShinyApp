#' @export
linear <- function(effect, ...) UseMethod("linear")

#' @export
linear.effect <- function(effect, ...){
  return( grepl("^linear", effect[["filetype"]]) )
}


#' @export
has_constant <- function(effect, ...) UseMethod("has_constant")

#' @export
has_constant.effect <- function(effect, ...){
  return( grepl("const", effect[["term"]]) )
}


#' @export
nonlinear <- function(effect, ...) UseMethod("nonlinear")

#' @export
nonlinear.effect <- function(effect, ...){
  return( grepl("^nonlinear", effect[["filetype"]]) )
}


#' @export
variables <- function(effect, ...) UseMethod("variables")

#' @export
variables.effect <- function(effect, ...){
  # remove 'const' if present
  variables <- gsub("const", "", effect[["term"]])
  # remove leading|trailing whitespaces and possible smooth 'sx(...)' key
  # also clean ',' if multiple covariates in smooth function
  variables_clean <- unlist(strsplit(gsub("^\\s+|s\\(|sx\\(|\\)|\\s+$", "", variables), ","))
  return( unlist(strsplit(variables_clean, " ")) )
}


#' predict an effect
#' 
#' @export
predict.effect <- function(effect, X, ...){
  len <- length(X[[1]])
  # extract only variables which are member of effect
  X <- X[variables(effect)] # if 'X' a list not slow
  
  if ( linear(effect) ) {
    if ( has_constant(effect) )
      X <- append(X, list(const = rep.int(1, len)), after = 0) # append '1' would work, but dimension in parameters not of same dimension ('sweep' a possible solution)
    design_matrix <- do.call(cbind, X)
    
  } else if ( nonlinear(effect) ) {
    basis <- source(effect[["pathbasis"]], local = TRUE)$value
    design_matrix <- basis(X)
    
  } else {
    stop( sprintf("%s: type of effect not supported", effect[["filetype"]]) )
  }
  
  params <- t(read.table(effect[["pathsamples"]], header = TRUE)[,-1])
  predicts <- design_matrix %*% params
  return( structure(list(predicts), names = effect[["equationtype"]]) )
}
