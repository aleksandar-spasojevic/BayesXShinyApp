
#' recieve distributional parameters (e.g \strong{\eqn{\eta}}'s)
#' 
#' returns sample of distributional parameters (e.g \strong{\eqn{\eta}})
#' 
#' @export
parameters <- function(bayesXOutput, ...) UseMethod("parameters")

#' @export
parameters.bayesXOutput <- function(bayesXOutput, 
                                    # if 'X' not given, we take sequence of each variable in data,
                                    # make grid and predict on this grid
                                    X = expand.grid(sequences(bayesXOutput)[variables(bayesXOutput)]),
                                    ...){
  # tryCatch since some elements of bayesXOutput are not of type 'effect'. If
  # one 'elem' is not of type 'effect' we will return 'NULL' otherwise 
  # 'predict.effect' function is called
  force(X)
  effects_predicted <- unlist(lapply(bayesXOutput, function(elem, ...){
    tryCatch(predict(elem, X = X, ...), 
             warning = function(w) NULL,
             error = function(e) NULL)
  }, ...), recursive = FALSE, use.names = TRUE)
  
  etas <- tapply(effects_predicted, 
                 INDEX = names(effects_predicted), 
                 FUN = function(...) {
                   eta <- do.call("+", ...)
                   class(eta) <- c("parameter", class(eta))
                   return(eta)
                 })
  
  return(structure(etas, 
                   class = c("parameters", class(etas)),
                   X = structure(as.list(X), out.attrs = NULL)))
}


#' subscript operator for 'parameters' objects
#' 
#' @note one can use \code{\link{all.equal}} to extract rows. Row's represent 
#' the underlying grid of covariates. Covariates cannot be compared with 
#' \code{==} operator, since they are \code{numeric}. Therefore one prefers 
#' \code{\link{all.equal}} for 'near equality'. Using \code{\link{all.equal}} 
#' will either result in a \code{character} \code{TRUE} or message, 
#' therefore we have to check if it's a \code{character}, if so, we extract indexes 
#' where \code{TRUE} stays and use those for indexing. Drawback of this is slowness!
#' @export
"[.parameters" <- function(parameters, ...){
  match <- with(attr(parameters, "X"), ...)
  # subset by row
  sel_params <- lapply(parameters, "[", match, , drop = FALSE)
  
  # set again classes
  for( index in 1:length(sel_params) )
    class(sel_params[[index]]) <- c("parameter", class(sel_params[[index]]))
  class(sel_params) <- c("parameters", class(sel_params))
  attr(sel_params, "X") <- lapply(attr(parameters, "X"), "[", match, drop = FALSE)
  
  return(sel_params)
}


#' @export
quantile.parameters <- function(parameters, ...){
  lapply(parameters, quantile, ...)
}


#' @export
quantile.parameter <- function(parameter, ...){
  # over 'columns'
  apply(parameter, FUN = quantile, ...)
}


#' @export
mean.parameters <- function(parameters, ...){
  lapply(parameters, mean, ...)
}


#' @export
mean.parameter <- function(parameter, ...){
  apply(parameter, FUN = mean, ...)
}


#' @export
var <- function(parameters, ...) UseMethod("var")

#' @export
var.parameters <- function(parameters, ...){
  lapply(parameters, var, ...)
}

#' @export
var.parameter <- function(parameter, ...){
  apply(parameter, FUN = stats::var, ...)
}


#' @export
sd <- function(parameters, ...) UseMethod("sd")

#' @export
sd.parameters <- function(parameters, ...){
  lapply(parameters, sd, ...)
}

#' @export
sd.parameter <- function(parameter, ...){
  apply(parameter, FUN = stats::sd, ...)
}
