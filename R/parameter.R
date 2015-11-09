
#' recieve distributional parameters (e.g \strong{\eqn{\eta}}'s)
#' 
#' returns sample of distributional parameters (e.g \strong{\eqn{\eta}})
#' 
#' @export
parameters <- function(bayesXOutput, ...) UseMethod("parameters")

#' @export
parameters.bayesXOutput <- function(bayesXOutput, 
                                    # if 'X' not given, we take sequence of each 
                                    # variable in data, make grid and predict on 
                                    # this grid
                                    X = expand.grid(sequences(bayesXOutput)[variables(bayesXOutput)]),
                                    ...){
  effects_predicted <- predict(bayesXOutput, X = X)
  etas <- aggregate(effects_predicted, X)
  params <- lapply(etas, distribution(bayesXOutput)$link)
  
  # set attributes
  attributes(params) <- attributes(etas)
  attr(params, "X") <- structure(as.list(X), out.attrs = NULL)
  attr(params, "distribution") <- structure(bayesXOutput["family"], 
                                            names = bayesXOutput["equationtype"])
  class(params) <- c("parameters", class(params))

  return( params )
}

aggregate.effects_predicted <- function(effects_predicted, ...){
  effects_aggr <- tapply(effects_predicted, 
                         INDEX = names(effects_predicted), 
                         FUN = function(...) {
                           eff_aggr <- do.call("+", ...)
                           class(eff_aggr) <- c("effects_aggregated", class(eff_aggr))
                           return(eff_aggr)
                         })
  
  return( structure(effects_aggr, class = c("effects_aggregated", class(effects_aggr))) )
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
  
  # set again classes and attributes
  for( index in 1:length(sel_params) )
    class(sel_params[[index]]) <- c("parameter", class(sel_params[[index]]))
  class(sel_params) <- c("parameters", class(sel_params))
  attr(sel_params, "X") <- lapply(attr(parameters, "X"), "[", match, drop = FALSE)
  attr(sel_params, "distribution") <- attr(parameters, "distribution")
  
  return(sel_params)
}


#' @export
quantile.parameters <- function(parameters, ...){
  lapply(parameters, quantile, ...)
}


#' @export
quantile.parameter <- function(parameter, ...){
  # over 'columns'
  apply(parameter, FUN = stats:::quantile.default, ...)
}


#' @export
mean.parameters <- function(parameters, ...){
  lapply(parameters, mean, ...)
}


#' @export
mean.parameter <- function(parameter, ...){
  apply(parameter, FUN = mean.default, ...)
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


#' @export
distribution <- function(parameters, ...) UseMethod("distribution")
distribution.parameters <- function(parameters, ...){
  # NOTE: for each equation type there is an distribution attribute, unnecessary
  # -> take first element; often all equation types of same distribution!
  .distributions[[attr(parameters, "distribution")[[1]]]]
}
