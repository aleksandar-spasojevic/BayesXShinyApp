
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
  etas <- aggregate(effects_predicted)
  params <- lapply(etas, distribution(bayesXOutput)$link)
  
  # set class and attributes
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
  class(sel_params) <- class(parameters)
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
  distr <- distribution(parameters)
  mean_fun <- mean(distr)
  
  structure(do.call(mean_fun, parameters), 
            X = attr(parameters, "X"), 
            class = c("moment", "matrix"))
}


#' @export
mean.parameter <- function(parameter, ...){
  apply(parameter, FUN = mean.default, ...)
}


#' @export
var <- function(parameters, ...) UseMethod("var")

#' @export
var.parameters <- function(parameters, ...){
  distr <- distribution(parameters)
  var_fun <- var(distr)
  
  structure(do.call(var_fun, parameters), 
            X = attr(parameters, "X"), 
            class = c("moment", "matrix"))
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
median.parameters <- function(parameters, ...){
  distr <- distribution(parameters)
  median_fun <- median(distr)
  
  structure(do.call(median_fun, parameters), 
            X = attr(parameters, "X"), 
            class = c("moment", "matrix"))
}


#' @export
mode <- function(parameters, ...) UseMethod("mode")

#' @export
mode.parameters <- function(parameters, ...){
  distr <- distribution(parameters)
  mode_fun <- mode(distr)
  
  structure(do.call(mode_fun, parameters), 
            X = attr(parameters, "X"), 
            class = c("moment", "matrix"))
}


#' @export
distribution <- function(parameters, ...) UseMethod("distribution")
distribution.parameters <- function(parameters, ...){
  # NOTE: for each equation type there is an distribution attribute, unnecessary
  # -> take first element; often all equation types of same distribution!
  distr <- .distributions[[attr(parameters, "distribution")[[1]]]]
  
  if( is.null(distr) )
    stop( sprintf("distribution not found, add: %s", attr(parameters, "distribution")[[1]]) )
  
  class(distr) <- c("distribution", class(distr))
  return(distr)
}


#' @export
density.parameters <- function(parameters, ...){
  
  distr <- distribution(parameters)
  dens.fun <- distr$density
  
  # rename parameters so they match with fun (density) parameters in R
  names(parameters) <- distr$parameter[names(parameters)]
  dens.fun.vec <- Vectorize(dens.fun, names(parameters))
  samples <- do.call(dens.fun.vec, append(parameters, list(...)))
  
  return( structure(samples, 
                    class = c("density", class(samples)),
                    x = ...,
                    X = attr(parameters, "X")) )
}
