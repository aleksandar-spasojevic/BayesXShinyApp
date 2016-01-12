
#' Wrapper around 'parameters'
#' 
#' @export
Parameters <- function(path, X, ...){
  # is file (= program) or folder (= output/result folder)
  if (dir.exists(path)) {
    output <- bayesXOutput(path)
    return( parameters(output, X = X, ...) )
    
  } else if (file.exists(path)) {
    result <- bayesX(path)
    output <- bayesXOutput(result)
    return( parameters(output, X = X, ...) )
    
  } else {
    stop("path not found")
  }
}


#' recieve distributional parameters (e.g \strong{\eqn{\eta}}'s)
#' 
#' returns sample of distributional parameters (e.g \strong{\eqn{\eta}})
#' @noRd
parameters <- function(bayesXOutput, ...) UseMethod("parameters")

parameters.bayesXOutput <- function(bayesXOutput, 
                                    # if 'X' not given, we take sequence of each 
                                    # variable in data, make grid and predict on 
                                    # this grid
                                    X = expand.grid(sequences(bayesXOutput)[variables(bayesXOutput)]),
                                    ...){
  effects_predicted <- predict(bayesXOutput, X = X)
  etas <- aggregate(effects_predicted)
  
  old_names <- names(etas)
  names(etas) <- .rename(names(etas)) # we rename etas's so one can use 
  # distribution objects with generic parameter names in function declaration 
  # (density, link, moment (e.g mu_1, ...))
  
  params <- tryCatch({
    link <- distribution(bayesXOutput)$link
    link_is_list <- is.list(link)
    if( link_is_list ){
      # link is named list: extract by parameters name the link function and
      # apply it on parameters eta
      params <- list()
      for( param in names(etas) )
        params[[param]] <- link[[ param ]]( etas[[param]] )
      params
    } else {
      # link is one function: apply same link-function on each parameters eta
      lapply(etas, link)
    }
  }, error = function(e){
    # use identity function as link
    warning( sprintf("there is no link function defined for '%s', instead we use identity function", 
                     bayesXOutput["family"][[1]]) )
    etas
  })
  
  # set class and attributes
  attributes(params) <- attributes(etas)
  attr(params, "X") <- structure(as.list(X), out.attrs = NULL)
  attr(params, "distribution") <- structure(bayesXOutput["family"], 
                                            names = bayesXOutput["equationtype"])
  attr(params, "old_names") <- old_names
  class(params) <- c("parameters", class(params))
  
  return( params )
}

aggregate.effects_predicted <- function(effects_predicted, ...){
  effects_aggr <- tapply(effects_predicted, 
                         # aggregate over 'equationtype' + 'dimension variable' (see '.dimension')
                         INDEX = list( names(effects_predicted) ), 
                         FUN = function(...) {
                           eff_aggr <- Reduce("+",...)
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
quantile <- function(parameters, ...) UseMethod("quantile")

#' @export
quantile.parameters <- function(parameters, ...){
  lapply(parameters, stats::quantile, ...)
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
            class = c("moment", "list"))
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
            class = c("moment", "list"))
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
median <- function(parameters, ...) UseMethod("median")

#' @export
median.parameters <- function(parameters, ...){
  distr <- distribution(parameters)
  median_fun <- median(distr)
  
  structure(do.call(median_fun, parameters), 
            X = attr(parameters, "X"), 
            class = c("moment", "list"))
}


#' @export
mode <- function(parameters, ...) UseMethod("mode")

#' @export
mode.parameters <- function(parameters, ...){
  distr <- distribution(parameters)
  mode_fun <- mode(distr)
  
  structure(do.call(mode_fun, parameters), 
            X = attr(parameters, "X"), 
            class = c("moment", "list"))
}


#' @export
cor <- function(parameters, ...) UseMethod("cor")

#' @export
cor.parameters <- function(parameters, ...){
  distr <- distribution(parameters)
  cor_fun <- cor(distr)
  
  structure(do.call(cor_fun, parameters), 
            X = attr(parameters, "X"), 
            class = c("moment", "list"))
}


distribution <- function(parameters, ...) UseMethod("distribution")
distribution.parameters <- function(parameters, ...){
  # NOTE: for each equation type there is an distribution attribute, unnecessary
  # -> take first valid element; often all equation types of same distribution!
  
  for( distr in attr(parameters, "distribution") ){
    if (is.null(distr))
      next
    return( .distributions[[distr]] )
  }
  
  if( is.null(distr) )
    stop( sprintf("distribution not found, add: %s", distr) )
}


#' @export
density <- function(parameters, ...) UseMethod("density")

#' @export
density.parameters <- function(parameters, ...){
  if(all(is.na(parameters)))
    stop("density not defined with these covariate values")
  distr <- distribution(parameters)
  dens.fun <- distr$density
  
  dens.fun.vec <- Vectorize(dens.fun, names(parameters))
  samples <- do.call(dens.fun.vec, append(parameters, list(...)))
  
  return( structure(samples, 
                    class = c(distr$class, "density", class(samples)),
                    x = ...,
                    X = attr(parameters, "X")) )
}


#' @export
print.density <- function(dens, ...){
  str(dens)
}


#' @note renames parameters so one can use distribution object's with function
#' arguments of general scheme
.rename <- function(names){
  splitted <- strsplit(names, "_")
  transposed <- structure(do.call(rbind, splitted), 
                          dimnames = list(NULL, c("param","dim")))
  transposed[,2] <- ave(transposed[,"dim"], transposed[,"param"], 
                        FUN = function(dim) 1:length(dim))
  
  new_names <- mapply(transposed[,1],transposed[,2],FUN = paste, sep = "_")
  return( new_names )
}

