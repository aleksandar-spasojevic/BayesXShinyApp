
.distributions <- list(
  dagum = list(density = VGAM::ddagum,
               parameter = c("a" = "shape1.a",
                             "p" = "shape2.p",
                             "b" = "scale"),
               link = exp)
)

class(.distributions) <- c("distributions", class(.distributions))

#' @export
distribution <- function(name, density, params){
  
  # create an instance of 'distribution' object
  density_not_fun <- !is.function(density)
  if( density_not_fun )
    stop("'denisty' has to be a function")
  
  params_have_no_names <- is.null(attr(params, "names"))
  if( params_have_no_names )
    stop("all elements in 'params' need a name")
  
  structure(list(density = density, parameter = params), names = name)
}

#' @export
add <- function(obj) UseMethod("add")
add.distribution <- function(obj){
  .distributions <<- append(.distributions, obj)
}


#' @export
supported_distributions <- function(){
  names(.distributions)
}


#' @export
density.bayesXOutput <- function(bayesXOutput, X, ...){
  params <- parameters(bayesXOutput, X)
  return( density(params, ...) )
}

#' @export
density.parameters <- function(parameters, ...){
  
  distr <- distribution(parameters)
  if( is.null(distr) )
    stop( sprintf("distribution not found, add: %s", attr(parameters, "distribution")[[1]]) )
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

#' @export
plot.density <- function(density, xlab = "", ...){
  quantiles <- t(apply(density, 1, quantile, c(0.05, 0.95)))
  x <- c(attr(density, "x"), rev(attr(density, "x")))
  plot.default(x, quantiles, type = "n", ...)
  polygon(x, c(quantiles[,1], rev(quantiles[,2])), col = "lightgrey")
  
  means <- apply(density, 1, mean)
  lines(attr(density, "x"), means, col = "blue")
}

#' @export
matplot <- function(density, xlab = "", ...){
  graphics::matplot(attr(density, "x"), density, 
                    type = "l", col = "grey", lty = 1, xlab = xlab,
                    ...)
}

