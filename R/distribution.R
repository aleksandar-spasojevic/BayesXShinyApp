
.distributions <- list(
  dagum = list(density = VGAM::ddagum,
               parameter = c("a" = "shape1.a",
                             "p" = "shape2.p",
                             "b" = "scale"))
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
