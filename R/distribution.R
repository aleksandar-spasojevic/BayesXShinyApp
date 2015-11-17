
.distributions <- list(
  dagum = structure(list(density = VGAM::ddagum,
                         parameter = c("a" = "shape1.a",
                                       "p" = "shape2.p",
                                       "b" = "scale"),
                         link = exp,
                         moment = list(
                           # only functions here!
                           # see wikipedia: https://en.wikipedia.org/wiki/Dagum_distribution
                           
                           # mean function
                           mean = function(a, b, p) {
                             mean <- (-b/a) * gamma(-1/a) * gamma(1/a + p) / gamma(p)
                             # if a <= 1 then mean not defined!
                             mean[ a <= 1 ] <- NA
                             return( mean )
                           },
                           
                           # median function
                           median = function(a, b, p) b * (-1 + 2^{1/p})^(-1/a),
                           
                           # mode function
                           mode = function(a, b, p) b * ((a*p - 1)/(a + 1))^(1/a),
                           
                           # variance function
                           var = function(a, b, p) {
                             var <- -((b/a)^2) * ( 2*a * gamma(-2/a)*gamma(2/a+p)/gamma(p) + (gamma(-1/a)*gamma(1/a + p)/gamma(p))^2 )
                             # if a <= 2 then variance not defined!
                             var[ a <= 2 ] <- NA
                             return( var )
                           }
                         )
  ), class = c("distribution", "list"))
)

class(.distributions) <- c("distributions", class(.distributions))


#' @export
supported_distributions <- function(){
  names(.distributions)
}

mean.distribution <- function(distr, ...){
  fun <- distr$moment$mean
  
  if( is.null(fun) )
    stop("no mean function defined for distribution")
  else
    return(fun)
}

median.distribution <- function(distr, ...){
  fun <- distr$moment$median
  
  if( is.null(fun) )
    stop("no median function defined for distribution")
  else
    return(fun)
}

mode.distribution <- function(distr, ...){
  fun <- distr$moment$mode
  
  if( is.null(fun) )
    stop("no mode function defined for distribution")
  else
    return(fun)
}

var.distribution <- function(distr, ...){
  fun <- distr$moment$var
  
  if( is.null(fun) )
    stop("no variance function defined for distribution")
  else
    return(fun)
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


#' @export
plot.moment <- function(samples, ...){
  # NOTE: in samples there can be values where moment per definition is not defined
  # -> we exclude the resulting NA's and calculate mean only over valid moment values
  mean <- apply(samples, 1, mean.default, na.rm = TRUE)
  
  X <- attr(samples, "X") # covariates values
  covariates <- names(X)
  df <- as.data.frame(append(X, list(mean = mean)))
  
  switch( length(covariates), # how many covariates in model?
          { # 1: univariate plotting
            plot(df, main = deparse(substitute(samples)), type = "l", ...)
          },
          { # 2: bivariate plotting
            require(ggplot2)
            print( 
              ggplot(df, do.call(aes_string, as.list(structure(covariates, names = c("x","y"))))) + 
                geom_tile(aes(fill = mean)) +
                ggtitle(deparse(substitute(samples)))
            )
          },
          # more than 2 covariates we do not support
          stop("more than 2 covariates in model -> we do not know how to plot")
  )
}


#' @export
"[.moment" <- function(samples, ...){
  match <- with(attr(samples, "X"), ...)
  # subset by row
  class(samples) <- "matrix"
  sel_samples <- samples[match, , drop = FALSE]
  
  # set again classes and attributes
  class(sel_samples) <- c("moment", class(samples))
  attr(sel_samples, "X") <- lapply(attr(samples, "X"), "[", match, drop = FALSE)
  
  return(sel_samples)
}

#' @export
lines.moment <- function(samples, ...){
  X <- attr(samples, "X") # get covariates
  len <- lapply(X, function(covariate) length(unique(covariate)))
  len_greater_one <- len > 1
  
  if( sum(len_greater_one) > 1 )
    stop("subset 'moment' by [...] operator so only one covariate is varying (other must be fixed)")
  
  means <- apply(samples, 1, mean, na.rm = TRUE)
  quantiles <- apply(samples, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  graphics::matplot( X[[ which(len_greater_one) ]], t(rbind(means, quantiles)), 
                     type = "l", lty = c(1,2,2), col = "black",
                     xlab = names(X)[len_greater_one],
                     ylab = "5% - mean - 95%",
                     main = deparse(substitute(samples)) )
  
}

