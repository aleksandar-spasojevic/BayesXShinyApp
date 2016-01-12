
.distributions <- list(
  
  # ----------------------------------------------------------------------------
  # Dagum Distribution
  # ---------------------------------------------------------------------------- 
  dagum = structure(
    list(
      density = function(a_1, p_1, b_1, ...) {
        return( VGAM::ddagum(shape1.a = a_1, shape2.p = p_1, scale = b_1, ...) )
      },
      class = "univariate",
      link = exp,
      moment = list(
        # only functions here!
        # see wikipedia: https://en.wikipedia.org/wiki/Dagum_distribution
        
        # mean function
        mean = function(a_1, b_1, p_1, ...) {
          a <- a_1; b <- b_1; p <- p_1
          
          mean <- (-b/a) * gamma(-1/a) * gamma(1/a + p) / gamma(p)
          # if a <= 1 then mean not defined!
          mean[ a <= 1 ] <- NA
          return( list("mean" = mean) )
        },
        
        # median function
        median = function(a_1, b_1, p_1, ...) {
          a <- a_1; b <- b_1; p <- p_1
          
          list("median" = b * (-1 + 2^{1/p})^(-1/a))
        },
        
        # mode function
        mode = function(a_1, b_1, p_1, ...) {
          a <- a_1; b <- b_1; p <- p_1
          
          list("mode" = b * ((a*p - 1)/(a + 1))^(1/a))
        },
        
        # variance function
        var = function(a_1, b_1, p_1, ...) {
          a <- a_1; b <- b_1; p <- p_1
          var <- -((b/a)^2) * ( 2*a * gamma(-2/a)*gamma(2/a+p)/gamma(p) + (gamma(-1/a)*gamma(1/a + p)/gamma(p))^2 )
          # if a <= 2 then variance not defined!
          var[ a <= 2 ] <- NA
          return( list("var" = var) )
        }
      )
    ), class = c("distribution", "list")),
  
  # ----------------------------------------------------------------------------
  # Bivariate Normal Distribution
  # ----------------------------------------------------------------------------
  bivnormal = structure(
    list(
      density = function(mu_1, mu_2, 
                         sigma_1, sigma_2, 
                         rho_1, 
                         ...) {
        mean <- c(mu_1, mu_2)
        cov <- diag(c(sigma_1, sigma_2))
        cov[upper.tri(cov)] <- rho_1 * sqrt(sigma_1 * sigma_2)
        cov[lower.tri(cov)] <- cov[upper.tri(cov)]
        return( mvtnorm::dmvnorm(mean = mean, sigma = cov, ...) )
      },
      class = "bivariate",
      
      # if you define link as named list, all regression must be defined
      link = list(mu_1 = function(eta) eta,
                  mu_2 = function(eta) eta,
                  sigma_1 = function(eta) exp(eta),
                  sigma_2 = function(eta) exp(eta),
                  rho_1 = function(eta) eta/sqrt(1 + eta^2)),
      
      moment = list(
        # mean function
        mean = function(mu_1, mu_2, ...) {
          return( list("mu_1" = mu_1,
                       "mu_2" = mu_2) )
        },
        
        # variance function
        var = function(sigma_1, sigma_2, ...) {
          return( list("sigma_1" = sigma_1,
                       "sigma_2" = sigma_2) )
        },
        
        # correlation function
        cor = function(rho_1, ...){
          return( list("rho_1" = rho_1) )
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


cor.distribution <- function(distr, ...){
  fun <- distr$moment$cor
  
  if( is.null(fun) )
    stop("no correlation function defined for distribution")
  else
    return(fun)
}


#' @export
plot.univariate <- function(density, xlab = "", ...){
  quantiles <- t(apply(density, 1, stats::quantile, c(0.05, 0.95), na.rm = TRUE))
  x <- c(attr(density, "x"), rev(attr(density, "x")))
  plot.default(x, quantiles, type = "n", ...)
  polygon(x, c(quantiles[,1], rev(quantiles[,2])), col = "lightgrey")
  
  means <- apply(density, 1, mean, na.rm = TRUE)
  lines(attr(density, "x"), means, col = "blue")
}


#' @export
matplot <- function(density, ...) UseMethod("matplot")
#' @export
matplot.univariate <- function(density, xlab = "", ...){
  graphics::matplot(attr(density, "x"), density, 
                    type = "l", col = "grey", lty = 1, xlab = xlab,
                    ...)
}

#' @export
plot.bivariate <- function(density, ...){
  mean <- apply(density, 1, base::mean.default, na.rm = TRUE)
  x <- attr(density, "x")
  df <- as.data.frame(append(x, list(mean = mean)))
  covariates <- names(df)
  
  print( ggplot(df, do.call(aes_string, 
                            as.list(structure(covariates, 
                                              names = c("x","y","z"))))) + 
           stat_contour(...)
  )
}


#' @export
plot.moment <- function(samples, ...){
  # NOTE: in samples there can be values where moment per definition is not defined
  # -> we exclude the resulting NA's and calculate mean only over valid moment values
  mean <- lapply(samples, rowMeans, na.rm = TRUE)
  
  X <- attr(samples, "X") # covariates values
  covariates <- names(X)
  df <- as.data.frame(append(X, structure(mean, names = "mean")))
  
  switch( length(covariates), # how many covariates in model?
          { # 1: univariate plotting
            return( plot(df, main = deparse(substitute(samples)), type = "l", ...) )
          },
          { # 2: bivariate plotting
            return(
              print( 
                ggplot(df, do.call(aes_string, as.list(structure(covariates, names = c("x","y"))))) + 
                  stat_contour(aes(z = mean, geom = "polygon", fill = ..level..)) +
                  ggtitle(deparse(substitute(samples)))
              ))
          }
  )
  
  # more than 2 covariates we do not support
  stop("more than 2 covariates in model -> we do not know how to plot")
}


#' @export
"[.moment" <- function(samples, ...){
  match <- with(attr(samples, "X"), ...)
  # subset by row
  # class(samples) <- "matrix"
  # sel_samples <- samples[match, , drop = FALSE]
  sel_samples <- lapply(samples, "[", match, , drop = FALSE)
  
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
  
  dimensions <- names(samples)
  
  par(mfrow = c(length(dimensions),1))
  for( dim in dimensions ){
    if( all(len_greater_one == FALSE) ){
      # we make boxplot: since all covariates unique!
      boxplot(as.vector(samples[[dim]]), 
              main = paste(dim, deparse(substitute(samples))),
              xlab = paste(names(X), X, sep = "=", collapse = ","), ...)
      
    } else if( is.factor(X[[ which(len_greater_one) ]]) ){
      # we make boxplot too but with many as factor levels in X
      boxplot(x = structure(t(samples[[dim]]), 
                            dimnames = list(NULL, X[[ which(len_greater_one) ]])),
              xlab = names(X)[len_greater_one],
              main = paste(dim, deparse(substitute(samples))),
              ...)
    } else {
      # we plot lines: since only one covariate varying
      means <- apply(samples[[dim]], 1, mean, na.rm = TRUE)
      quantiles <- apply(samples[[dim]], 1, stats::quantile, probs = c(0.05, 0.95), na.rm = TRUE)
      graphics::matplot( X[[ which(len_greater_one) ]], t(rbind(means, quantiles)), 
                         type = "l", lty = c(1,2,2), col = "black",
                         xlab = names(X)[len_greater_one],
                         ylab = "5% - mean - 95%",
                         main = paste(dim, deparse(substitute(samples))) )
    }
  }
}

print.moment <- function(moment, ...) str(as.matrix(moment))
