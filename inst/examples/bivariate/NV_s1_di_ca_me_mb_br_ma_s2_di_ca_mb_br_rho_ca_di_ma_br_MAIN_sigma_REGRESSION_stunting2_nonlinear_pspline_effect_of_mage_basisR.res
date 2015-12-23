BayesX.design.matrix<-function(x, ...) {
  require("splines")
  x <- unlist(x)
  knots <- c(6.84211, 8.77474, 10.7074, 12.64, 14.5726, 16.5053, 18.4379, 20.3705, 22.3032, 24.2358, 26.1684, 28.1011, 30.0337, 31.9663, 33.8989, 35.8316, 37.7642, 39.6968, 41.6295, 43.5621, 45.4947, 47.4274, 49.36, 51.2926, 53.2253, 55.1579)
  degree <- 3
  ll <- knots[degree + 1]
  ul <- knots[length(knots) - degree]
  degree <- degree + 1
  n <- length(x)
  ind <- x <= ul & x >= ll
  if(sum(ind) == n) {
    X <- spline.des(knots, x, degree)$design
  } else {
    D <- spline.des(knots, c(ll, ll, ul, ul), degree, c(0, 1, 0, 1))$design
    X <- matrix(0, n, ncol(D))
    X[ind, ] <- spline.des(knots, x[ind], degree)$design
    ind <- x < ll
    if(sum(ind) > 0) 
    X[ind, ] <- cbind(1, x[ind] - ll) %*% D[1:2, ]
    ind <- x > ul
    if(sum(ind) > 0)
    X[ind, ] <- cbind(1, x[ind] - ul) %*% D[3:4, ]
  }
  attr(X, "type") <- "ps"
  return(X)
}
