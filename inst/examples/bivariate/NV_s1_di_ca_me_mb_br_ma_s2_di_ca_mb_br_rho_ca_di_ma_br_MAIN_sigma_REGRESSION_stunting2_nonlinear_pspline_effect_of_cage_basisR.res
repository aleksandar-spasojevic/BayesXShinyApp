BayesX.design.matrix<-function(x, ...) {
  require("splines")
  x <- unlist(x)
  knots <- c(-5.98684, -4.10789, -2.22895, -0.35, 1.52895, 3.40789, 5.28684, 7.16579, 9.04474, 10.9237, 12.8026, 14.6816, 16.5605, 18.4395, 20.3184, 22.1974, 24.0763, 25.9553, 27.8342, 29.7132, 31.5921, 33.4711, 35.35, 37.2289, 39.1079, 40.9868)
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
