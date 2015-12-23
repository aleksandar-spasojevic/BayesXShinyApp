BayesX.design.matrix<-function(x, ...) {
  require("splines")
  x <- unlist(x)
  knots <- c(-0.170331, -0.116699, -0.0630673, -0.00943549, 0.0441963, 0.0978282, 0.15146, 0.205092, 0.258724, 0.312356, 0.365987, 0.419619, 0.473251, 0.526883, 0.580515, 0.634147, 0.687778, 0.74141, 0.795042, 0.848674, 0.902306, 0.955938, 1.00957, 1.0632, 1.11683, 1.17046)
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
