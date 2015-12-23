BayesX.design.matrix<-function(x, ...) {
  require("splines")
  x <- unlist(x)
  knots <- c(-3.76316, -2.58211, -1.40105, -0.22, 0.961053, 2.14211, 3.32316, 4.50421, 5.68526, 6.86632, 8.04737, 9.22842, 10.4095, 11.5905, 12.7716, 13.9526, 15.1337, 16.3147, 17.4958, 18.6768, 19.8579, 21.0389, 22.22, 23.4011, 24.5821, 25.7632)
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
