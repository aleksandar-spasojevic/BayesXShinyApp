BayesX.design.matrix<-function(x, ...) {
  require("splines")
  x <- unlist(x)
  knots <- c(7.76316, 9.23411, 10.7051, 12.176, 13.6469, 15.1179, 16.5888, 18.0598, 19.5307, 21.0017, 22.4726, 23.9436, 25.4145, 26.8855, 28.3564, 29.8274, 31.2983, 32.7693, 34.2402, 35.7112, 37.1821, 38.6531, 40.124, 41.5949, 43.0659, 44.5368)
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
