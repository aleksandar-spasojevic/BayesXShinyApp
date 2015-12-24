library(BayesXShinyApp)
# with path
output_path <- system.file("examples/bivariate", package = "BayesXShinyApp")

# load grid where parameters should be computed on
load( paste0(output_path, "/X.Rdata") )

# compute parameters
params <- Parameters(output_path, X = X)

# subset parameters by grid, so we calculate densities on one observation 
# (one node in grid) - otherwise it takes too long to calculate
param <- params[csex == 0 & cage == 9]

# since the distribution is bivariate, we need y1 and y2 (naming doesn't matter)
y1 <- seq(-6, 6, length.out = 100)
y2 <- seq(-6, 6, length.out = 100)
grid <- structure(expand.grid(y1,y2), names = c("y1","y2"))

# calculate density on grid 
dens <- density(param, x = grid)
# for moments one can use all parameters, since it does not involve a lot
# computation (caution: depends on X, if X is big, this can cause long 
# computation time)
mu <- mean(params)
variance <- var(params)
correlation <- cor(params)

# plot density with quantiles; since it is a bivariate distribution we make a
# contour plot (plot function decides automagically)
plot(dens, bins = 10)

# since bivariate, matplot not defined!
lines(mu[cage == 9])
lines(mu[cage == 19 & csex == 1])
lines(variance[csex == 0])
lines(correlation[cage == 9])


# one can even plot UNION distribution of cage = {1, 9, 19}
dens2 <- density(params[csex == 1], x = grid)
plot(dens2)
