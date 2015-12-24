library(BayesXShinyApp)
output_path <- system.file("examples/univariate", package = "BayesXShinyApp")

# load grid where parameters should be computed on
load( paste0(output_path, "/X.Rdata") )

# compute parameters
params <- Parameters(output_path, X = X)

# subset parameters by grid, so we calculate densities on one observation 
# (one node in grid) - otherwise it takes too long to calculate
# for example we have 10'000 nodes on grid, we have 1000 samples per node and
# we have 300 (= y) values to compute density on them, resulting in 
# 10'000 * 1000 * 300 = 3 * 10^9 calculations -> a normal computer cannot handle
# this amount of data!
param <- params[x1 == 0 & x2 == 1]

# calculate density and moment for selected parameter sample
density <- density(param, x = seq(0.01, 10, length.out = 300))
# for moments one can use all parameters, since it does not involve a lot
# computation (caution: depends on X, if X is big, this can cause long 
# computation time)
mu <- mean(params)
variance <- var(params)
median <- median(params)

# plot density with quantiles
plot(density)

# plot densities resulted in quantiles above
matplot(density)

# plot moments
plot(mu)
plot(variance)
plot(median)

# line plot when all covariates but one are fixed
lines( mu[x1 == 1] )
