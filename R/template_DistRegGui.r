# 
# #0. Data generating process (just for reproducability)
# #   Estimation of the model in BayesX will deliver the output that
# #   will be given by the user.
# 
# set.seed(34234)
# #sample size
# n <- 1000
# #covariates
# x1 <- runif(n)
# x2 <- runif(n)
# 
# #nonlinear functions with centering
# f1 <-  2*x2^2
# f1 <- f1-mean(f1)
# f2 <- sin(x2)
# f2 <- f2-mean(f2)
# #predictors
# eta1 <- 0.5+1*x1+f1
# eta2 <- 0-x1+f2
# eta3 <- 0.3
# #parameters, all response functions are exp()
# a <- exp(eta1)
# b <- exp(eta2)
# p <- exp(eta3)
# #draw y's from F
# y <- VGAM::rdagum(n,scale=b,shape1.a=a,shape2.p=p)
# #save data set
# write.table(data.frame(y=y,x1=x1,x2=x2), "data.raw", quote=FALSE,row.names=FALSE)
# 
# # run in BayesX template.prg (bayesX instructions)
# res <- bayesX("./inst/template.prg")
# 
# 
# #a) read output in R. general procedure: read all _sample.raw files (contain the coefficients beta, gamma)
# #   get design matrices X, Z
# #   construct predictors for all samples
# 
# #compute predictors in each sample step and on a grid of covariates
# #IMPORTANT: the evaluation at points x1 and x2 has later to be done according the users changes in the interface!!!
# #idea define grid of x1 and x2 and store values!!
# x1grid <- x2grid <- seq(0,1,length=100)
# 
# 
# #parameter a
# gammaa <- t(read.table("template_MAIN_a_REGRESSION_y_LinearEffects_sample.raw",
#                        header=TRUE)[,-1])
# Xa <- cbind(rep(1,length(x1grid)),x1grid)
# betaa <- t(read.table("template_MAIN_a_REGRESSION_y_nonlinear_pspline_effect_of_x2_sample.raw",
#                       header=TRUE)[,-1])
# source("template_MAIN_a_REGRESSION_y_nonlinear_pspline_effect_of_x2_basisR.res")
# Za <- BayesX.design.matrix(x2grid)
# 
# 
# #parameter b
# gammab <- t(read.table("template_MAIN_b_REGRESSION_y_LinearEffects_sample.raw",header=TRUE)[,-1])
# Xb <- cbind(rep(1,length(x1grid)),x1grid)
# betab<- t(read.table("template_MAIN_b_REGRESSION_y_nonlinear_pspline_effect_of_x2_sample.raw",header=TRUE)[,-1])
# source("template_MAIN_b_REGRESSION_y_nonlinear_pspline_effect_of_x2_basisR.res")
# Zb <- BayesX.design.matrix(x2grid)
# 
# #parameter p
# gammap <- t(read.table("template_MAIN_p_REGRESSION_y_LinearEffects_sample.raw",header=TRUE)[,-1])
# Xp <- cbind(rep(1,length(x1grid)))
# 
# 
# 
# eta1 <- apply(rbind(gammaa,betaa),MARGIN=2,FUN=function(x,Xa,Za){Xa%*%x[1:dim(Xa)[2]]+Za%*%x[(dim(Xa)[2]+1):length(x)]},Xa,Za)
# eta2 <- apply(rbind(gammab,betab),MARGIN=2,FUN=function(x,Xb,Zb){Xa%*%x[1:dim(Xb)[2]]+Zb%*%x[(dim(Xb)[2]+1):length(x)]},Xb,Zb)
# eta3 <- apply(rbind(gammap),MARGIN=2,FUN=function(x,Xp){Xp%*%x[1:dim(Xp)[2]]},Xp)
# 
# #compute parameters, later, response functions may change according to the distribution F!!
# 
# a <- exp(eta1)
# b <- exp(eta2)
# p <- exp(eta3)
# 
# # b) call functions for plotting.
# # example 1: plot y against f(y)
# #y should be chosen appropriately
# #TO BE DONE: use function ddagum to compute f(y) for all covariate combinations and for all samples using the parameters and ddagum
# # y <- seq(0,1,length=100)
# # dens <- function(y, param=list(a,b,p))
# #   {
# #   #to be implemented
# #   }
# # 
# # 
# # #compute posterior mean and quantiles
# # #TO BE DONE: compute mean and quantiles over the samples (columns of a, b, p)
# # resmean <-
# # respqu2p5 <-
# # #further quantiles should be possible
# 
# # #example 2: plot x1 against a/b
# # #IMPORTANT: a/b should be argument of function
# # #TO BE DONE: compute a/b for all covariate combinations and for all samples
# # func <- function(x)
# #   {
# #   return(a/b)
# #   }
# # 
# # #TO BE DONE: compute mean and quantiles over samples
# # 
# # 
# # #TO BE DONE: plot the results
