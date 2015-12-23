#===================================================
#Malnutrition. Get plots of the paper for
#best fitting t-model (density plots also for best 
#fitting normal model.
#-----------------------------------------------
#Author: Nadja Klein, nklein@uni-goettingen.de
#first edited on 09.04.2014
#===================================================
library(BayesXsrc)
library(BayesX)


#please adapt accordingly!

#working directory
wd <- "./multivariat/"
#path where your bayesx-version is stored
# pathbayes <- paste("/home/c4031003/bayesx/bayesx/trunk/BayesX", sep = "")
#path of textfiles
pathfiles <- paste(wd, "prg/", sep = "")
#path of data
pathdata <-  paste(wd, "data/", sep = "")



#path for plots
if(!file.exists(paste(wd, "plots", sep = ""))) {
	dir.create(paste(wd, "plots", sep = ""))
}
pathplots <-  paste(wd, "plots/", sep = "")

#path were best fitting models should be stored.
pathbest <- paste(wd, "results/", sep = "")
#===================================================

m <- read.bnd(paste(pathdata, "india_dist_sort.bnd", sep = ""))


prename <- "NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br"

# mage <- read.table(paste(pathbest, prename, "_MAIN_RHO_REGRESSION_stunting2_nonlinear_pspline_effect_of_mage.res", sep = ""),header = TRUE)
cage <- read.table(paste(pathbest, prename, "_MAIN_RHO_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage.res", sep = ""),header = TRUE)
# breastfeeding <- read.table(paste(pathbest, prename, "_MAIN_RHO_REGRESSION_stunting2_nonlinear_pspline_effect_of_breastfeeding.res", sep = ""),header = TRUE)

mage_mean <- mage$pmean

mage_pqu2p5 <- mage$pqu2p5

mage_pqu10 <- mage$pqu10

mage_pqu97p5 <- mage$pqu97p5

mage_pqu90 <- mage$pqu90

cage_mean <- cage$pmean

cage_pqu2p5 <- cage$pqu2p5

cage_pqu10 <- cage$pqu10

cage_pqu97p5 <- cage$pqu97p5

cage_pqu90 <- cage$pqu90

breastfeeding_mean <- breastfeeding$pmean

breastfeeding_pqu2p5 <- breastfeeding$pqu2p5

breastfeeding_pqu10 <- breastfeeding$pqu10

breastfeeding_pqu97p5 <- breastfeeding$pqu97p5

breastfeeding_pqu90 <- breastfeeding$pqu90

xbreastfeeding <- breastfeeding$breastfeeding
xmage <- mage$mage
xcage <- cage$cage

data <- read.table(paste(pathdata, "india.raw", sep = ""),header = TRUE)

x1 <- data$cage
x2 <- data$breastfeeding
x3 <- data$mage

y1 <- data$stunting2
y2 <- data$wasting2

x1seg <- sort(unique(x1))
densx1seg <- as.numeric(table(x1))
x2seg <- sort(unique(x2))
densx2seg <- as.numeric(table(x2))
x3seg <- sort(unique(x3))
densx3seg <- as.numeric(table(x3))

#------------------------------------------------------------------------------------------------------------------------------------------
#from here quantile plots for four ages (and all other effects kept constant) Normal distribution
#IMPORTANT: this is just an example, on what value we set them. The ones with value=0 are not considered, since they do not occur in the model.
constc <-  -0.212813 
consts1 <- 0.353587
consts2 <- 0.0983864
constmu1 <-  -1.58066
constmu2 <-   -0.759399 
  
mu1breast <- 0.0764103238630642#(=14)
mu2breast <- 0.0267177240483872 #(=14)
s1breast <- -0.020727147602699 #(=14)
s2breast <- 0 #not selected
cbreast <- -0.0272990468670914 #(=14)

mu1mbmi <- -0.0648653561653808 #19.78
mu2mbmi <- -0.0187110625208044 #19.78
s1mbmi <- -0.0158690338708206 #19.78
s2mbmi <- -0.00494726081641721 #19.78
cmbmi <- 0 #not selected

mu1mage <- 0.0366118129861379 #25
mu2mage <- 0.0410698722033482 #25
s1mage <- -0.0142791457898089 #25
s2mage <- 0# not a candidate
cmage <- 0.00191677314670419 #25

mu1medu <- -0.193673608284175 #4
mu2medu <- -0.0834645805119948 #4
s1medu <-  0.00980203314092041 #4
s2medu <-  0# not a candidate
cmedu <- 0 #not selected

mu1edupartner <- -0.097845501678576 #7
mu2edupartner <- -0.031765485596367 #7
s1edupartner <- 0 # not a candidate
s2edupartner <- 0 #not a candidate
cedupartner <- 0 #not a candidate

mu1spat <- 0.301466514040649 #84
mu2spat <-  0.108750790061042 #84
s1spat <- 0.209084471591632 #84
s2spat <- 0.219863953523481#84
cspat <- -0.064136914814861#84

#read samples of age effect for all distribution parameters
cage <- t(read.table(paste(pathbest, prename, "_MAIN_RHO_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_sample.raw", sep = ""), header = TRUE)[,-1])
mu1age <- t(read.table(paste(pathbest, prename, "_MAIN_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_sample.raw", sep = ""), header = TRUE)[,-1])
mu2age <- t(read.table(paste(pathbest, prename, "_MAIN_MU_REGRESSION_wasting2_nonlinear_pspline_effect_of_cage_sample.raw", sep = ""), header = TRUE)[,-1])
s1age <- t(read.table(paste(pathbest, prename, "_MAIN_SCALE_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_sample.raw", sep = ""), header = TRUE)[,-1])
s2age <- t(read.table(paste(pathbest, prename, "_MAIN_SCALE_REGRESSION_wasting2_nonlinear_pspline_effect_of_cage_sample.raw", sep = ""), header = TRUE)[,-1])

xcage <-read.table(paste(pathbest, prename, "_MAIN_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_basisR.res", sep = ""), header = TRUE)$cage
xcage.part <- c(3,6,12,24)

source(paste(pathbest, prename, "_MAIN_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_basisR.res", sep = ""))
Z <- BayesX.design.matrix(xcage.part)


#mu1 has identity link, eta=mu
fun <- function(samples) {
  a <- ( (Z%*%(mu1age[,samples])) + mu1breast + mu1mage + mu1mbmi + mu1medu + mu1edupartner + constmu1 + mu1spat )
  return(a)    							  
}

mu1 <- lapply(1:1000,FUN=fun)

#mu2 has identity link, eta=mu
fun <- function(samples) {
  a <- ( (Z%*%(mu2age[,samples])) + mu2breast + mu2mage + mu2mbmi + mu2medu + mu2edupartner + constmu2 + mu2spat )
  return(a)      						  
}

mu2 <- lapply(1:1000,FUN=fun)


#sigma1 has exp link, exp(eta)=sigma
fun <- function(samples) {
  a <- exp( (Z%*%(s1age[,samples])) +s1breast + s1mage + s1mbmi + s1medu + s1edupartner + consts1 + s1spat )
  return(a)      						  
}

s1 <- lapply(1:1000,FUN=fun)

#sigma2 has exp link, exp(eta)=sigma
fun <- function(samples) {
  a <- exp( (Z%*%(s2age[,samples])) +s2breast + s2mage + s2mbmi + s2medu + s2edupartner + consts2 + s2spat )
  return(a)        					  
}

s2 <- lapply(1:1000,FUN=fun)

#correlation parameter has link eta/sqrt(1+eta^2)=rho
fun <- function(samples) {
  a <- ( (Z%*%(cage[,samples])) +cbreast + cmage + cmbmi + cmedu + cedupartner + constc + cspat )
  a <- a/(sqrt(1+a^2))
  return(a)          				  
}

cor <- lapply(1:1000,FUN=fun)


#define a sequence of grids of y's (within the ranges of stunting2, wasting2)
y1.seq <- seq(-6,6,by=0.1)
y2.seq <- seq(-6,6,by=0.1)

#the covariance matrix for one exemplay sample is computed as follows:
#matrix(c(s1[i]^2,cor[i]*s1[i]*s2[i],cor[i]*s1[i]*s2[i],s2[i]^2),ncol=2)
#TODO for you: use dmvnorm from package mvtnorm to compute the densities.
#IMPORTANT: The following is only working if we had only 1 MCMC samples and mu1, mu2 and so on would be only vectors!!!
#library("mvtnorm")
#res <- list()
#par(mfrow=c(2,2))
#for(i in 1:length(xcage.part)) {
 # density <- matrix(0,ncol=length(y2.seq),nrow=length(y1.seq))
 # for(j in 1:length(y1.seq)) {
 #   for(k in 1:length(y2.seq)) {
 #     mu <- c(mu1[i],mu2[i])
 #     y <- c(y1.seq[j],y2.seq[k])
 #     sig <- matrix(c(s1[i]^2,cor[i]*s1[i]*s2[i],cor[i]*s1[i]*s2[i],s2[i]^2),ncol=2)
 #     density[j,k] <- dmvnorm(y,mean=mu,sigma=sig)
 #   }
 # }
 # res[[i]] <- density
 # contour(y1.seq,y2.seq,density,drawlabels=TRUE)
 ## points(y1,y2)
#}

# postscript(paste(pathbest, prename, "_quantilecurves.eps", sep = ""))
# par(mfrow=c(2,2),mar=c(5.1,6.1,4.1,2.1)-1)
for(i in 1:length(xcage.part)) {
  # density <- res[[1]]
# plot(y1[which(data$cage<=4.5)],y2[which(data$cage<=4.5)],xlab="stunting",ylab="wasting",main="age of child = 3 months",xlim=c(-6,6),ylim=c(-6,6),cex.main=1.5,cex.lab=1.5,cex.axis=1.5,col="grey")
  # contour(y1.seq,y2.seq,density,labels=seq(0.01,0.07,by=0.01),add=TRUE,levels=seq(0.01,0.07,by=0.01),nlevels=7,method="flattest")
# legend("bottomleft",legend="children with ages between 0 and 4.5 months",col="darkgrey",pch=1,pt.lwd=2,bty="n")
# textt <- bquote(paste(mu[stunting], " = ", .(round(mu1[1], digits = 2)), sep = ""))
# text(3.3, 4.8, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(mu[wasting], " = ", .(round(mu2[1], digits = 2)), sep = ""))
# text(3.3, 4.0, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[stunting], " = ", .(round(s1[1], digits = 2)), sep = ""))
# text(3.3, 3.2, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[wasting], " = ", .(round(s2[1], digits = 2)), sep = ""))
# text(3.3, 2.4, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(rho, " = ", .(round(cor[1], digits = 2)), sep = ""))
# text(3.3, 5.6, textt, cex = 1.1,adj = c(0,NA))

# density <- res[[2]]
# plot(y1[which(data$cage>=4.5&data$cage<=9)],y2[which(data$cage>=4.5&data$cage<=9)],xlab="stunting",ylab="wasting",main="age of child = 6 months",xlim=c(-6,6),ylim=c(-6,6),cex.main=1.5,cex.lab=1.5,cex.axis=1.5,col="grey")
# contour(y1.seq,y2.seq,density,labels=seq(0.01,0.07,by=0.01),add=TRUE,levels=seq(0.01,0.07,by=0.01),nlevels=7,method="flattest")
# legend("bottomleft",legend="children with ages between 4.5 and 9 months",col="darkgrey",pch=1,pt.lwd=2,bty="n")
# textt <- bquote(paste(mu[stunting], " = ", .(round(mu1[2], digits = 2)), sep = ""))
# text(3.3, 4.8, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(mu[wasting], " = ", .(round(mu2[2], digits = 2)), sep = ""))
# text(3.3, 4.0, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[stunting], " = ", .(round(s1[2], digits = 2)), sep = ""))
# text(3.3, 3.2, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[wasting], " = ", .(round(s2[2], digits = 2)), sep = ""))
# text(3.3, 2.4, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(rho, " = ", .(round(cor[2], digits = 2)), sep = ""))
# text(3.3, 5.6, textt, cex = 1.1,adj = c(0,NA))

# density <- res[[3]]
# plot(y1[which(data$cage<=15&data$cage>=9)],y2[which(data$cage<=15&data$cage>=9)],xlab="stunting",ylab="wasting",main="age of child = 9 months",xlim=c(-6,6),ylim=c(-6,6),cex.main=1.5,cex.lab=1.5,cex.axis=1.5,col="grey")
# contour(y1.seq,y2.seq,density,labels=seq(0.01,0.07,by=0.01),add=TRUE,levels=seq(0.01,0.07,by=0.01),nlevels=7,method="flattest")
# legend("bottomleft",legend="children with ages between 9 and 15 months",col="darkgrey",pch=1,pt.lwd=2,bty="n")
# textt <- bquote(paste(mu[stunting], " = ", .(round(mu1[3], digits = 2)), sep = ""))
# text(3.3, 4.8, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(mu[wasting], " = ", .(round(mu2[3], digits = 2)), sep = ""))
# text(3.3, 4.0, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[stunting], " = ", .(round(s1[3], digits = 2)), sep = ""))
# text(3.3, 3.2, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[wasting], " = ", .(round(s2[3], digits = 2)), sep = ""))
# text(3.3, 2.4, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(rho, " = ", .(round(cor[3], digits = 2)), sep = ""))
# text(3.3, 5.6, textt, cex = 1.1,adj = c(0,NA))

# density <- res[[4]]
# plot(y1[which(data$cage>=15&data$cage<=36)],y2[which(data$cage>=15&data$cage<=36)],xlab="stunting",ylab="wasting",main="age of child = 24 months",xlim=c(-6,6),ylim=c(-6,6),cex.main=1.5,cex.lab=1.5,cex.axis=1.5,col="grey")
# contour(y1.seq,y2.seq,density,labels=seq(0.01,0.07,by=0.01),add=TRUE,levels=seq(0.01,0.07,by=0.01),nlevels=7,method="flattest")
# legend("bottomleft",legend="children with ages between 15 and 36 months",col="darkgrey",pch=1,pt.lwd=2,bty="n")
# textt <- bquote(paste(mu[stunting], " = ", .(round(mu1[4], digits = 2)), sep = ""))
# text(3.3, 4.8, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(mu[wasting], " = ", .(round(mu2[4], digits = 2)), sep = ""))
# text(3.3, 4.0, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[stunting], " = ", .(round(s1[4], digits = 2)), sep = ""))
# text(3.3, 3.2, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(sigma[wasting], " = ", .(round(s2[4], digits = 2)), sep = ""))
# text(3.3, 2.4, textt, cex = 1.1,adj = c(0,NA))
# textt <- bquote(paste(rho, " = ", .(round(cor[4], digits = 2)), sep = ""))
# text(3.3, 5.6, textt, cex = 1.1,adj = c(0,NA))

}
# dev.off()
