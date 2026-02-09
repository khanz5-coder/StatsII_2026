#####################
# load libraries
# set wd
# clear global .envir
#####################
  
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
#creating data
set.seed(123)
data <- (rcauchy(1000, location = 0, scale = 1))
#creating the function
KSpval <- function(data) {
  k <- length(data) #initalizing for iteration.
  ECDF <- ecdf(data) 
  empiricalCDF <- ECDF(data)
  d_value <- max(abs(empiricalCDF - pnorm(data))) #test statistic
  i <- 1:k #taking a vectorized approach instead of a loop for calculating the terms
  terms<- exp(-(((2*i - 1)^2) * pi^2)/(8*(d_value^2))) #calculating the terms for all 100 k's
  d_obs <- sqrt(2 * pi)/d_value * sum(terms) #calculating the observation 
return(d_obs)
}
KSpval(data)
# [1] 5.652523e-29



#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
#Plotting the data:
plot(data$x, data$y, ylab = "Y", xlab = "X")
#creating the linear likelihood function
linear.lik <- function(theta, y, X) { #creating a log of likelihood function
  n <-  nrow(X) 
  k <- ncol(X)
  beta <- theta[1:k] 
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*%e)/ (2*sigma2) )
return(-logl) 
}
#running with optim.
MLEt <- optim(fn=linear.lik, y=data$y, X = cbind(1, data$x), par=c(1,1,1), hessian=T, method ="BFGS")
MLEt$par  
#[1]  0.1429829  2.7263116 -1.4423360
linear <- summary(lm(data$y ~ data$x)) 
# (Intercept)  0.13919
# data$x       2.72670

