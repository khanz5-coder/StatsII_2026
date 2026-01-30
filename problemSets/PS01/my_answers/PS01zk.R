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
KSpval <- function(data) {
  data <- data[[1]] #explicitly assuming one column to prevent ecdf error 
  k <- length(data) #setting the length of K
  ECDF <- ecdf(data) #creating empirical distribution of observed data
  empiricalCDF <- ECDF(data) #creating empirical distribution
  d_value <- max(abs(empiricalCDF - pnorm(data))) #test statistic
  i <- 1:k #taking a vectorized approach instead of a loop for calculating the terms
  terms <- exp(-((2*i - 1)^2) * pi^2)/(8*(d_value^2)) #calculating the terms for all 100 k's
  d_obs <- sqrt(2 * pi)/d_value * sum(terms) #calculating the observation
return(d_obs)
}

# [1] 0.0006529358



#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)