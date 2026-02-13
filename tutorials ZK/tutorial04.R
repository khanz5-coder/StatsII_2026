##################
#### Stats II ####
##################

###############################
#### Tutorial 4: Logit ####
###############################

# In today's tutorial, we'll begin to explore logit regressions
#     1. Estimate logit regression in R using glm()
#     2. Practice makes inferences using logit regression
#     3. Compare logit models

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

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt")

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.

#first we need to convert the data into factors that are character vectors 

variables <- c("hsgrad", "nonwhite", "mhs", "fhs", "intact")
graduation[variables] <- lapply(graduation[variables], factor)

#runnign the regression 
reg1 <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + nsibs + asvab + intact,
  data = graduation,
  family = "binomial")
# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
grad_null <- glm(hsgrad ~ 1, data = graduation, family = binomial)
anova(grad_null, reg1, test = "LRT")

#at least one of the variables in the model explain the variation in the model. 

# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
confint(reg1)
# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

# (b) The logistic regression in the previous problem assumes that the
#partial relationship between the log-odds of high-school graduation and number of siblings is linear.

graduation$nsib_f <- factor(graduation$nsibs)

m_factor <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + nsib_f + asvab + intact,
  data = graduation,
  family = "binomial")

anova(reg1, m_factor, test = "LRT")

unique(graduation$nsib_f)
table(graduation$nsib_f)

#after running we see that -3, 15, and 17 do not have a sufficent number of cases. 
grad_clean <- subset(graduation, nsibs >= 0)
grad_clean$nsib_f <- cut(
  grad_clean$nsibs,
  breaks = c(-1, 1, 3 ,5, 10, 20),
  labels <- c("0-1", "2-3", "4-5", "6-10", "11+")
)

unique(grad_clean$nsib_f)
table(grad_clean$nsib_f) 

m_factor2 <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + nsib_f + asvab + intact,
  data = grad_clean,
  family = "binomial")

m_f <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + nsibs + asvab + intact,
  data = grad_clean,
  family = "binomial")
summary(m_factor2)

anova(m_f, m_factor2, test = "LRT")
#since we have the same amountof variables, 
#assuming linearity improves the model, factorizing did not improve the model.
#taking it as a categorocal model did not do a good job therefore linearity is ideal. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?

