#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base", "package:stargazer")
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

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

str(climateSupport) #unordering data set for ease of analysis
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)

m1 <- glm( #running an additive model
  choice ~ countries + sanctions,
  data = climateSupport,
  family = "binomial")

summary(m1) #summarizing
stargazer(m1)

m_null <- glm(choice ~ 1, data = climateSupport, family = binomial) 
#running a null model
anova(m_null, m1, test = "LRT") 
# Analysis of Deviance Table
# Model 1: choice ~ 1
# Model 2: choice ~ countries + sanctions
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1      8499      11783                          
# 2      8494      11568  5   215.15 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#2a
diff_s5_15 <- coef(m1)["sanctions15%"] - coef(m1)["sanctions5%"]
# -0.3251028 
or <- exp(diff_s5_15)
# 0.7224531 
orpercent <- (or - 1) * 100
# -27.75469 

#2c
#computing for 80/192 country participation and 0 sanctions
odds_80 <- coef(m1)["(Intercept)"] + coef(m1)["countries80 of 192"]
#calculating the probablity 
probability_80 <- exp(odds_80) / (1 + exp(odds_80))
#checking the result. 
probability_80
# 0.5159191 

odds5 <- odds5 - 1
Q2C <- odds5
# 0.3998442

m_interaction <- glm(choice ~ countries * sanctions,
                     family = binomial, data = climateSupport)

summary(m_interaction)
stargazer(m_interaction)
#creating a model with an interactive term
anova(m1, m_interaction, test = "LRT")

# Analysis of Deviance Table
# Model 1: choice ~ countries + sanctions
# Model 2: choice ~ countries * sanctions
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1      8494      11568                     
# 2      8488      11562  6   6.2928   0.3912
