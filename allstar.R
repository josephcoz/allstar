wd <- as.character(getwd())
dirstr <-  paste0(wd, '/allstar.txt')
allstar <- read.table(file = dirstr, header = TRUE)

str(allstar)

# notice the high correlation b/w audience and BOS players
cor(allstar$aud, allstar$BOS.total)
plot(allstar$BOS.total, allstar$aud)

# summary starts of '.start' variables
summary(allstar["BOS.total"])
summary(allstar["ATL.total"])
summary(allstar["MIA.total"])
# BOS.total is highly correlated with aud because
# BOS.total is high on average

# Tukey: "Statisticians, like artist, have a tendency to fall in love with their models"
# Dr. Keith Vorkink 28 Mar 2017 BYU Devotional
# Hocking: 'If the goal is prediciton, we will do better with a "wrong" model'

# better to omit effects that would be poorly estimated

# In statistics if we see a contradiction b/w data and the model, we think the model is wrong
# In physics if we see a contradiction b/w data and the model, we think the data is wrong

#******************************************************************************************************************
# SUBSET REGRESSION / VARIABLE SELECTION

# Analysis
# Objective: Find the best model that has a subset of all available explanatory variables

# Choices for 'best' model:

# Trade-off b/w prediction performance and model parsimony; we would prefer a simpler model with less pred. performance

# R^2/Radj^2: percentage of variation that is explained by the model (want it close to 1)
# Problem: The way to "max R^2" is by using all the variables in the model

# Akaike Information Criterion (AIC) - incurs a 'penalty' for a complex model

# Bayes' Informatino Criterion (BIC) 

# Why don't we use 'statistically significant' to choose model?
# Are hypothesis tests on the resulting model fair?

#*****************************************************************************************************************
# Stepwise selection
# instead of looking at all the possible subsets, we will 'rank' subsets by AIC (or BIC), then choose

# backward elimination isn't possible because we have wide, skinny data

# will use forward selection: best two-variable model
small_model <- lm(aud/10^3 ~ +1, data=allstar)
big_model <- formula(lm(aud/10^3 ~ ., data=allstar))

allstar_out <- step(small_model, scope=big_model, direction="forward", steps=2) # we want subset of two
# direction: forward, backward, or stepwise

# what it looks like if we choose 5 variables instead
# allstar_out5 <- step(small_model, scope=big_model, direction="forward", steps=5)
# summary(allstar_out5)
# this model is overfit:
# has VERY high R-squared
# some of the variables just don't make sense

summary(allstar_out)

# graphic of effects
library(car)
crPlots(allstar_out)

