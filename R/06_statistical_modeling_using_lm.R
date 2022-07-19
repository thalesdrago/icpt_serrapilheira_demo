####### Written by Thales in July 2022
####### Based in the script given by Sara Mortara
####### Written during the course Introduction to Scientific Computation
####### Module: Statistical modeling - Sara
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script simulates the random movement, based on tumbles and runs, of an E. coli
####### and analyzes the resultant trajectories

# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Script to fit linear model in R
# First version 2022-07-18
# --------------------------------------------------#

# loading packages
library(ggplot2)

# reading data
cat <- read.csv("data/raw/crawley_regression.csv")

# Do leaf chemical compounds affect the growth of caterpillars? ----------------
# We already have an hypothesis, that is: tanin will negatively affect growth
# Sometimes we do not have an hypothesis, and what we do is more like variable selection

# the response variable
boxplot(cat$growth, col = "darkgreen")

# the predictor variable
unique(cat$tannin)

# creating the lm
mod_cat <- lm(growth ~ tannin, data = cat)

summary(mod_cat)


## ----lm-plot------------------------------------------------------------------
plot(growth ~ tannin, data = cat, bty = 'l', pch = 19)
abline(mod_cat, col = "red", lwd = 2)


## ----lm-ggplot----------------------------------------------------------------
ggplot(data = cat, mapping = aes(x = tannin, y = growth)) +
  geom_point() +
  geom_smooth(method = lm) + # specifying that our model is linear, if we do not do this, it will fit a smooth curve
  theme_classic()
# The shade silhuette is the conffidence interval of the line


## AOV table
summary.aov(mod_cat)


## fitted values
predict(mod_cat) #predicting the growth using the values of tanin and the model
cat$fitted <- predict(mod_cat)

# Comparing fitted vs. observed values
ggplot(data = cat) +
  geom_point(aes(x = growth, y = fitted)) +
  geom_abline(aes(slope = 1,  intercept = 0)) +
  theme_classic()


## Model diagnostics -----------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_cat)
par(mfrow = c(1, 1))
# The Residual vs Fitted plot tests the homogeneity of variance
# we expecet no pattern at all. If there was heterokedasticity, then this would look like a cone
# The Normal Q-Q plot tests if the residuals follow a normal distribution
# we expect to points to follow the line
# The Scale-Location plot shows how the variance of the residuals are distributed
# we expect a straight line
# The Residual vs Leverage plot shows the variance of the model in another way
# we expect that all points have a distance less then 2


# Comparing statistical distributions using fitdistrplus ------------------------------------------
library(fitdistrplus)

# Importing data
data("groundbeef")
?groundbeef
str(groundbeef)


plotdist(groundbeef$serving, histo = TRUE, demp = TRUE)

descdist(groundbeef$serving, boot = 1000)

fw <- fitdist(groundbeef$serving, "weibull")
summary(fw)

fg <- fitdist(groundbeef$serving, "gamma")
fln <- fitdist(groundbeef$serving, "lnorm")

par(mfrow = c(1, 1))
plot_legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot_legend)
qqcomp(list(fw, fln, fg), legendtext = plot_legend)
cdfcomp(list(fw, fln, fg), legendtext = plot_legend)
ppcomp(list(fw, fln, fg), legendtext = plot_legend)


gofstat(list(fw, fln, fg))
# The goodness-of-fit (GOF) statistics are only good if the tested distributions have a similar number of parameters
# This means that GOF statistics do not incorporate overfit, so it is possible that distributions with more parameters will many times surpass the ohter ones
# AIC and BIC take into account the number of parameters and incorporates the ideia of overfit
