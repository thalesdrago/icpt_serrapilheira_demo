####### Written by Thales in July 2022
####### Based in the script given by Sara Mortara
####### Written during the course Introduction to Scientific Computation
####### Module: Populational models - Sara
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script implements basic population models: logistic growth
####### and Lotka-Volterra competition models


library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data


##################### Logistic growth

# Calling the logistic growth function
source("./func/logGrowth.R")

# named vector with parameters
p <- c(r = 1, a = 0.001)
# initial condition
y0 <- c(N = 10)
# time steps
t <- seq(1, 100, length.out = 400)

# give the function and the parameters to the ode function
p <- c(r = 1, a = 0.000001)
out_log1 <- ode(y = y0, times = t, func = logGrowth, parms = p)
p <- c(r = 1.5, a = 0.000001)
out_log2 <- ode(y = y0, times = t, func = logGrowth, parms = p)
p <- c(r = 2, a = 0.000001)
out_log3 <- ode(y = y0, times = t, func = logGrowth, parms = p)
p <- c(r = 0.5, a = 0.000001)
out_log4 <- ode(y = y0, times = t, func = logGrowth, parms = p)
p <- c(r = 0.2, a = 0.000001)
out_log5 <- ode(y = y0, times = t, func = logGrowth, parms = p)


# Converting to a data.frama before plotting
# deSolve creats a deSolve matrix, and ggplot works better with dataframes
df_log1 <- as.data.frame(out_log1)
df_log2 <- as.data.frame(out_log2)
df_log3 <- as.data.frame(out_log3)
df_log4 <- as.data.frame(out_log4)
df_log5 <- as.data.frame(out_log5)


ggplot() +
  geom_line(data = df_log1,aes(x = time, y = N), col ="red") +
  geom_line(data = df_log2, aes(x = time, y = N), col ="blue") +
  geom_line(data = df_log3, aes(x = time, y = N), col ="green") +
  geom_line(data = df_log4,aes(x = time, y = N), col ="yellow") +
  geom_line(data = df_log5, aes(x = time, y = N), col ="purple") +
  theme_classic()




##################### Lotka-Volterra competition model
source("./func/LV_competition.R")


# LV parameters
a <- matrix(c(0.04, 0.04, 0.01, 0.03), nrow = 2)
r <- c(0.1, 0.1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)


out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv)
summary(out_lv)

# The data output of ode is not tidy, since we have two columns with information about N
# We need to create a new column for species and a colpase the information about N in one column
# For this we can use the pivot_longer function, but first the data must be in data.frame format
df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3)

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()

