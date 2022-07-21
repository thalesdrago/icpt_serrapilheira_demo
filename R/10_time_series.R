####### Written by Thales in July 2022
####### Based in the script given by Sara Mortara
####### Written during the course Introduction to Scientific Computation
####### Module: Time-series data
####### Instructor: Sara Mortara
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script provides an introduction on how to deal with time-series data

### Loading packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(lubridata) # check the documentation to see function that help when dealing with date and date formats

# Importing data
covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")


# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date)

# Checking the class
class(covid$date)

# Now we can make numeric operations
range(covid$date)

# Ploting the time-series
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

# Oops. We have negative cases and will substitute the negative values per zero.
# We are doing this as an exercise, we would need to know more about the data set
# in order to be sure what this negative values mean
covid$new_confirmed[covid$new_confirmed < 0] <- 0

# Ploting the cleaned
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases")

# Changing which dates are displayed, changing the breaches, using scale_x_date()
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases") +
  scale_x_date(breaks = "4 months")

# Changing the date format to year - month
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases") +
  scale_x_date(breaks = "4 months", date_labels = "%Y-%m")

# Creating a rolling mean to get rid of the peaks and facilitate the visualization

covid$roll_mean <- zoo::rollmean(covid$new_confirmed, 14, fill = NA)

head(covid)

# Ploting the raw data with the rolling mean
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases") +
  scale_x_date(breaks = "4 months", date_labels = "%Y-%m") +
  geom_line(aes(x= date, y = roll_mean), col ="red", size = 1.5)


