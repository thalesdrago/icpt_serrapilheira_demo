####### Writting began in July 2022 by Thales
####### Based in the script given by Andrea Sanchez-Tapia
####### Written during the course Introduction to Scientific Computation
####### Module: Spatial data in R
####### Instructor: Andrea Sanchez-Tapia
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script provides an introduction on spatial data and analyses in R


# Classical packages used in R for spatial analysis raster, sf and tmap

library(sf)
library(tmap)
library(dplyr)
library(raster)
library(ggplot2)
data(World)
# package tmap has a syntax similar to ggplot. The functions start all with tm_. It has a sintax similar to ggplot2
tm_shape(World) +
  tm_borders()

# A simple examination with head(), str() or dplyr::glimpse() can tell us the class,
# dimensions, and other characteristics of object World.
# Features are the rows and the fields are the colunms
head(World)
names(World)
class(World)
dplyr::glimpse(World)

# What happens when you execute plot(World)?
# How would you plot only one of the variables? What happens when you execute
plot(World)
plot(World[,1]) # plot[i] or plot [,i] will plot the all features and for the i field
plot(World[1,]) # plot[i,] will plot the 1 feature and all i fields
plot(World["pop_est"])

#A key difference between data frames and sf objects is the presence of geometry,
# that looks like a column when printing the summary of any sf object
head(World[, 1:4])

# When calling World$geometry, however, we perceive it’s not a single column.
# Actually, geometries are a class in their own, sfc

class(World)
class(World$geometry)
World$geometry

head(sf::st_coordinates(World))

# We can tranform World to a data frame by taking geometry out
no_geom <- sf::st_drop_geometry(World)
class(no_geom)

# Getting the bouding boxes
st_bbox(World)

names(World)
unique(World$continent)

# Using dplyr sintax
World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()

# You can also create new variables and use them in your maps:
# Mutate creates a new colunm
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX", "ARG"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")


install.packages("rnaturalearth")
install.packages("remotes")
remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(rnaturalearthhires)
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)


dir.create("data/shapefiles", recursive = T) # Allows you to create the dir data and the dir inside it shapefiles at the same time

st_write(obj = bra,
         dsn = "data/shapefiles/bra.shp",
         delete_layer = T) # the delete_layer allows you to overwrite the shapefile
                              # usually the shapefiles are protected and cannot be easily deleted


# Loading, ploting, and saving a raster from the disk ---------------------------

library(raster)
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)


