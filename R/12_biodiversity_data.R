####### Writing began in July 2022 by Thales
####### Based in the script given by Andrea Sanchez-Tapia
####### Written during the course Introduction to Scientific Computation
####### Module: Biodiversity databases
####### Instructor: Andrea Sanchez-Tapia
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script provides an introduction on how to import and clean biodiversity data from GBIF

# Library
library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
library(dplyr)


# Download data from GBIF for Myrsine coriacea (Primulaceae)
species <- "Myrsine coriacea"
occs <- occ_search(scientificName = species, # Specify that you are calling the species name
                   limit = 100000,           # Specify the limit of occurences
                   basisOfRecord = "PRESERVED_SPECIMEN")
names(occs)

# This returns a object of the class gbif, which is basically a list of 5 other objects
# We just want now the occurence data
myrsine.data <- occs$data
colnames(myrsine.data)


# Saving the data
dir.create("data/raw/", recursive = TRUE)
write.csv(myrsine.data,
          "data/raw/myrsine_data.csv",
          row.names = FALSE)

# Checking the names
sort(unique(myrsine.data$scientificName))

table(myrsine.data$taxonomicStatus) # how many accepted names and synonyms

table(myrsine.data$scientificName, myrsine.data$taxonomicStatus) # how many accepted names and synonyms

# Using the function TPL() from package taxonstand to check if the taxonomic updates in the gbif data are correct
species.names <- unique(myrsine.data$scientificName)  # First we must creat a vector with all the species names
dim(species.names)

tax.check <- TPL(species.names)
names(tax.check)

# creating new object w/ original and new names after TPL
new.tax <- data.frame(scientificName = species.names,
                      genus.new.TPL = tax.check$New.Genus,
                      species.new.TPL = tax.check$New.Species,
                      status.TPL = tax.check$Taxonomic.status,
                      scientificName.new.TPL = paste(tax.check$New.Genus,
                                                     tax.check$New.Species))

# now we are merging raw data and checked data
myrsine.new.tax <- merge(myrsine.data, new.tax, by = "scientificName")

# Exporting data
dir.create("data/processed/", recursive = TRUE)
write.csv(myrsine.new.tax,
          "data/processed/data_taxonomy_check.csv",
          row.names = FALSE)

# Ploting the distribution
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1) # asp rescale the view so it does not look squished
map(, , , add = TRUE) # the three commas is because you are using the default of the first arguments


# Checking and excluding NAs
myrsine.coord <- myrsine.data[!is.na(myrsine.data$decimalLatitude)
                              & !is.na(myrsine.data$decimalLongitude),]

# Cleaning the dataset using coordinate cleaner
# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = myrsine.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               value = "clean")

par(mfrow = c(1, 2))
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)
plot(decimalLatitude ~ decimalLongitude, data = geo.clean, asp = 1)
map(, , , add = TRUE)




