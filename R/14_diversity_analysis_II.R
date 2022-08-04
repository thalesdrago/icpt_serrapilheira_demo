####### Writting began in August 2022 by Thales
####### Based in the script given by Andrea Sanchez-Tapia
####### Written during the course Introduction to Scientific Computation
####### Module: Taxonomical and functional diversity
####### Instructor: Andrea Sanchez-Tapia
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script provides ways of estimating diversity (functional, taxonomic, phylogeny) metrics

# Library
library(vegan)
library(cluster)
library(FD)
library(SYNCSA)
library(taxize)
library(dplyr)
library(ape) # read newick
library(phytools)
library(devtools)


### On the diversity indeces ----------------------------------------------------------------
# Problems with the mathematical behaviour of the diversity index (search fot the work of Pavoine)

# Common diversity indecis are special cases of Reyni diversity with different scales
# The scale goes from zero to infinity, in smaller scales each individual species has a similar weight
# Higher scale levels gives more importance to the most abundant species

Community.A <- c(10, 6, 4, 1)
Community.B <- c(17, rep(1,7))

diversity(Community.A, "shannon")
diversity(Community.B, "shannon")
diversity(Community.A, "invsimpson")
diversity(Community.B, "invsimpson")

ren_A <- renyi(Community.A)
ren_B <- renyi(Community.B)

ren_AB <- rbind(ren_A, ren_B)

matplot(t(ren_AB), type = "l", axes = F)
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11)
legend("topright", legend = c("Community A", "Community B"),
       lty = c(1, 2),
       col = c(1, 2))
# Since the profiles of diversity of these two communities intersect it is not possible to say each is more diverse



### Importing cestes dataset -----------------------------------------------------------------
# Listing all files that end with csv and returning the full names
cestes_files <- list.files(path = "data/raw/cestes",
                           pattern = "csv$",
                           full.names = TRUE)

# Creating a vector that has the names of the files without the .csv in the end
cestes_names <- gsub(".csv", "", basename(cestes_files), fixed = TRUE)

comm <- read.csv(cestes_files[1])

head(comm[,1:6])

traits <- read.csv(cestes_files[5])
head(traits[,1:6])

# Changing the rows names
rownames(comm)[1:6]
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

head(traits)[,1:6]
rownames(traits) <- paste0(traits[,1])
traits <- traits[,-1]
head(traits[,1:6])



### Calculating species richness and taxonomic diversity ---------------------------------------
richness <- vegan::specnumber(comm)

shannon <- vegan::diversity(comm, index = "shannon")
simpson <- vegan::diversity(comm, index = "simpson")

# Calculating functional diversity
gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)

# Implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?

class(gow)
class(gow2)#different classes

plot(gow, gow2, asp = 1) #same values
plot(hclust(gow2)) # Creating a dendrogram



### Rao's quadratic entropy calculations ------------------------------------------------
# Estimating Rao's
tax <- SYNCSA::rao.diversity(comm)
fun <- SYNCSA::rao.diversity(comm, traits = traits)

# Comparing Rao's and Simpson
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)

# We can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)

# The returned object has Villéger's indices and Rao calculation
names(FuncDiv1)

# We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)
head(FuncDiv)


### Calculating phylogenetic diversity -----------------------------------------------------
spp <- read.csv(cestes_files[4])

spp$TaxonName

# Getting the taxonomic ranks for each species from the NCBI database
classification_data <- classification(spp$TaxonName, db = "ncbi")

head(classification_data)
length(classification_data)

# For some species like this the function could not find anything
classification_data$`Asphodelus aetivus`

# Filtering the list to get a vector of families
tibble_exp <- classification_data[[1]] %>% # starting with one list
                filter(rank == "family") %>%
                select(name) # this returns a tibble
tibble_exp

character_exp <- classification_data[[1]] %>%
                  filter(rank == "family") %>%
                  pull(name)

character_exp


vector_families <- NULL
for (i in 1:length(classification_data)) {
  if (!is.na(classification_data[i])) {
    vector_families[i] <- classification_data[[i]] %>%
      filter(rank == "family") %>%
      pull(name)
  }
}
vector_families

# Magallon et al 2015 A metacalibrated time-tree documents the early rise of flowering plant phylogenetic diversity
#phytools::read.newick()
APG <- ape::read.nexus("./data/raw/Magallon_et_al_2015/Magallon_etal_PL_ML.nex")
plot(APG)
sort(APG$tip.label)
sort(spp$TaxonName)
