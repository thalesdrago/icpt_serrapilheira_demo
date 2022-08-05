####### Writing began in July 2022 by Thales
####### Based in the script given by Andrea Sanchez-Tapia
####### Written during the course Introduction to Scientific Computation
####### Module: Multivariate analysis
####### Instructor: Andrea Sanchez-Tapia
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script

# Library
library(vegan)
library(cluster)



### Hierarquical clustering techiniques -----------------------------------------------------------
# We are going to explore more hierarquical clustering
data(dune.env)
data(dune)

head(dune.env)
head(dune)

table(dune.env$Management)


### Cluster analysis of the dune  vegetation
# We can calculate to difference distance indeces - Bray-Curtis and Chord distance
# Bray-Curtis distance
bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1
chord_distance <- dist(decostand(dune, "norm"))


### Using distance matrices to generate dendograms (hierarquical clustering)
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")

plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)


### Ordination techiniques ----------------------------------------------------------------
# We are going to explore more hierarquical clustering
# Normalizing the data to do the PCA
norm <- decostand(dune, "norm")

# PCA species
pca <- vegan::rda(norm) # RDA is similar to PCA, but you can incorporate a second matrix (e.g. environmental matrix)

plot(pca)

par(mfrow = c(1, 1))

# PCA environmental factors
dune.env[, c("Moisture")] <- as.numeric(dune.env[, c("Moisture")])
dune.env[, c("Manure")] <- as.numeric(dune.env[, c("Manure")])
pca_env <- vegan::rda(dune.env[, c("A1", "Moisture", "Manure")])

plot(pca_env)

par(mfrow = c(1, 2))


