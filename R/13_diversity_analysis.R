####### Writing began in July 2022 by Thales
####### Based in the script given by Andrea Sanchez-Tapia
####### Written during the course Introduction to Scientific Computation
####### Module: Diversity metrics and analysis
####### Instructor: Andrea Sanchez-Tapia
####### Serrapilheira ICTP/SAIRF QBIO program

####### This script provides an introduction on how to calculate basic diversity metrics from a community matrix

# Importing data
comm <- read.csv("data/raw/cestes/comm.csv")
head(comm[,1:6])

# Which species are the five most abundant considering all sites -----------------------------
comm <- as.data.frame(comm)
ab_spp <- sort(apply(comm[,-1], 2, sum), decreasing = T)
ab_spp[1:5]


# Which is the richness of each site ---------------------------------------
comm_b <- comm
comm_b[comm_b >1] = 1 # making it binary
comm_b

sum2 <- apply(comm_b[,-1], 1, sum)
richness <- data.frame(comm$Sites, sum2)
names(richness[1]) <- "sites"
head(richness)


# Which is the dominant species in each site --------------------------------
dom_sp <-apply(comm[, -1],1, which.max)
site_sp <- data.frame(comm$Sites, dom_sp)
head(site_sp)


# Estimating Shannon diversity index for each site
x <- comm[1,-1]
my_diversity <- function(x, index){
  x <- as.numeric(x)
  sum_x <- sum(x)
  pi <- NULL
  for (i in 1:length(x)) {
    pi[i] <- x[i]/sum_x
  }
  if (index == "shannon") {
    log_pi <- -pi*log(pi)
    log_pi[is.na(log_pi)] <- 0
    result <- sum(log_pi)
  }
  if (index == "simpson") {
    D <- 1 - sum(pi^2)
    result <- D
  }
  return(result)
}

my_diversity(x, index = "simpson")

# Cheking using the vegan::diversity package
diversity(x, "shannon")
diversity(x, "simpson")
