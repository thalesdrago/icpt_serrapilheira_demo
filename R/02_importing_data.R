# Scientific programming course ICTP-Serrapilheira July-2022
# Class #03: Scientific workflows
# Reading raw data, writing processed data, functions of apply family, for loop, writing outputs: tables and figs

# Reading data -----------------------------------------------------------------

# Listing all files that end with csv and returning the full names

cestes_files <- list.files(path = "data/raw/cestes",
                           pattern = "csv$",
                           full.names = TRUE)

# Creating a vector that has the names of the files without the .csv in the end
cestes_names <- gsub(".csv", "", basename(cestes_files), fixed = TRUE)

#Reading environmental file
envir <- read.csv(cestes_files[3])

#Applying to each element of the vector of files the function read.csv and creating a list
data_list <- lapply(cestes_files, read.csv)
names(data_list) <- cestes_names #Naming each element of the list with the names of the vector of names

length(data_list)

# Inspecting data
head(data_list$envir)
dim(data_list$envir)
summary(data_list$envir)

# Output 1: summary table ------------------------------------------------------

# Creating summary table for all environmental variables
sd(envir$Clay)
envir_mean <- apply(envir[, -1], 2, mean) # applying the function mean to each collunm, but the first, of the envir dataframe
envir_sd <- apply(envir[, -1], 2, sd)

# Creating a function in R -----------------------------------------------------
# This function calculates the standard error of a vector
std <- function(x, round = FALSE, ...) {
  std <- sd(x) / sqrt(length(x))
  if (round) std <- round(std, ...) #the ... let the user determine in the std function the arguments of the round function
  return(std)
}

std(envir$Clay, round = TRUE, digits = 2)

#Applying the std function to all the collunms of envir dataframe, but the first
envir_std <- apply(envir[, -1], 2, std, round = TRUE, digits = 2)

envir_tbl <- data.frame(variable = names(envir_mean),
                        mean = round(envir_mean, 2),
                        std = envir_std, row.names = NULL)

# Writing summary table
if (!dir.exists("output/")) dir.create("output/", recursive = TRUE)
write.csv(envir_tbl,
          "output/02_envir_summary.csv", row.names = FALSE)

# Output 2: figure -------------------------------------------------------------

# Species vs sites table
head(data_list$comm)

comm <- data_list$comm

head(comm)

# Sum of species abundances across sites
comm_sum <- apply(comm[, -1], 2, sum)
colSums(comm[, -1])

# Plotting a species abundance curve
plot(sort(comm_sum, decreasing = TRUE), las = 1, bty = "l",
     xlab = "Species", ylab = "Abundance")

# Exporting the figure
res <- 300
fig_size <- (res * 240) / 72

if (!dir.exists("figs")) dir.create("figs")
png("figs/02_species_abundance.png",
    res = res,
    height = fig_size,
    width = 1.5 * fig_size)
plot(sort(comm_sum, decreasing = TRUE), las = 1, bty = "l",
     xlab = "Species", ylab = "Abundance")
text(substitute(paste(italic("Bolboschoenus maritimus"))), x = 14, y = 126,
     cex = 0.8)
text(substitute(paste(italic("Phalaris coerulescens"))), x = 14, y = 80,
     cex = 0.8)
dev.off()

# for loop in R ----------------------------------------------------------------
comm_df <- as.data.frame(comm_sum)
comm_df$TaxonName <- NA
for (sp in rownames(comm_df)) {
  comm_df[sp, "TaxonName"] <- data_list$splist$TaxonName[data_list$splist$TaxCode == sp]
}
