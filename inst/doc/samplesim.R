## ---- eval = FALSE, echo = TRUE------------------------------------------
#  # Install the < devtools > package
#  install.packages("devtools", dependencies = TRUE)
#  
#  # Load the < devtools > package
#  library(devtools)
#  
#  # Install the < samplesim > package from GitHub
#  devtools::install_github("ahasverus/samplesim", build_vignettes = TRUE)
#  
#  # Load the < samplesim > package
#  library(samplesim)

## ---- eval = TRUE, echo = FALSE------------------------------------------
library(samplesim)

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  # List the content (objects and functions) of the < samplesim > package
#  ls("package:samplesim")
#  
#  # Open the < samplesim > package home page
#  help(package = "samplesim")
#  
#  # Open the help file of a specific function
#  help(plot_samplesim)
#  
#  # Open the vignette (current document)
#  browseVignettes(package = "samplesim")

## ---- eval = TRUE, echo = TRUE-------------------------------------------
# Import isotopic plasma values of the consumers
data(dataConsumer)

# Print the first ten rows
head(dataConsumer, 10)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
# Import isotopic plasma values of the preys
data(dataPrey)

# Print the data frame content
dataPrey

## ---- eval = TRUE, echo = TRUE-------------------------------------------
# Import raw isotopic plasma values of the preys
data(rawPrey)

# Print the first ten rows
head(rawPrey, 10)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
# List species names
levels(rawPrey$Species)

# Group preys species into sources
(src <- list(
  Marine    = c("Ristri", "Urilom"),
  Reindeer  = c("Rantar"),
  Voles     = c("Micoec"),
  Lemming   = c("Lemlem"),
  Ptarmigan = c("Laglag", "Lagmut"),
  Birds     = c("Melnig")))

# Format preys data
(dataPrey <- format_sources(data = rawPrey, labels = src))

## ---- eval = TRUE, echo = TRUE-------------------------------------------
# Prey species names
(species <- c("Marine", "Birds", "Voles"))

# R List conversion
(src <- as.list(species))

# Name elements of the list
names(src) <- species

# Print list
src

## ---- eval = TRUE, echo = FALSE------------------------------------------
head(dataConsumer)

## ---- eval = TRUE, echo = FALSE------------------------------------------
head(dataPrey)

## ---- eval = FALSE-------------------------------------------------------
#  samplesim(
#    target,
#    sources,
#    type = NULL,
#    nsamples = NULL,
#    modwhich = NULL,
#    correct = NULL,
#    nrep = 100,
#    interval = 90,
#    name = NULL)

## ---- eval = FALSE, echo = TRUE, message = FALSE-------------------------
#  # samplesim run for one source
#  samplesim(
#    target = dataConsumer,
#    source = dataPrey,
#    type = "one source",
#    modwhich = 3,
#    nsamples = c(2, 5, 7, 10, 15, 25, 50, 75, 100, 250, 500, 750, 1000, 1500),
#    nrep = 500,
#    interval = 90,
#    name = "simulation_1")

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  # Content of the new directory (results)
#  dir("./simulation_1")

## ---- eval = TRUE, echo = FALSE------------------------------------------
dir("~/Desktop/simulation_1")

## ---- echo = FALSE, eval = TRUE------------------------------------------
# Import medians of credible intervals
log <- readLines("~/Desktop/simulation_1/logfile.txt")
cat(paste0(log, collapse = "\n"))

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  # Import medians of credible intervals
#  medians <- readRDS("./simulation_1/medians.rds")

## ---- echo = FALSE, eval = TRUE------------------------------------------
# Import medians of credible intervals
medians <- readRDS("~/Desktop/simulation_1/medians.rds")

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Structure of the objects
class(medians)

# Names of the dimensions
dimnames(medians)

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Extract results of the first replicate
medians[1, , ]

# Compute mean over replicates
apply(medians, 3:2, mean)

## ---- echo = -c(1, 2, 5), eval = TRUE, fig.width = 7.1, fig.height = 4.5----
indir <- getwd() ; setwd("~/Desktop")

# Visualize results with default settings
plot_samplesim(name = "simulation_1")
setwd(indir)

## ---- echo = -c(1, 2, 5), eval = TRUE, fig.width = 7.1, fig.height = 4.5----
indir <- getwd() ; setwd("~/Desktop")

# Visualize results with default settings
plot_samplesim(name = "simulation_1", change = TRUE)
setwd(indir)

