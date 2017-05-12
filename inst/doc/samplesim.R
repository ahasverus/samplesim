## ---- eval = FALSE, echo = TRUE------------------------------------------
#  # Install the < devtools > package
#  install.packages("devtools", dependencies = TRUE)
#  
#  # Load the < devtools > package
#  library(devtools)
#  
#  # Install the < samplesim > package from GitHub
#  devtools::install_github("ahasverus/samplesim")
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
names(src) <- species

# Print list
src

## ---- eval = TRUE, echo = FALSE------------------------------------------
head(dataConsumer)

## ---- eval = TRUE, echo = FALSE------------------------------------------
head(dataPrey)

## ---- eval = FALSE, echo = TRUE------------------------------------------
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
#    nsamples = c(2:10, 15, 25, 50, 75, 100, 250),
#    nrep = 500,
#    interval = 90,
#    name = "simulation_1")

## ---- eval = TRUE, echo = TRUE-------------------------------------------
# Content of the new directory (results)
dir("simulation_1")

## ---- echo = FALSE, eval = TRUE------------------------------------------
# Import medians of credible intervals
log <- readLines("simulation_1/logfile.txt")
cat(paste0(log, collapse = "\n"))

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Import medians of credible intervals
medians <- readRDS("simulation_1/medians.rds")

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Structure of the object
class(medians)

# Names of the dimensions
names(dimnames(medians))

# Names of the content of the second dimension
dimnames(medians)[[2]]

# Names of the content of the third dimension
dimnames(medians)[[3]]

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Extract results of the first replicate
medians[1, , ]

# Compute mean over replicates
apply(medians, 3:2, mean)

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Import medians and widths of credible intervals
datas <- get_output("simulation_1")

# Structure of the data frame
str(datas)

# Print the first ten and last ten rows
rbind(head(datas, 10), tail(datas, 10))

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Import medians of credible intervals
widths <- datas[datas$type == "Width of credible intervals", ]

# Check
rbind(head(widths, 10), tail(widths, 10))

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Import medians and widths of credible intervals expressed as percentage of change
datas <- get_output("simulation_1", change = TRUE, reference = 2)

# Structure of the data frame
str(datas)

# Print the first ten and last ten rows
rbind(head(datas, 10), tail(datas, 10))

## ---- echo = TRUE, eval = TRUE, fig.width = 7.1, fig.height = 4.5--------
# Visualize results
plot_samplesim(name = "simulation_1")

## ---- echo = TRUE, eval = TRUE, fig.width = 7.1, fig.height = 4.5--------
# Visualize results expressed as percentages of change
plot_samplesim(name = "simulation_1", change = TRUE, reference = 2)

## ---- echo = TRUE, eval = TRUE, fig.width = 7.1, fig.height = 4.5--------
# Import medians and widths of credible intervals
datas <- get_output("simulation_1")

# Reminder #1
head(datas)

# Reminder #2
tail(datas)

# Graph
ggplot(aes(x = size, y = value), data = datas) +
  geom_boxplot(aes(fill = source), width = 0.8, outlier.shape = NA) +
  labs(x = "Sample size", y = "Values", fill = "Sources") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  facet_grid(. ~ type)

## ---- echo = TRUE, eval = TRUE, fig.width = 7.1, fig.height = 4.5--------
# Import medians and widths of credible intervals
datas <- get_output("simulation_1", change = TRUE)

# Select medians of credible intervals
medians <- datas[datas$type == "Median of posterior distribution", ]

# Print data
head(medians)

# Graph
ggplot(aes(x = size, y = value, group = source), data = medians) +
  geom_point(aes(color = source)) +
  geom_line(aes(color = source)) +
  labs(x = "Sample size", y = "Change in medians (%)", color = "Sources") +
  theme_light() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

