# samplesim

An R package for estimating sample size effects in stable isotope mixing solutions

To install this package on R (or RStudio), you have to:

```r
# Install the < devtools > package
install.packages('devtools', dependencies = TRUE)

# Load the < devtools > package
library(devtools)

# Install the < samplesim > package from GitHub
devtools::install_github('ahasverus/samplesim')

# Load the < samplesim > package
library(samplesim)
```

For the beginners, you can:

```r
# List the content (objects and functions) of the < samplesim > package
ls('package:samplesim')

# Open the < samplesim > package home page
help(samplesim)

# Open the help file of a specific function
help(plot_samplesim)

# Open the vignette
browseVignettes(package = 'samplesim')
```

### Reference

Lecomte N., Ehrich D., Casajus N., Berteaux D., Giroux M.-A. and Yoccoz N.G. <span style='color: #338c8c;'>How many is enough? An R package for evaluating the effect of sample size on estimates and precision of stable isotope mixing solutions</span>. <em>Methods in Ecology and Evolution</em>, submitted.
