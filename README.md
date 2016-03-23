# SampleSim

An R package for estimating sample size effects in stable isotope mixing solutions

To install this package on R (or RStudio), you have to:

```{r}
### Install the devtools package
install.packages('devtools')

### Load the devtools package
library(devtools)

### Install the samplesim package from GitHub
devtools::install_github('ahasverus/SampleSim')

### Load the samplesim package
library(SampleSim)
```

For the beginners, you can:

```{r}
### See functions available in the package
ls('package:SampleSim')

### See the package home page
help(SampleSim)

### See the help file of a specific function
help(plotsamplesim)

### See the vignette
vignette('SampleSim')
```
