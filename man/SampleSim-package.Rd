\name{samplesim-package}

\alias{samplesim-package}

\docType{package}

\title{
Sample size effects in stable isotope mixing solutions
}

\description{
This package allows the investigation of the effect of sample size on estimates and precision of stable isotope mixing solutions calculated with the package \code{\link[siar]{siar}} (written by Andrew Parnell et al.). Sample sizes are modified assuming a normal distribution with a user defined mean and standard deviation. Samples of different sizes are created from this distribution, and mixing proportions are estimated for several replicates of each sample size using the function siarmcmcdirchletv4 (package siar) and default values for the MCMC.
}

\details{
\tabular{ll}{
Package: \tab samplesim\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-05-12\cr
License: \tab GPL-2\cr
}
}

\author{
Nicolas Casajus, Nicolas Lecomte, Dorothee Ehrich

Maintainer: Nicolas Casajus <nicolas.casajus@gmail.com>
}
\references{
Lecomte N., Ehrich D., Casajus N., Berteaux D., Giroux M.-A., Yoccoz N.G. How many is enough? An R package for evaluating the effect of sample size on estimates and precision of stable isotope mixing solutions. Submitted in Methods in Ecology and Evolution.
}

\seealso{
\code{\link[siar:siar-package]{siar}}
}
