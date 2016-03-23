\name{rawPrey}

\alias{rawPrey}

\docType{data}

\title{Raw prey plasma data}

\description{
A 3 column and 97 row data frame which contains the isotopic plasma values of 97 prey (sources) over 2 isotopes.
}

\usage{
data(rawPrey)
}

\format{
A data frame with 97 observations on the following 3 variables:
\describe{
\item{\code{Species}}{Species code}
\item{\code{d13C}}{Plasma values of the d13C isotope}
\item{\code{d15N}}{Plasma values of the d15N isotope}
}
}

\details{
This dataset is not designed for a direct use by the function \code{\link{samplesim}}. The function \code{\link{formatsources}} converts these raw data in the samplesim standard (see \code{\link{dataPrey}} and the example below).
}

\examples{
data(rawPrey)

# Group similar preys
src <- list(c("Ristri", "Urilom"), "Rantar", "Micoec", "Lemlem", c("Laglag", "Lagmut"),
"Melnig")

# Name of sources
names(src) <- c("Marine", "Reindeer", "Voles", "Lemming", "Ptarmigan", "Birds")

# Format data
(meansdiso <- formatsources(rawPrey, labels = src))
}
