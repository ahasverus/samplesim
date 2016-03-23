\name{formatsources}

\alias{formatsources}

\title{Format source data for stable isotope analysis}

\description{
This function computes mean and standard deviation of isotope concentrations from sources raw measurements.
}

\usage{
formatsources(data, labels)
}

\arguments{
\item{data}{A data frame containing the isotope values (column) for each individual prey (row). The first column must contain the source name. See \code{\link{rawPrey}} for an example.}
\item{labels}{A list containing the different source names where the name of each element of the list has to be specified. Each element of the list can contain more than one source name in case of grouping. See the example below.}
}

\details{
This function works with two or more than two isotopes.}

\value{
A data frame with each source as a separate row and (2 x number of isotopes) columns. An additional column (first) contains the source names. For each isotope mean and standard deviation are returned.
}

\author{
Nicolas Casajus, Nicolas Lecomte, Dorothee Ehrich
}

\seealso{
\code{\link{rawPrey}}, \code{\link{samplesim}}
}

\examples{
data(rawPrey)

# Group similar preys
src <- list(c("Ristri", "Urilom"), "Rantar", "Micoec", "Lemlem", c("Laglag", "Lagmut"), 
"Melnig")

# Name of sources
names(src) <- c("Marine", "Reindeer", "Voles", "Lemming", "Ptarmigan", "Birds")
src

# Format data
(meansdiso <- formatsources(rawPrey, labels = src))
}
