\name{dataPrey}

\alias{dataPrey}

\docType{data}

\title{Preys plasma data}

\description{
A 5 column and 6 row data frame which contains the isotopic plasma values of 6 preys (sources) over mean and standard deviation values of 2 isotopes. Data come from Killengreen et al. (2011).
}

\usage{
data(dataPrey)
}

\format{
A data frame with 6 observations on the following 5 variables:
\describe{
\item{\code{Sources}}{Name of sources}
\item{\code{Meand13C}}{Mean values of the d13C isotope}
\item{\code{SDd13C}}{Standard deviations of the d13C isotope}
\item{\code{Meand15N}}{Mean values of the d15N isotope}
\item{\code{SDd15N}}{Standard deviations of the d15N isotope}
}
}

\details{
This dataset is designed for a direct use by the function \code{\link{samplesim}}.
}

\references{
Killengreen S., Lecomte N., Ehrich D., Schott T., Yoccoz N.G. and Ims R.A. (2011) The importance of marine vs. human-induced subsidies in the maintenance of an expanding mesocarnivore in the arctic tundra. Journal of Animal Ecology, 80, 1049-1060.
}

\examples{
data(dataPrey)
print(dataPrey)
}
