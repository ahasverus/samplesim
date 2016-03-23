\name{dataPrey}

\alias{dataPrey}

\docType{data}

\title{Preys plasma data}

\description{
A 5 column and 6 row data frame which contains the isotopic plasma values of 6 preys (sources) over mean and standard deviation values of 2 isotopes.
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

\examples{
data(dataPrey)
print(dataPrey)
}
