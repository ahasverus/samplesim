\name{dataConsumer}

\alias{dataConsumer}

\docType{data}

\title{Consumer plasma data}

\description{
A 2 column and 60 row data frame which contains the isotopic plasma values of 60 consumers over 2 isotopes.
}

\usage{
data(dataConsumer)
}

\format{
A data frame with 60 observations on the following 2 variables:
\describe{
\item{\code{d13C}}{Plasma values of the d13C isotope}
\item{\code{d15N}}{Plasma values of the d15N isotope}
}
}

\details{
This dataset is designed for a direct use by the function \code{\link{samplesim}}.
}

\examples{
data(dataConsumer)
head(dataConsumer)
}
