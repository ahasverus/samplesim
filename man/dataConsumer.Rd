\name{dataConsumer}

\alias{dataConsumer}

\docType{data}

\title{Consumer plasma data}

\description{
A 2 column and 60 row data frame which contains the isotopic plasma values of 60 consumers over 2 isotopes. Data come from Killengreen et al. (2011).
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

\references{
Killengreen S., Lecomte N., Ehrich D., Schott T., Yoccoz N.G. and Ims R.A. (2011) The importance of marine vs. human-induced subsidies in the maintenance of an expanding mesocarnivore in the arctic tundra. Journal of Animal Ecology, 80, 1049-1060.
}

\examples{
data(dataConsumer)
head(dataConsumer)
}
