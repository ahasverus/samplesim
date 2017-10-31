\name{get_output}

\alias{get_output}

\title{Import samplesim simulation results}

\description{
This function imports samplesim simulation results. More specifically it loads the median and width of credible intervals.
}

\usage{
get_output(name = "simulation_name", change = FALSE, reference = NULL)
}

\arguments{
\item{name}{Name of the simulation to extract results.}
\item{change}{If TRUE, values are expressed as a percentage of change compared to a reference.}
\item{reference}{The reference state to compute percentage of change. Default is the minimum value of sample size.}
}

\value{
A data frame with five columns:
\item{source}{The source name.}
\item{size}{The sample size.}
\item{replicate}{The number of replicate.}
\item{value}{The value of medians/widths of credible intervals.}
\item{type}{The label of value (i.e. medians or widths)}
If 'change' is TRUE, then the column replicate is omitted and results are aggregated over replicates.
}

\references{
Lecomte N., Ehrich D., Casajus N., Berteaux D., Giroux M.-A., Yoccoz N.G. How many is enough? An R package for evaluating the effect of sample size on estimates and precision of stable isotope mixing solutions. Submitted in Methods in Ecology and Evolution.
}

\author{
Nicolas Casajus, Nicolas Lecomte, Dorothee Ehrich
}

\seealso{
\code{\link{samplesim}}, \code{\link{plot_samplesim}}
}

\examples{
# See the vignette for a complete example.
}
