\name{plot_samplesim}

\alias{plot_samplesim}

\title{Plot samplesim simulation results}

\description{
This plot function is a graphical representation of the effects of sample size on estimates and precision of stable isotope mixing solutions. Two plots are currently available: a plot for the width of the credible interval and one for the median of the posterior distribution, both displayed for each sample size and each source.
}

\usage{
plot_samplesim(name = "simulation_1", change = FALSE, reference = NULL)
}

\arguments{
\item{name}{Name of the simulation to plot results.}
\item{change}{If TRUE, values are expressed as a percentage of change.}
\item{reference}{The reference state to compute percentage of change.}
}

\details{
This plot function automatically loads results data stored by the \code{\link{samplesim}} function from the simulation name. It produces two plots. The first plot presents the width of the credible interval displayed for each sample size and each source. The second represents the median of the posterior distribution for each sample size and each source.
}

\author{
Nicolas Casajus, Nicolas Lecomte, Dorothee Ehrich
}

\seealso{
\code{\link{samplesim}}, \code{\link{get_output}},
}

\examples{
# Load datasets
data(dataPrey)
data(dataConsumer)

# Effect of sample size by modifying source 4
samplesim(
  dataConsumer,
  dataPrey,
  type = "one source",
  nsamples = c(5, 10, 25),
  nrep = 10,
  modwhich = 4,
  name = "sim_1")

# Visualize results
plot_samplesim(name = "sim_1")

# Visualize results (percentage of change)
plot_samplesim(name = "sim_1", change = TRUE, reference = 5)
}
