\name{plotsamplesim}

\alias{plotsamplesim}

\title{Plot SampleSim simulation results}

\description{
This plot function is a graphical representation of the effects of sample size on estimates and precision of stable isotope mixing solutions. Two plots are currently available: a plot for the width of the credible interval and one for the median of the posterior distribution, both displayed for each sample size and each source.
}

\usage{
plotsamplesim(name = "Simul", layout = "mpd", yaxis = "fixed", bg = T, 
col.bg = "lightgray", col.box = "white", family = "serif", 
cex.main = ifelse(layout == "spd", 1, 2), cex.axis = ifelse(layout == "spd", .75, 
1.25), cex.lgd = ifelse(layout == "spd", .75, 1.25), cex.lab = ifelse(layout == 
"spd", 1, 1.25), cex.text = ifelse(layout == "spd", 1, 2), ...)
}

\arguments{
\item{name}{Name of the simulation to plot results.}
\item{layout}{A two-choice argument. If 'spd', a plot by graphical device is drawned; if 'mpd', the two plots are drawned in the same panel.}
\item{yaxis}{A two-choice argument. If 'fixed', y-axis will bounded between 0 and 1; if 'adaptive' y-axis will adjusted to data.}
\item{bg}{Logical. If TRUE, a background is plotted.}
\item{col.bg}{Color of the background. If \code{bg} is FALSE, this argument is ignored.}
\item{col.box}{Color of background border. If \code{bg} is FALSE, this argument is ignored.}
\item{family}{Font family.}
\item{cex.main}{Size of title.}
\item{cex.axis}{Size of axis annotations.}
\item{cex.lgd}{Size of legend text.}
\item{cex.lab}{Size of axis labels.}
\item{cex.text}{Size of information text. Only used when \code{layout} is set to 'mpd'.}
\item{\dots}{Others graphical parameters.}
}

\details{
This plot function automatically loads results data stored by the \code{\link{samplesim}} function from the simulation name. It produces two plots. The first plot presents the width of the credible interval displayed for each sample size and each source. The second represents the median of the posterior distribution for each sample size and each source.
}

\author{
Nicolas Casajus, Nicolas Lecomte, Dorothee Ehrich
}

\seealso{
\code{\link{samplesim}}
}

\examples{
# Load datasets
data(dataPrey)
data(dataConsumer)

# Effect of sample size by modifying source 4
samplesim(dataConsumer, dataPrey, type = "one source", nsamples = c(5, 10, 25), 
nrep = 10, modwhich = 4, name = "sim_1")

# Visualize results with default settings
plotsamplesim(name = "sim_1")

# Visualize results without background and in separate windows
plotsamplesim(name = "sim_1", bg = FALSE, layout = "spd", cex.main = 1.25, 
cex.axis = 1, cex.lgd = 1, cex.lab = 1)
}