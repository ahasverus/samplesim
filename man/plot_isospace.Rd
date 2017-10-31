\name{plot_isospace}

\alias{plot_isospace}

\title{Plot data in the isotope space}

\description{
This function represents the data in the isotope space to overlay consumer and sources isotope values.
}

\usage{
plot_isospace(mix, source, discr, filename = "isospace", plot_save_pdf = FALSE, plot_save_png = FALSE)
}

\arguments{
\item{mix}{The output returned by the \code{\link[MixSIAR]{load_mix_data}} function and containing consumer isotope values. See the vignette for a complete example and the help page of the \code{\link[MixSIAR]{load_mix_data}} function.}
\item{source}{The output returned by the \code{\link[MixSIAR]{load_source_data}} function and containing mean and standard deviation isotope values of sources (and in some case raw values). See the vignette for a complete example and the help page of the \code{\link[MixSIAR]{load_source_data}} function.}
\item{discr}{The output returned by the \code{\link[MixSIAR]{load_discr_data}} function and containing discrimination values. See the vignette for a complete example and the help page of the \code{\link[MixSIAR]{load_discr_data}} function.}
\item{filename}{The name of the plot file(s) to save.}
\item{plot_save_pdf}{Boolean. If TRUE, the plot(s) are aved as pdf.}
\item{plot_save_png}{Boolean. If TRUE, the plot(s) are aved as png.}
}

\references{
Lecomte N., Ehrich D., Casajus N., Berteaux D., Giroux M.-A., Yoccoz N.G. How many is enough? An R package for evaluating the effect of sample size on estimates and precision of stable isotope mixing solutions. Submitted in Methods in Ecology and Evolution.
}

\note{
User can also use the \code{\link[MixSIAR]{plot_data}} of the package MixSIAR.
}

\author{
Nicolas Casajus, Nicolas Lecomte, Dorothee Ehrich
}

\seealso{
\code{\link{samplesim}}, \code{\link{get_output}}
}

\examples{
# See the vignette for a complete example.
}
