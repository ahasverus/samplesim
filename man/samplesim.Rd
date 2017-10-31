\name{samplesim}

\alias{samplesim}

\title{Effect of sample size in stable isotope mixing models}

\description{
This function allows investigating the effect of sample size on estimates and precision of stable isotope mixing solutions. User can modify the sample size for one chosen source, for all sources or for the consumer. See details section for further informations.}

\usage{
samplesim(package = "siar", mix, source, discr, type = NULL,
nsamples = NULL, modify = NULL, nrep = 100, interval = 90,
name = NULL, resid_err = TRUE, process_err = FALSE,
run = "test", alpha.prior = 1)
}

\arguments{
\item{package}{The package name to be used to estimate mixing proportions. Must be one of 'siar' or 'mixsiar'.}
\item{mix}{The output returned by the \code{\link[MixSIAR]{load_mix_data}} function and containing consumer isotope values. See the vignette for a complete example and the help page of the \code{\link[MixSIAR]{load_mix_data}} function.}
\item{source}{The output returned by the \code{\link[MixSIAR]{load_source_data}} function and containing mean and standard deviation isotope values of sources (and in some case raw values). See the vignette for a complete example and the help page of the \code{\link[MixSIAR]{load_source_data}} function.}
\item{discr}{The output returned by the \code{\link[MixSIAR]{load_discr_data}} function and containing discrimination values. See the vignette for a complete example and the help page of the \code{\link[MixSIAR]{load_discr_data}} function.}
\item{type}{Character indicating the type of analysis to run. Must be one of 'one source', 'all sources' or 'consumer'.}
\item{nsamples}{A vector with the sample sizes to simulate.}
\item{modify}{The source name to modify (case sensitive). This argument has to be specified when \code{type} is 'one source'. Otherwise it will be ignored.}
\item{nrep}{An integer specifying the number of replicates for each sample sizes. Default is 100.}
\item{interval}{An integer indicating the width of credible interval to use for precision estimation. Default is 90.}
\item{name}{A character string giving the name of the simulation. If NULL the simulation will be named by the time of the simulation. This name will serve to create a directory in which results will be stored.}
\item{resid_err}{Boolean indicating if residual error is included in the model. See \code{\link[MixSIAR]{run_model}} for further informations. Only necessary if package = mixsiar.}
\item{process_err}{Boolean indicating if process error is included in the model. See \code{\link[MixSIAR]{run_model}} for further informations. Only necessary if package = mixsiar.}
\item{run}{String or list specifying MCMC parameters. See \code{\link[MixSIAR]{run_model}} for further informations. Only necessary if package = mixsiar.}
\item{alpha.prior}{Numeric. Dirichlet prior on p.global. See \code{\link[MixSIAR]{run_model}} for further informations. Only necessary if package = mixsiar.}
}

\details{
This function assesses the sensitivity of isotopes mixing models to variation in numbers of samples from source tissues. This tool can be used prior to full-blown studies in a similar manner than power analyses. It used the function \code{\link[siar]{siarmcmcdirichletv4}} developped by Andrew Parnell et al. and available in the package \code{\link[siar]{siar}}. Alternatively, it can be used with the function \code{\link[MixSIAR]{run_model}} developped by Brian Stock et al. and available in the package \code{\link[MixSIAR]{MixSIAR}}. User can choose to sample one particular source (argument \code{type} sets to 'one source') or all the sources in the same type (argument \code{type} sets to 'all sources'). User can also choose to modify consumer data (argument \code{type} sets to 'consumer'). Sample sizes are modified assuming a normal distribution with a user defined mean and standard deviation. Samples of different sizes are created from this distribution, and mixing proportions are estimated for several replicates of each sample size with the function \code{\link[siar]{siarmcmcdirichletv4}} or \code{\link[MixSIAR]{run_model}}.
}

\value{
This function does not return any object in the R console. Results are stored in a directory (argument 'name') and contain four R objects.
\item{intervals}{A four dimensions array with the upper and lower bounds of the credible interval for each sample size, replicate and source. First dimension represents lower and upper bounds; second dimension corresponds to the number of sources; third dimension is the number of replicates; and fourth dimension is the number of sample size.}
\item{widths}{A three dimensions array with the width (precision) of credible intervals for each source, each replicate and each sample size. First dimension corresponds to the number of replicates; second dimension is the number of sources; and third dimension represents the number of sample size.}
\item{medians}{A three dimensions array with the median (estimate) of credible intervals for each source, each replicate and each sample size. Dimensions are the same as for widths object.}
\item{datasets}{A four dimensions array with all resampled datasets.}
A logfile is also written and contains all parameters of the simulation.
}

\references{
Lecomte N., Ehrich D., Casajus N., Berteaux D., Giroux M.-A., Yoccoz N.G. How many is enough? An R package for evaluating the effect of sample size on estimates and precision of stable isotope mixing solutions. Submitted in Methods in Ecology and Evolution.
}

\note{
The use of samplesim with the package MixSIAR requires the installation of the software JAGS. See the \code{\link[MixSIAR]{MixSIAR}} documentation for further details.
}

\author{
Nicolas Casajush, Nicolas Lecomte, Dorothee Ehric
}

\seealso{
\code{\link{plot_samplesim}}, \code{\link[siar]{siarmcmcdirichletv4}}, \code{\link[MixSIAR]{run_model}}
}

\examples{
# See the vignette for a complete example.
}
