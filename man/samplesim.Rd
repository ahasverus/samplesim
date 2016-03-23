\name{samplesim}

\alias{samplesim}

\title{Effect of sample size in stable isotope mixing models}

\description{
This function allows investigating the effect of sample size on estimates and precision of stable isotope mixing solutions. User can modify the sample size for one chosen source, for all sources or for the consumer. See details section for further informations.}

\usage{
samplesim(target, sources, type = NULL, nsamples = NULL, modwhich = NULL,
correct = NULL, nrep = 100, interval = 90, name = NULL)}

\arguments{
\item{target}{A data frame with consumer isotope values. See example below or \code{\link{dataConsumer}} for the appropriate format.}
\item{sources}{A data frame with mean and standard deviation isotope values of sources. See example below or \code{\link{dataPrey}} for the appropriate format.}
\item{type}{Character indicating the type of analysis to run. Must be one of 'one source', 'all sources' or 'consumer'.}
\item{nsamples}{A vector with the sample sizes to simulate.}
\item{modwhich}{An integer indicating which source has to be modified. This argument has to be specified when \code{type} is 'one source'. Otherwise it will be ignored.}
\item{correct}{Optional. If specified, a data frame with discrimination values. See \code{\link[siar]{siar}} for further details.}
\item{nrep}{An integer specifying the number of replicates for each sample sizes. Default is 100.}
\item{interval}{An integer indicating the width of credible interval to use for precision estimation. Default is 90.}
\item{name}{A character string giving the name of the simulation. If NULL the simulation will be named by the time of the simulation. This name will serve to create a directory in which results will be stored.}
}

\details{
This function assesses the sensitivity of isotopes mixing models to variation in numbers of samples from source tissues. This tool can be used prior to full-blown studies in a similar manner than power analyses. It used the function \code{\link[siar]{siarmcmcdirichletv4}} developped by Andrew Parnell and available in the package \code{\link[siar]{siar}}. User can choose to sample one particular source (argument \code{type} sets to 'one source') or all the sources in the same type (argument \code{type} sets to 'all sources'). User can also choose to modify consumer data (argument \code{type} sets to 'consumer'). Sample sizes are modified assuming a normal distribution with a user defined mean and standard deviation. Samples of different sizes are created from this distribution, and mixing proportions are estimated for several replicates of each sample size with the function \code{\link[siar]{siarmcmcdirichletv4}}.
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
Lecomte N.,Ehrich D., Casajus N., Berteaux D., Giroux M.-A., Yoccoz N.G. How many is enough: sample size and the outcome of stable isotope mixing models. Submitted in Methods in Ecology and Evolution.
}

\author{
Nicolas Casajush, Nicolas Lecomte, Dorothee Ehric
}

\seealso{
\code{\link{formatsources}}, \code{\link{plotsamplesim}}, \code{\link[siar]{siarmcmcdirichletv4}}
}

\examples{
# Load datasets
data(dataPrey)
data(dataConsumer)

# Effect of sample size by modifying source 4
samplesim(dataConsumer, dataPrey, type = "one source", nsamples = c(5, 10, 25),
nrep = 10, modwhich = 4, name = "sim_1")

# Import estimates data
load("./sim_1/medians.Rdata")

# Calculate mean estimates by sources and by sample size
x <- NULL
for (i in 1 : dim(medians)[3])
x <-  rbind(x, apply(medians[ , , i], 2, mean))
}
