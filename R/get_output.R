#' Import samplesim results
#'
#' @description 
#' This function imports samplesim simulation results. More specifically it 
#' loads the median and width of credible intervals.
#'
#' @param change a logical value. If `TRUE` values are expressed as a 
#'   percentage of change compared to a reference. Default is `FALSE`.
#'   
#' @param reference an integer. The reference state to compute percentage of 
#'   change. If `NULL` (default) the minimum value of sample size will be used.
#' 
#' @inheritParams samplesim
#'
#' @return A data frame with five columns:
#' - `source`: the source name.
#' - `size`: the sample size.
#' - `replicate`: the number of replicate.
#' - `value`: the value of medians/widths of credible intervals.
#' - `type`: the label of value (i.e. medians or widths)
#' 
#' If `change = TRUE`, then the column replicate is omitted and results are 
#' aggregated over replicates.
#' 
#' @seealso [samplesim()], [plot_samplesim()], [plot_isospace()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Please visit the vignette available at: 
#' ## https://ahasverus.github.io/samplesim/articles/samplesim.html
#' }

get_output <- function(name, path = ".", change = FALSE, reference = NULL) {

  ## Checks ----

  if (!dir.exists(path)) {
    stop("The directory ", path, " does not exist.")
  }
  
  if (!dir.exists(file.path(path, name))) {
    stop("The directory ", file.path(path, name), " does not exist.")
  }


  ## Import data ----

  medians <- readRDS(file.path(path, name, "medians.rds"))
  widths  <- readRDS(file.path(path, name, "widths.rds"))


  ## Convert into % of change ----

  if (change) {

		if (is.null(reference)) {
      reference <- as.numeric(gsub("size", "", dimnames(medians)$"size"[1]))
    }

		tab1 <- as.data.frame(matrix(ncol = length(dimnames(medians)[2]$"source"),
		                             nrow = length(dimnames(medians)[3]$"size")))
		
		rownames(tab1) <- dimnames(medians)[3]$"size"
		colnames(tab1) <- dimnames(medians)[2]$"source"

		tab2 <- tab1

		for (i in 1:dim(medians)[2]) {

			for (j in 1:dim(medians)[3]) {

				tab1[j, i] <- mean(medians[ , i, j])
				tab2[j, i] <- mean( widths[ , i, j])
			}
		}

    pos <- which(rownames(tab1) == paste0("size", reference))
    
    if (length(pos) == 0) {
      stop("Unable to retrieve the sample size ", reference)
    }

    for (i in (1:nrow(tab1))[-pos]){

      tab1[i, ] <- round(100 * (tab1[i, ] - tab1[pos, ]) / tab1[pos, ], 3)
      tab2[i, ] <- round(100 * (tab2[i, ] - tab2[pos, ]) / tab2[pos, ], 3)
    }

    tab1[pos, ] <- round(100 * (tab1[pos, ] - tab1[pos, ]) / tab1[pos, ], 3)
    tab2[pos, ] <- round(100 * (tab2[pos, ] - tab2[pos, ]) / tab2[pos, ], 3)


		dat1 <- dat2 <- data.frame()

		for (i in 1:ncol(tab1)) {

			dat1 <- rbind(dat1, data.frame("size"   = rownames(tab1),
			                               "source" = rep(colnames(tab1)[i], 
			                                              nrow(tab1)),
			                               "value"  = tab1[ , i]))

			dat2 <- rbind(dat2, data.frame("size"   = rownames(tab2),
			                               "source" = rep(colnames(tab2)[i], 
			                                              nrow(tab2)),
			                               "value"  = tab2[ , i]))
		}

    dat1$"type" <- rep("Median of posterior distribution", nrow(dat1))
    dat2$"type" <- rep("Width of credible intervals", nrow(dat2))

    tab <- rbind(dat1, dat2)

  } else {

    tab1 <- data.frame()

		for (i in 1:dim(medians)[3]) {

			dat <- medians[ , , i]
			tmp <- data.frame()

			for (j in 1:ncol(dat)) {

				tmp <- rbind(tmp, data.frame("source"    = colnames(dat)[j],
				                             "size"      = rep(dimnames(
				                               medians)[3]$"size"[i], nrow(dat)),
				                             "replicate" = rownames(dat),
				                             "value"     = dat[ , j],
				                             row.names   = NULL))
			}
			
			tab1 <- rbind(tab1, tmp)
		}
    
    tab1$"type" <- rep("Median of posterior distribution", nrow(tab1))


		tab2 <- data.frame()

		for (i in 1:dim(widths)[3]) {

			dat <- widths[ , , i]
			tmp <- data.frame()

			for (j in 1:ncol(dat)) {
			  
				tmp <- rbind(tmp, data.frame("source"    = colnames(dat)[j],
				                             "size"      = rep(dimnames(
				                               widths)[3]$"size"[i], nrow(dat)),
				                             "replicate" = rownames(dat),
				                             "value"     = dat[ , j],
				                             row.names = NULL))
			}
			
			tab2 <- rbind(tab2, tmp)
		}
		
    tab2$"type" <- rep("Width of credible intervals", nrow(tab2))

    tab <- rbind(tab1, tab2)


    tab$"replicate" <- as.character(gsub("replicate", "", tab$"replicate"))
    tab$"replicate" <- factor(tab$"replicate", 
                              levels = sort(as.numeric(unique(
                                tab$"replicate"))))

  }


  tab$"size"   <- as.character(gsub("size", "", tab$"size"))
  tab$"size"   <- factor(tab$"size", 
                         levels = sort(as.numeric(unique(tab$"size"))))
  tab$"source" <- factor(tab$"source", 
                         levels = sort(as.character(unique(tab$"source"))))
  tab$"type"   <- factor(tab$"type", 
                         levels = c("Width of credible intervals", 
                                    "Median of posterior distribution"))

  tab
}
