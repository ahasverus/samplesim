get_output <- function(
	name      = "simulation_name",
	change    = FALSE,
	reference = NULL
){



  # CHECKS

	if (length(which(dir() == name)) == 0) {

    stop("Select the appropriate folder or simulation name.")
  }



  # IMPORT DATA

  setwd(name)
  medians <- readRDS("medians.rds")
  widths  <- readRDS("widths.rds")



  # CONVERT IN % OF CHANGE

  if (change) {

		if (is.null(reference)) {

      reference <- as.numeric(gsub("size", "", dimnames(medians)$size[1]))
    }

		tab1 <- as.data.frame(
			matrix(
				ncol = length(dimnames(medians)[2]$source),
				nrow = length(dimnames(medians)[3]$size)
			)
		)
		rownames(tab1) <- dimnames(medians)[3]$size
		colnames(tab1) <- dimnames(medians)[2]$source


		tab2 <- tab1

		for (i in 1:dim(medians)[2]) {

			for (j in 1:dim(medians)[3]) {

				tab1[j, i] <- mean(medians[ , i, j])
				tab2[j, i] <- mean( widths[ , i, j])
			}
		}

    pos <- which(rownames(tab1) == paste0("size", reference))

    for (i in (1:nrow(tab1))[-pos]){

      tab1[i, ] <- round(100 * (tab1[i, ] - tab1[pos, ]) / tab1[pos, ], 3)
      tab2[i, ] <- round(100 * (tab2[i, ] - tab2[pos, ]) / tab2[pos, ], 3)
    }

    tab1[pos, ] <- round(100 * (tab1[pos, ] - tab1[pos, ]) / tab1[pos, ], 3)
    tab2[pos, ] <- round(100 * (tab2[pos, ] - tab2[pos, ]) / tab2[pos, ], 3)


		dat1 <- dat2 <- data.frame()

		for (i in 1:ncol(tab1)) {

			dat1 <- rbind(
				dat1,
				data.frame(
					size   = rownames(tab1),
					source = rep(colnames(tab1)[i], nrow(tab1)),
					value  = tab1[ , i]
				)
			)

			dat2 <- rbind(
				dat2,
				data.frame(
					size   = rownames(tab2),
					source = rep(colnames(tab2)[i], nrow(tab2)),
					value  = tab2[ , i]
				)
			)
		}

    dat1$type <- rep("Median of posterior distribution", nrow(dat1))
    dat2$type <- rep("Width of credible intervals", nrow(dat2))

    tab <- rbind(dat1, dat2)

  } else {

		# CONVERT < WIDTHS > TO DATA FRAME

    tab1 <- data.frame()

		for (i in 1:dim(medians)[3]) {

			dat <- medians[ , , i]
			tmp <- data.frame()

			for (j in 1:ncol(dat)) {

				tmp <- rbind(
					tmp,
					data.frame(
						source    = colnames(dat)[j],
						size      = rep(dimnames(medians)[3]$size[i], nrow(dat)),
						replicate = rownames(dat),
						value     = dat[ , j],
						row.names = NULL
					)
				)
			}
			tab1 <- rbind(tab1, tmp)
		}
    tab1$type <- rep("Median of posterior distribution", nrow(tab1))



    # CONVERT < MEDIANS > TO DATA FRAME

		tab2 <- data.frame()

		for (i in 1:dim(widths)[3]) {

			dat <- widths[ , , i]
			tmp <- data.frame()

			for (j in 1:ncol(dat)) {
				tmp <- rbind(
					tmp,
					data.frame(
						source    = colnames(dat)[j],
						size      = rep(dimnames(widths)[3]$size[i], nrow(dat)),
						replicate = rownames(dat),
						value     = dat[ , j],
						row.names = NULL
					)
				)
			}
			tab2 <- rbind(tab2, tmp)
		}
    tab2$type <- rep("Width of credible intervals", nrow(tab2))



    # COLLAPSE THE TWO DATA FRAME

    tab <- rbind(tab1, tab2)



    # CLEAN AND SORT FACTOR < REPLICATES >

    tab$replicate <- as.character(gsub("replicate", "", tab$replicate))
    tab$replicate <- factor(tab$replicate, levels = sort(as.numeric(unique(tab$replicate))))

  }


  # CLEAN FACTOR

  tab$size   <- as.character(gsub("size", "", tab$size))
  tab$size   <- factor(tab$size, levels = sort(as.numeric(unique(tab$size))))
  tab$source <- factor(tab$source, levels = sort(as.character(unique(tab$source))))
  tab$type   <- factor(tab$type, levels = c("Width of credible intervals", "Median of posterior distribution"))

  setwd("..")

  return(tab)
}
