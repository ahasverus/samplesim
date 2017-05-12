get_output <-
function(name = "simulation_1", change = FALSE, reference = NULL){



  # CHECKS
	if (length(which(dir() == name)) == 0){
    stop("Select the appropriate folder or simulation name.")
  }



  # IMPORT DATA
  setwd(name)
  medians <- readRDS("medians.rds")
  widths  <- readRDS("widths.rds")



  # CONVERT IN % OF CHANGE

  if (change){

    if (is.null(reference)){
      reference <- as.numeric(gsub("size", "", dimnames(medians)$size[1]))
    }

    medians <- apply(medians, 3:2, mean)
    widths <- apply( widths, 3:2, mean)

    pos <- which(rownames(medians) == paste0("size", reference))

    for (i in (1:nrow(medians))[-pos]){
      medians[i, ] <- round(100 * (medians[i, ] - medians[pos, ]) / medians[pos, ], 1)
      widths[i, ] <- round(100 * (widths[i, ] - widths[pos, ]) / widths[pos, ], 1)
    }

    medians[pos, ] <- round(100 * (medians[pos, ] - medians[pos, ]) / medians[pos, ], 1)
    widths[pos, ] <- round(100 * (widths[pos, ] - widths[pos, ]) / widths[pos, ], 1)


    medians <- adply(medians, 1:2)
    medians$type <- rep("Width of credible intervals", nrow(medians))
    widths <- adply(widths, 1:2)
    widths$type <- rep("Median of posterior distribution", nrow(widths))

    colnames(medians)[3] <- colnames(widths)[3] <- 'value'

    tab <- rbind(medians, widths)

  } else {

    # CONVERT < WIDTHS > TO DATA FRAME

    tab1 <- adply(widths, 3:1)
    colnames(tab1) <- c("size", "source", "replicate", "value")
    tab1$type <- rep("Width of credible intervals", nrow(tab1))



    # CONVERT < MEDIANS > TO DATA FRAME

    tab2 <- adply(medians, 3:1)
    colnames(tab2) <- c("size", "source", "replicate", "value")
    tab2$type <- rep("Median of posterior distribution", nrow(tab2))



    # COLLAPSE THE TWO DATA FRAME

    tab <- rbind(tab1, tab2)



    # CLEAN AND SORT FACTOR < REPLICATES >

    tab$replicate <- as.character(gsub("replicate", "", tab$replicate))
    tab$replicate <- factor(tab$replicate, level = sort(as.numeric(unique(tab$replicate))))

  }


  # CLEAN FACTOR

  tab$size <- as.character(gsub("size", "", tab$size))
  tab$size <- factor(tab$size, level = sort(as.numeric(unique(tab$size))))
  tab$source <- factor(tab$source, level = sort(as.character(unique(tab$source))))
  tab$type   <- factor(tab$type, level = c("Width of credible intervals", "Median of posterior distribution"))

  setwd("..")

  tab
}
