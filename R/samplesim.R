samplesim <-
function(target, sources, type = NULL, nsamples = NULL, modwhich = NULL,
correct = NULL, nrep = 100, interval = 90, name = NULL){

	if (!(type %in% c("one source", "all sources", "consumer")))
		stop("You have to select one type of analysis among 'onesource', 'allsources' and 'consumer'.")

	if (is.null(modwhich) && type == "one source")
		stop("You have to specify which source will be sampled.")

	if (type %in% c("all sources", "consumer"))
		modwhich <- NULL

	if (is.null(nsamples))
		stop("You have to specify the sample size(s).")

	if (is.null(name)){
		name <- as.character(Sys.time())
		name <- gsub(' ', '', name) ; name <- gsub('-','', name) ; name <- gsub(':','', name)
		name <- substr(name, 3, 12)
	}

	if (length(which(dir() == name)) == 1){
		cat("The simulation name already exists.\nDo you want to overwrite it (type 'y' or 'n')?")
		ans <- readLines(n = 1)
		if (ans == "n")
			stop("Change the simulation name.")
	}

	dir.create(name, showWarnings = F)
	setwd(name)

	nbsizes <- length(nsamples)
	nbiso <- (ncol(sources)-1)/2
	iso <- colnames(target)
	nbsources <- nrow(sources)

	medians <- array(dim = c(nrep, nbsources, nbsizes), dimnames = list(paste("rep", 1:nrep, sep = "."), as.character(sources[ , 1]), paste("nsamples", nsamples, sep = ".")))

	widths <- medians

	intervals <- array(dim = c(nbiso, nbiso + nbsources, nrep, nbsizes))
	dimnames(intervals) <-  list(NULL, c(as.character(sources[ , 1]), paste("SD", 1:nbiso, sep = "")), paste("rep", 1:nrep, sep = "."), paste("nsamples", nsamples, sep = "."))

	if (type == "consumer"){

		datasets <- array(dim = c(max(nsamples), nbiso, nrep, nbsizes))
		dimnames(datasets) <-  list(NULL, colnames(target), paste("rep", 1:nrep, sep = "."), paste("nsamples", nsamples, sep = "."))
		
	}else{

		datasets <- array(dim = c(nbsources, nbiso*2, nrep, nbsizes))
		dimnames(datasets) <-  list(as.character(sources[ , 1]), colnames(sources)[-1], paste("rep", 1:nrep, sep = "."), paste("nsamples", nsamples, sep = "."))
	}

	for (m in 1 : nbsizes){

		res <- list()

		for (n in 1 : nrep){

			cat(paste("\n\n>>>>> Calculating replicate dataset ", n," for a sample size of ",nsamples[m],".\n\n",sep = ""))

			sources.s <- sources

			if (type == "one source")

				sources.s[modwhich, ] <- meansd.nd(sources[modwhich, ], nsamples[m])

			if (type == "all sources"){

				for (i in 1 : nbsources)
					sources.s[i, ] <- meansd.nd(sources[i, ], nsamples[m])
			}

			if (type == "consumer"){

				tar.mn <- apply(target, 2, mean)
				tar.sd <- apply(target, 2, sd)
				target.s <- NULL

				for (i in 1 : length(tar.mn))
					target.s <- cbind(target.s, rnorm(nsamples[m], tar.mn[i], tar.sd[i]))
				colnames(target.s) <- colnames(target)
			}

			if (type == "one source" || type == "all sources"){

				datasets[ , , n, m] <- as.matrix(sources.s[, -1])
				res[[n]] <- siarmcmcdirichletv4(as.matrix(target), sources.s, correct)[[15]]

			}else{

				datasets[1:nsamples[m] , , n, m] <- target.s
				res[[n]] <- siarmcmcdirichletv4(as.matrix(target.s), sources, correct)[[15]]
			}
		}

		credintervals <- lapply(res, credint, interval = interval)
		for (n in 1 : nrep){

			intervals[ , , n, m] <- credintervals[[n]]

			temp <- intervals[2, , n, m] - intervals[1, , n, m]
			widths[n, , m] <- temp[1 : nbsources]

			medians[n, , m] <- apply(res[[n]][, 1 : nbsources], 2, median)

		}
		rm(list = c("credintervals", "res"))
	}

	# Save results
	save(intervals, file = "intervals.Rdata")
	save(widths, file = "widths.Rdata")
	save(medians, file = "medians.Rdata")
	save(datasets, file = "datasets.Rdata")


	# Write log file
	cat("-------------------------------------------------------------------------------\n", file = "logfile.txt", sep = "")
	cat("   SampleSim: results of mixing model simulations modifying the sample sizes\n", file = "logfile.txt", sep = "", append = T)
	cat("-------------------------------------------------------------------------------\n\n", file = "logfile.txt", sep = "", append = T)
	cat("(Mixing proportions were estimated with the R-package 'siar' by Andrew Parnell)", "\n\n", file = "logfile.txt", append = T, sep = " ")
	cat("Simulation name: ", name, "\n", file = "logfile.txt", sep = "", append = T)
	cat("Date: ", as.character(Sys.time()), "\n\n", file = "logfile.txt", sep = "", append = T)
	cat("Type of sampling: ", type, "\n", file = "logfile.txt", sep = "", append = T)
	if (!is.null(modwhich)){
		cat("Modified source: ", modwhich, "\n", file = "logfile.txt", append = T, sep = "")
		cat("Modified source name: ", as.character(sources[modwhich, 1]), "\n", file = "logfile.txt", append = T, sep = "")
	}
	cat("Number of replicates by sample size: ", nrep, "\n", file = "logfile.txt", sep = "", append = T)
	cat("Sample sizes: ", paste(nsamples, collapse = ", "), "\n", file = "logfile.txt", sep = "", append = T)
	cat("Credible intervals width: ", interval, "\n", file = "logfile.txt", sep = "", append = T)

	setwd("..")
	cat("Results and datasets have been stored in: ", paste(getwd(), "/", name, sep = ""), ".\n", sep = "")
}
