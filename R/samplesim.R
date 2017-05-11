samplesim <-
function(target, sources, type = NULL, nsamples = NULL, modwhich = NULL,
correct = NULL, nrep = 100, interval = 90, name = NULL){



	# CHECKS

	if (!(type %in% c("one source", "all sources", "consumer"))){
		stop("You have to select one type of analysis among 'onesource', 'allsources' and 'consumer'.")
	}

	if (is.null(modwhich) && type == "one source"){
		stop("You have to specify which source will be sampled.")
	}

	if (type %in% c("all sources", "consumer")){
		modwhich <- NULL
	}

	if (is.null(nsamples)){
		stop("You have to specify the sample size(s).")
	}

	if (is.null(name)){
		name <- substr(gsub(" |-|:", "", as.character(Sys.time())), 3, 12)
	}

	if (length(which(dir() == name)) == 1){
		cat("The simulation name already exists.\nDo you want to overwrite it (type 'y' or 'n')?")
		ans <- readLines(n = 1)
		if (tolower(ans) == "n"){
			stop("Please provides a new simulation name.")
		}
	}



	# OUTPUTS DIRECTORY CREATION

	dir.create(name, showWarnings = FALSE)
	setwd(name)



	# PARAMETERS

	nbsizes   <- length(nsamples)
	nbiso     <- (ncol(sources) - 1) / 2
	iso       <- colnames(target)
	nbsources <- nrow(sources)



	# OBJECTS CREATION

	medians    <-
	widths     <- array(
		dim = c(nrep, nbsources, nbsizes),
		dimnames = list(
			replicate = paste0("replicate", 1:nrep),
			source    = as.character(sources[ , 1]),
			size      = paste0("size", nsamples)
		)
	)

	intervals  <- array(
		dim = c(nbiso, nbiso + nbsources, nrep, nbsizes),
		dimnames = list(
			interval  = c("lower", "upper"),
			source    = c(as.character(sources[ , 1]), paste0("sd", 1:nbiso)),
			replicate = paste0("replicate", 1:nrep),
			size      = paste0("size", nsamples)
		)
	)

	if (type == "consumer"){

		datasets <- array(
			dim = c(max(nsamples), nbiso, nrep, nbsizes),
			dimnames = list(
				individual = NULL,
				isotope    = colnames(target),
				replicate  = paste0("replicate", 1:nrep),
				size       = paste0("size", nsamples)
			)
		)

	} else {

		datasets <- array(
			dim = c(nbsources, nbiso*2, nrep, nbsizes),
			dimnames = list(
				source    = as.character(sources[ , 1]),
				isotope   = colnames(sources)[-1],
				replicate = paste0("replicate", 1:nrep),
				size      = paste0("size", nsamples)
			)
		)
	}



	# RUNNING SAMPLESIM

	for (m in 1:nbsizes){

		res <- list()

		for (n in 1:nrep){

			cat(paste("\n\n>>>>> Calculating replicate dataset ", n, " for a sample size of ", nsamples[m], ".\n\n", sep = ""))

			sources.s <- sources

			if (type == "one source"){
				sources.s[modwhich, ] <- meansd.nd(sources[modwhich, ], nsamples[m])
			}

			if (type == "all sources"){
				for (i in 1:nbsources){
					sources.s[i, ] <- meansd.nd(sources[i, ], nsamples[m])
				}
			}

			if (type == "consumer"){
				tar.mn <- apply(target, 2, mean)
				tar.sd <- apply(target, 2, sd)
				target.s <- NULL
				for (i in 1:length(tar.mn)){
					target.s <- cbind(target.s, rnorm(nsamples[m], tar.mn[i], tar.sd[i]))
				}
				colnames(target.s) <- colnames(target)
			}

			if (type %in% c("one source", "all sources")){
				datasets[ , , n, m] <- as.matrix(sources.s[, -1])
				res[[n]] <- siarmcmcdirichletv4(as.matrix(target), sources.s, correct)[[15]]
			} else {
				datasets[1:nsamples[m] , , n, m] <- target.s
				res[[n]] <- siarmcmcdirichletv4(as.matrix(target.s), sources, correct)[[15]]
			}
		}

		credintervals <- lapply(res, credint, interval = interval)
		for (n in 1:nrep){
			intervals[ , , n, m] <- credintervals[[n]]
			temp <- intervals[2, , n, m] - intervals[1, , n, m]
			widths[n, , m]  <- temp[1:nbsources]
			medians[n, , m] <- apply(res[[n]][, 1:nbsources], 2, median)
		}
		rm(list = c("credintervals", "res"))
	}



	# SAVE RESULTS (.rds)

	saveRDS(intervals, file = "intervals.rds")
	saveRDS(widths,    file = "widths.rds")
	saveRDS(medians,   file = "medians.rds")
	saveRDS(datasets,  file = "datasets.rds")



	# WRITE LOG FILE

	cat(
		"====================================================\n",
		file = "logfile.txt", sep = ""
	)
	cat(
		"        SampleSim - User-defined parameters\n",
		file = "logfile.txt", sep = "", append = TRUE
	)
	cat(
		"====================================================\n\n",
		file = "logfile.txt", sep = "", append = TRUE
	)
	cat(
		"Simulation name: ", name, "\n",
		file = "logfile.txt", sep = "", append = TRUE
	)
	cat(
		"Date: ", as.character(Sys.time()), "\n\n",
		file = "logfile.txt", sep = "", append = TRUE
	)
	cat(
		"Type of sampling: ", type, "\n",
		file = "logfile.txt", sep = "", append = TRUE
	)

	if (!is.null(modwhich)){
		cat(
			"Modified source: ", modwhich, "\n",
			file = "logfile.txt", sep = "", append = TRUE
		)
		cat(
			"Modified source name: ", as.character(sources[modwhich, 1]), "\n",
			file = "logfile.txt", sep = "", append = TRUE
		)
	}

	cat(
		"Number of replicates by sample size: ", nrep, "\n",
		file = "logfile.txt", sep = "", append = TRUE
	)
	cat(
		"Sample sizes: ", paste(nsamples, collapse = ", "), "\n",
		file = "logfile.txt", sep = "", append = TRUE
	)
	cat(
		"Credible intervals width: ", interval, "\n\n",
		file = "logfile.txt", sep = "", append = TRUE
	)
	cat(
		"====================================================\n\n",
		file = "logfile.txt", sep = "", append = TRUE
	)



	# RESET WORKING DIRECTORY

	setwd("..")
	cat("Results and datasets have been stored in: ", paste0(getwd(), "/", name), ".\n", sep = "")
}
