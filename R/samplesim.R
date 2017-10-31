samplesim <- function(
  package = "siar",
  mix,
  source,
  discr,
  type = NULL,
  nsamples = NULL,
  modify   = NULL,
  nrep = 100,
  interval = 90,
  name = NULL,
  resid_err = TRUE,
  process_err = FALSE,
  run = "test",
  alpha.prior = 1
) {



  package <- tolower(package)
  type    <- tolower(type)


  # CHECKS

  if (!(package %in% c("siar", "mixsiar"))) {

    stop("Incorrect package name: must be 'siar' or 'mixsiar'")
  }

  if (!(type %in% c("one source", "all sources", "consumer"))) {

    stop("You have to select one type of analysis among 'one source', 'all sources' and 'consumer'.")
  }

  if (is.null(modify) && type == "one source") {

    stop("You have to specify which source will be sampled.")
  }

  if (type %in% c("all sources", "consumer")) {

    modwhich <- NULL

  } else {

    modwhich <- which(source$source_names == modify)

    if (length(modwhich) == 0) {

        stop(paste0("Wrong source name. Available sources are: ", paste0(source$source_names, collapse = " / ")))
    }
  }

  if (is.null(nsamples)) {

    stop("You have to specify the sample size(s).")
  }

  if (is.null(name)) {

    name <- substr(gsub(" |-|:", "", as.character(Sys.time())), 3, 12)
  }

  if (length(which(dir() == name)) == 1) {

    cat("The simulation name already exists. Do you want to overwrite it (type 'y' or 'n')?")

    ans <- readLines(n = 1)

    if (tolower(ans) == "n") {

      stop("Please provides a new simulation name.")
    }
  }

  if (length(source$S_factor1) > 0 || length(mix$fac_random) > 0 || length(mix$factors) > 0) {

    stop("Analysis by factors (fixed or random) is not currently supported by samplesim.")
  }



  # OUTPUTS DIRECTORY CREATION

  dir.create(name, showWarnings = FALSE)
  setwd(name)



  # PARAMETERS

  nbsizes      <- length(nsamples)
  iso          <- mix$iso_names
  nbiso        <- length(iso)
  nbsources    <- length(source$source_names)
  source_names <- as.factor(as.character(source$source_names))



  # CONVERT DATA FOR SIAR

  if (package == "siar") {

    if (source$data_type == "raw") {

      colnames(source$S_MU)  <- paste0("Mean", colnames(source$S_MU))
      colnames(source$S_SIG) <- paste0("SD", colnames(source$S_SIG))
    }

    sources <- data.frame(Source = source_names)
    for (i in 1:ncol(source$S_MU)) {

      sources <- cbind(sources, source$S_MU[ , i])
      colnames(sources)[ncol(sources)] <- colnames(source$S_MU)[i]

      sources <- cbind(sources, source$S_SIG[ , i])
      colnames(sources)[ncol(sources)] <- colnames(source$S_SIG)[i]
    }
    source <- sources
    rownames(source) <- NULL

    mix <- as.data.frame(mix$data_iso)

    corrects <- data.frame(Source = source_names)
    for (i in 1:ncol(discr$mu)) {

      corrects <- cbind(corrects, discr$mu[ , i])
      colnames(corrects)[ncol(corrects)] <- colnames(discr$mu)[i]

      corrects <- cbind(corrects, discr$sig2[ , i])
      colnames(corrects)[ncol(corrects)] <- colnames(discr$sig2)[i]
    }
    rownames(corrects) <- NULL
  }


  # OBJECTS CREATION

  medians    <- array(
    dim      = c(nrep, nbsources, nbsizes),
    dimnames = list(
      replicate = paste0("replicate", 1:nrep),
      source    = source_names,
      size      = paste0("size", nsamples)
    )
  )

  widths     <- medians

  if (package == "mixsiar") {
    intervals  <- array(
      dim      = c(2, nbsources, nrep, nbsizes),
      dimnames = list(
        interval  = c("lower", "upper"),
        source    = source_names,
        replicate = paste0("replicate", 1:nrep),
        size      = paste0("size", nsamples)
      )
    )

  } else {

    intervals  <- array(
  		dim = c(2, nbiso + nbsources, nrep, nbsizes),
  		dimnames = list(
  			interval  = c("lower", "upper"),
  			source    = c(as.character(source[ , 1]), paste0("sd", 1:nbiso)),
  			replicate = paste0("replicate", 1:nrep),
  			size      = paste0("size", nsamples)
  		)
  	)
  }

  if (type == "consumer") {

    datasets <- array(
      dim      = c(max(nsamples), nbiso, nrep, nbsizes),
      dimnames = list(
        individual = NULL,
        isotope    = colnames(mix),
        replicate  = paste0("replicate", 1:nrep),
        size       = paste0("size", nsamples)
      )
    )

  } else {

    datasets <- array(
      dim      = c(nbsources, nbiso * 2, nrep, nbsizes),
      dimnames = list(
        source    = source_names,
        isotope   = colnames(source)[-1],
        replicate = paste0("replicate", 1:nrep),
        size      = paste0("size", nsamples)
      )
    )
  }



  # RUNNING SAMPLESIM

  for (m in 1:nbsizes) {

    res <- list()

    for (n in 1:nrep) {

      cat(paste("\n\n>>>>> Calculating replicate dataset ", n, " for a sample size of ", nsamples[m], ".\n\n", sep = ""))

      sources.s <- source


      ###
      ### SAMPLE DATASET
      ###



      ### PACKAGE
      if (package == "siar") {

        ## ANALYSIS TYPE
        if (type == "one source") {

          sources.s[modwhich, ] <- meansd.nd(
            data  = source[modwhich, ],
            n     = nsamples[m],
            input = "means"
          )
        }

        ## ANALYSIS TYPE
        if (type == "all sources") {

          for (k in 1:nbsources) {

            sources.s[k, ] <- meansd.nd(
              data  = source[k, ],
              n     = nsamples[m],
              input = "means"
            )
          }
        }

        ## ANALYSIS TYPE
        if (type == "consumer") {

          tar.mn <- apply(mix, 2, mean)
          tar.sd <- apply(mix, 2, sd)
          mix.s <- NULL

          for (i in 1:length(tar.mn)) {

            mix.s <- cbind(mix.s, rnorm(nsamples[m], tar.mn[i], tar.sd[i]))
          }
          colnames(mix.s) <- colnames(mix)
        }
      }

      ### PACKAGE
      if (package == "mixsiar") {

        ## ANALYSIS TYPE
        if (type == "one source") {

          # SOURCE FORMAT
          if (sources.s$data_type == "means") {

            MU_SIG <- data.frame(
              source = source_names,
                       source$S_MU,
                       source$S_SIG
            )

            ssample <- meansd.nd(
              data  = MU_SIG[modwhich, ],
              n     = nsamples[m],
              input = "means"
            )

            for (i in 1:length(iso)) {

              sources.s$S_MU[modwhich, paste0("Mean", iso[i])]     <- ssample[, paste0("Mean", iso[i])]
              sources.s$S_SIG[modwhich, paste0("SD", iso[i])]      <- ssample[, paste0("SD", iso[i])]
              sources.s$MU_array[modwhich, paste0("Mean", iso[i])] <- sources.s$S_MU[modwhich, paste0("Mean", iso[i])]
              sources.s$SIG2_array[modwhich, paste0("SD", iso[i])] <- sources.s$S_SIG[modwhich, paste0("SD", iso[i])] ^ 2
              # sources.s$n_array[modwhich] <- nsamples[m]
            }
          }

          # SOURCE FORMAT
          if (sources.s$data_type == "raw") {

            mu  <- source$S_MU
            colnames(mu) <- paste0("Mean", colnames(mu))
            sig <- source$S_SIG
            colnames(sig) <- paste0("SD", colnames(sig))

            MU_SIG <- data.frame(
              source = source_names,
                       mu,
                       sig
            )

            ssample <- meansd.nd(
              data  = MU_SIG[modwhich, ],
              n     = nsamples[m],
              input = "raw"
            )

            for (i in 1:length(iso)) {

              sources.s$S_MU[modwhich, iso[i]]  <- mean(ssample[ , iso[i]])
              sources.s$S_SIG[modwhich, iso[i]] <- sd(ssample[ , iso[i]])
              sources.s$SOURCE_array[modwhich, i, ] <- NA
              sources.s$SOURCE_array[modwhich, i, 1:nrow(ssample)] <- ssample[ , iso[i]]
            }
            sources.s$n.rep[modwhich] <- nrow(ssample)
          }
        }

        ## ANALYSIS TYPE
        if (type == "all sources") {

          # SOURCE FORMAT
          if (sources.s$data_type == "means") {

            MU_SIG <- data.frame(
              source = source_names,
                       source$S_MU,
                       source$S_SIG
            )

            for (k in 1:nbsources) {

              ssample <- meansd.nd(
                data  = MU_SIG[k, ],
                n     = nsamples[m],
                input = "means"
              )

              for (i in 1:length(iso)) {

                sources.s$S_MU[k, paste0("Mean", iso[i])]     <- ssample[, paste0("Mean", iso[i])]
                sources.s$S_SIG[k, paste0("SD", iso[i])]      <- ssample[, paste0("SD", iso[i])]
                sources.s$MU_array[k, paste0("Mean", iso[i])] <- sources.s$S_MU[k, paste0("Mean", iso[i])]
                sources.s$SIG2_array[k, paste0("SD", iso[i])] <- sources.s$S_SIG[k, paste0("SD", iso[i])] ^ 2
                sources.s$n_array[k] <- nsamples[m]
              }
            }
          }

          # SOURCE FORMAT
          if (sources.s$data_type == "raw") {

            mu  <- source$S_MU
            colnames(mu) <- paste0("Mean", colnames(mu))
            sig <- source$S_SIG
            colnames(sig) <- paste0("SD", colnames(sig))

            MU_SIG <- data.frame(
              source = source_names,
                       mu,
                       sig
            )

            for (k in 1:nbsources) {

              ssample <- meansd.nd(
                data  = MU_SIG[k, ],
                n     = nsamples[m],
                input = "raw"
              )

              for (i in 1:length(iso)) {

                sources.s$S_MU[k, iso[i]]  <- mean(ssample[ , iso[i]])
                sources.s$S_SIG[k, iso[i]] <- sd(ssample[ , iso[i]])
                sources.s$SOURCE_array[k, i, ] <- NA
                sources.s$SOURCE_array[k, i, 1:nrow(ssample)] <- ssample[ , iso[i]]
              }
              sources.s$n.rep[k] <- nrow(ssample)
            }
            sources.s$SOURCE_array <- sources.s$SOURCE_array[ , , 1:nsamples[m]]
          }
        }

        ## ANALYSIS TYPE
        if (type == "consumer") {

          tar.mn <- apply(mix$data_iso, 2, mean)
          tar.sd <- apply(mix$data_iso, 2, sd)
          data.s <- NULL

          for (i in 1:length(iso)) {

            data.s <- cbind(data.s, rnorm(nsamples[m], tar.mn[i], tar.sd[i]))
          }
          colnames(data.s) <- colnames(mix$data_iso)

          mix.s          <- mix
          mix.s$data     <- data.s
          mix.s$data_iso <- data.s
          mix.s$N        <- nrow(data.s)
        }
      }


      ###
      ### RUN MODELS
      ###


      if (package == "siar") {

        if (type %in% c("one source", "all sources")) {

          datasets[ , , n, m] <- as.matrix(sources.s[ , -1])

          if (sum(discr$mu) == 0){

              res[[n]] <- siar::siarmcmcdirichletv4(
                as.matrix(mix),
                sources.s
              )[[15]]

          } else {

            res[[n]] <- siar::siarmcmcdirichletv4(
              as.matrix(mix),
              sources.s,
              corrects
            )[[15]]
          }

        } else {

          datasets[1:nsamples[m], , n, m] <- mix.s

          if (sum(discr$mu) == 0){

            res[[n]] <- siar::siarmcmcdirichletv4(
              as.matrix(mix.s),
              source
            )[[15]]

          } else {

            res[[n]] <- siar::siarmcmcdirichletv4(
              as.matrix(mix.s),
              source,
              corrects
            )[[15]]
          }
        }
      }

      if (package == "mixsiar") {

        if (type %in% c("one source", "all sources")) {

          datasets[ , , n, m] <- as.matrix(cbind(sources.s$S_MU, sources.s$S_SIG))

          write_JAGS_model(
            filename    = "MixSIAR_model.txt",
            resid_err   = resid_err,
            process_err = process_err,
            mix         = mix,
            source      = sources.s
          )

          jags <- run_model(
            run            = run,
            mix            = mix,
            source         = sources.s,
            discr          = discr,
            model_filename = "MixSIAR_model.txt",
            alpha.prior    = alpha.prior,
            resid_err      = resid_err,
            process_err    = process_err
          )

          res[[n]] <- jags$BUGSoutput[[8]][[2]]
          colnames(res[[n]]) <- source_names


        } else {

          datasets[1:nsamples[m], , n, m] <- mix.s$data

          write_JAGS_model(
            filename    = "MixSIAR_model.txt",
            resid_err   = resid_err,
            process_err = process_err,
            mix         = mix.s,
            source      = sources
          )

          jags <- run_model(
            run            = run,
            mix            = mix.s,
            source         = source,
            discr          = discr,
            model_filename = "MixSIAR_model.txt",
            alpha.prior    = alpha.prior,
            resid_err      = resid_err,
            process_err    = process_err
          )

          res[[n]] <- jags$BUGSoutput[[8]][[2]]
          colnames(res[[n]]) <- source_names
          file.remove("MixSIAR_model.txt")
        }
      }
    }  # Close loop on nsamples

    credintervals <- lapply(res, credint, interval = interval)

    for (n in 1:nrep) {

      intervals[ , , n, m] <- credintervals[[n]]
      temp                 <- intervals[2, , n, m] - intervals[1, , n, m]
      widths[n, , m]       <- temp[1:nbsources]
      medians[n, , m]      <- apply(res[[n]][ , 1:nbsources], 2, median)
    }
    rm(list = c("credintervals", "res"))
  }



  # SAVE RESULTS (.rds)

  saveRDS(intervals, file = "intervals.rds")
  saveRDS(widths,    file = "widths.rds")
  saveRDS(medians,   file = "medians.rds")
  saveRDS(datasets,  file = "datasets.rds")



  # WRITE LOG FILE

  cat("====================================================\n", file = "logfile.txt", sep = "")
  cat("        SampleSim - User-defined parameters\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("====================================================\n\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("Simulation name: ", name, "\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("Date: ", as.character(Sys.time()), "\n\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("Type of sampling: ", type, "\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("Package: ", package, "\n", file = "logfile.txt", sep = "", append = TRUE)

  if (!is.null(modwhich)) {
    cat("Modified source: ", modwhich, "\n", file = "logfile.txt", sep = "", append = TRUE)
    cat("Modified source name: ", as.character(source_names[modwhich]), "\n", file = "logfile.txt", sep = "", append = TRUE)
  }

  cat("Number of replicates by sample size: ", nrep, "\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("Sample sizes: ", paste(nsamples, collapse = ", "), "\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("Credible intervals width: ", interval, "\n\n", file = "logfile.txt", sep = "", append = TRUE)
  cat("====================================================\n\n", file = "logfile.txt", sep = "", append = TRUE)



  # RESET WORKING DIRECTORY

  setwd("..")
  cat("Results and datasets have been stored in: ", paste0(getwd(), "/", name), ".\n", sep = "")
}
