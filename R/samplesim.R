#' Effect of sample size in stable isotope mixing models
#' 
#' @description 
#' This function allows investigating the effect of sample size on estimates 
#' and precision of stable isotope mixing solutions. User can modify the sample
#' size for one chosen source, for all sources or for the consumer. 
#' See details section for further information.
#'
#'
#'
#' @param package a character of length 1. The package name to be used to 
#'   estimate mixing proportions. Must be one of `'siar'` (default) or 
#'   `'mixsiar'`.
#' 
#' @param mix a named list. The output returned by the 
#'   [MixSIAR::load_mix_data()] function and containing consumer isotope values.
#' 
#' @param source a named list. The output returned by the 
#'   [MixSIAR::load_source_data()] function and containing mean and standard 
#'   deviation isotope values of sources (and in some case raw values).
#' 
#' @param discr a named list. The output returned by the 
#'   [MixSIAR::load_discr_data()] function and containing discrimination values.
#' 
#' @param type a character of length 1 indicating the type of analysis to run. 
#'   Must be one of `'one source'`, `'all sources'` or `'consumer'`.
#' 
#' @param nsamples a numeric vector with the sample sizes to simulate.
#' 
#' @param modify a character of length 1. The source name to modify 
#'   (case sensitive). This argument has to be specified when 
#'   `type = 'one source'`. Otherwise it will be ignored.
#' 
#' @param nrep an integer of length 1 specifying the number of replicates for 
#'   each sample sizes. Default is `100`.
#' 
#' @param interval an integer of length 1 indicating the width of credible 
#'   interval to use for precision estimation. Default is `90`.
#' 
#' @param name a character of length 1 giving the name of the simulation. 
#'   If `NULL` (default) the simulation will be named by the time of the 
#'   simulation. This name will serve to create a directory in which results 
#'   will be stored.
#' 
#' @param resid_err a logical value indicating if residual error is included in 
#'   the model. See [MixSIAR::run_model()] for further information. Only 
#'   necessary if `package = 'mixsiar'`.
#' 
#' @param process_err a logical value indicating if process error is included 
#'   in the model. See [MixSIAR::run_model()] for further information. Only 
#'   necessary if `package = 'mixsiar'`.
#' 
#' @param run a character string or a list specifying MCMC parameters. See
#'   [MixSIAR::run_model()] for further information. Only necessary if 
#'   `package = 'mixsiar'`.
#' 
#' @param alpha.prior a numeric of length 1 specifying the Dirichlet prior on 
#'   p.global. See [MixSIAR::run_model()] for further information. Only 
#'   necessary if `package = 'mixsiar'`.
#' 
#' @param path a character string of length 1. The directory to save results. 
#'   This directory must exist and can be an absolute or a relative path.
#'   Default is the current working directory.
#'
#'
#'
#' @return This function does not return any object in the R console. Results 
#' are stored in a directory (argument `name`) and contain four R objects:
#' - `intervals`: a four dimensions array with the upper and lower bounds of 
#' the credible interval for each sample size, replicate and source. First 
#' dimension represents lower and upper bounds; second dimension corresponds to
#' the number of sources; third dimension is the number of replicates; and
#' fourth dimension is the number of sample size.
#' - `widths`: a three dimensions array with the width (precision) of credible 
#' intervals for each source, each replicate and each sample size. First 
#' dimension corresponds to the number of replicates; second dimension is the 
#' number of sources; and third dimension represents the number of sample size.
#' - `medians`: a three dimensions array with the median (estimate) of credible
#' intervals for each source, each replicate and each sample size. Dimensions 
#' are the same as for widths object.
#' - `datasets`: a four dimensions array with all re-sampled datasets.
#' 
#' A log file is also written and contains all parameters of the simulation.
#' 
#' 
#' 
#' @details 
#' This function assesses the sensitivity of isotopes mixing models to 
#' variation in numbers of samples from source tissues. This tool can be used 
#' prior to full-blown studies in a similar manner than power analyses. It used
#' the function [siar::siarmcmcdirichletv4()]. Alternatively, it can be used 
#' with the function [MixSIAR::run_model()]. User can choose to sample one 
#' particular source (argument `type` sets to `'one source'`) or all the 
#' sources in the same type (argument `type` sets to `'all sources'`). User can
#' also choose to modify consumer data (argument `type` sets to `'consumer'`).
#' Sample sizes are modified assuming a normal distribution with a user defined
#' mean and standard deviation. Samples of different sizes are created from 
#' this distribution, and mixing proportions are estimated for several 
#' replicates of each sample size with the function 
#' [siar::siarmcmcdirichletv4()] or [MixSIAR::run_model()].
#' 
#' 
#' 
#' @references 
#' Lecomte N., Ehrich D., Casajus N., Berteaux D., Giroux M.-A., Yoccoz N.G. 
#' How many is enough? An R package for evaluating the effect of sample size on 
#' estimates and precision of stable isotope mixing solutions. 
#' Submitted to _Methods in Ecology and Evolution_.
#' 
#' 
#' 
#' @seealso [get_output()], [plot_samplesim()], [plot_isospace()]
#' 
#' @export
#'
#' @examples

samplesim <- function(package = "siar", mix, source, discr, type = NULL, 
                      nsamples = NULL, modify = NULL, nrep = 100, 
                      interval = 90, name = NULL, resid_err = TRUE,
                      process_err = FALSE, run = "test", alpha.prior = 1, 
                      path = ".") {


  ## Checks ----
  
  package <- tolower(package)
  
  if (!(package %in% c("siar", "mixsiar"))) {
    stop("Argument 'package' must be one of 'siar' or 'mixsiar'")
  }

  if (is.null(type)) {
    stop("Argument 'type' cannot be NULL.")
  }
  
  if (!(type %in% c("one source", "all sources", "consumer"))) {
    stop("Argument 'type' must be one of 'one source', 'all sources' and ", 
         "'consumer'.")
  }

  if (is.null(modify) && type == "one source") {
    stop("You have to specify which source will be sampled with the argument ",
         "'modify'.")
  }

  if (type %in% c("all sources", "consumer")) {

    modwhich <- NULL

  } else {

    modwhich <- which(source$"source_names" == modify)

    if (length(modwhich) == 0) {
        stop("Wrong source name. Available sources are: ", 
             paste0(source$"source_names", collapse = ", "))
    }
  }

  if (is.null(nsamples)) {
    stop("You have to specify the sample size(s) with the argument ", 
         "'nsamples'.")
  }

  if (is.null(name)) {
    name <- substr(gsub(" |-|:", "", as.character(Sys.time())), 3, 12)
  }

  if (!dir.exists(path)) {
    stop("The directory ", path, " does not exist.")
  }
  
  if (dir.exists(file.path(path, name))) {

    cat("The simulation name already exists. Do you want to overwrite it", 
        "(type 'y' or 'n')?")

    ans <- readLines(n = 1)

    if (tolower(ans) == "n") {
      stop("Please provides a new simulation name.")
    }
  }

  if (length(source$"S_factor1") > 0 || length(mix$"fac_random") > 0 || 
      length(mix$"factors") > 0) {
    stop("Analysis by factors (fixed or random) is not currently supported.")
  }



  ## Directory creation ----

  dir.create(file.path(path, name), showWarnings = FALSE)


  ## Set parameters ----

  nbsizes      <- length(nsamples)
  iso          <- mix$"iso_names"
  nbiso        <- length(iso)
  nbsources    <- length(source$"source_names")
  source_names <- as.factor(as.character(source$"source_names"))



  ## Convert data for siar ----

  if (package == "siar") {

    if (source$"data_type" == "raw") {

      colnames(source$"S_MU")  <- paste0("Mean", colnames(source$"S_MU"))
      colnames(source$"S_SIG") <- paste0("SD", colnames(source$"S_SIG"))
    }

    sources <- data.frame("Source" = source_names)
    
    for (i in 1:ncol(source$"S_MU")) {

      sources <- cbind(sources, source$"S_MU"[ , i])
      colnames(sources)[ncol(sources)] <- colnames(source$"S_MU")[i]

      sources <- cbind(sources, source$"S_SIG"[ , i])
      colnames(sources)[ncol(sources)] <- colnames(source$"S_SIG")[i]
    }
    
    source <- sources
    rownames(source) <- NULL

    mix <- as.data.frame(mix$"data_iso")

    corrects <- data.frame("Source" = source_names)
    
    for (i in 1:ncol(discr$"mu")) {

      corrects <- cbind(corrects, discr$"mu"[ , i])
      colnames(corrects)[ncol(corrects)] <- colnames(discr$"mu")[i]

      corrects <- cbind(corrects, discr$"sig2"[ , i])
      colnames(corrects)[ncol(corrects)] <- colnames(discr$"sig2")[i]
    }
    
    rownames(corrects) <- NULL
  }


  ## Objects creation ----

  medians <- array(dim = c(nrep, nbsources, nbsizes),
                   dimnames = list("replicate" = paste0("replicate", 1:nrep),
                                   "source"    = source_names,
                                   "size"      = paste0("size", nsamples)))

  widths  <- medians

  if (package == "mixsiar") {
    
    intervals <- array(dim = c(2, nbsources, nrep, nbsizes),
                       dimnames = list("interval"  = c("lower", "upper"),
                                       "source"    = source_names,
                                       "replicate" = paste0("replicate", 
                                                            1:nrep),
                                       "size"      = paste0("size", nsamples)))

  } else {

    intervals <- array(dim = c(2, nbiso + nbsources, nrep, nbsizes),
                       dimnames = list("interval"  = c("lower", "upper"),
                                       "source"    = c(as.character(source[ , 
                                                                            1]),
                                                       paste0("sd", 1:nbiso)),
                                       "replicate" = paste0("replicate", 
                                                            1:nrep),
                                       "size"      = paste0("size", nsamples)))
  }

  if (type == "consumer") {

    datasets <- array(dim = c(max(nsamples), nbiso, nrep, nbsizes),
                      dimnames = list("individual" = NULL,
                                      "isotope"    = colnames(mix),
                                      "replicate"  = paste0("replicate", 
                                                            1:nrep),
                                      "size"       = paste0("size", nsamples)))

  } else {

    datasets <- array(dim = c(nbsources, nbiso * 2, nrep, nbsizes),
                      dimnames = list("source"    = source_names,
                                      "isotope"   = colnames(source)[-1],
                                      "replicate" = paste0("replicate", 1:nrep),
                                      "size"      = paste0("size", nsamples)))
  }


  ## Running samplesim ----

  for (m in 1:nbsizes) {

    res <- list()

    for (n in 1:nrep) {

      cat("\n\n>>>> Calculating replicate dataset", n, "for a sample size of", 
          nsamples[m], "\n\n")

      sources.s <- source


      ## SAMPLE DATASET ----

      if (package == "siar") {

        if (type == "one source") {

          sources.s[modwhich, ] <- meansd.nd(data  = source[modwhich, ],
                                             n     = nsamples[m],
                                             input = "means")
        }

        if (type == "all sources") {

          for (k in 1:nbsources) {

            sources.s[k, ] <- meansd.nd(data  = source[k, ],
                                        n     = nsamples[m],
                                        input = "means")
          }
        }

        if (type == "consumer") {

          tar.mn <- apply(mix, 2, mean)
          tar.sd <- apply(mix, 2, stats::sd)
          
          mix.s <- NULL

          for (i in 1:length(tar.mn)) {

            mix.s <- cbind(mix.s, stats::rnorm(nsamples[m], tar.mn[i], 
                                               tar.sd[i]))
          }
          
          colnames(mix.s) <- colnames(mix)
        }
      }

      
      if (package == "mixsiar") {

        if (type == "one source") {

          if (sources.s$"data_type" == "means") {

            MU_SIG <- data.frame("source" = source_names, source$"S_MU", 
                                 source$"S_SIG")

            ssample <- meansd.nd(data  = MU_SIG[modwhich, ],
                                 n     = nsamples[m],
                                 input = "means")

            for (i in 1:length(iso)) {

              sources.s$"S_MU"[modwhich, paste0("Mean", iso[i])]     <- 
                ssample[ , paste0("Mean", iso[i])]
              sources.s$"S_SIG"[modwhich, paste0("SD", iso[i])]      <- 
                ssample[ , paste0("SD", iso[i])]
              sources.s$"MU_array"[modwhich, paste0("Mean", iso[i])] <- 
                sources.s$"S_MU"[modwhich, paste0("Mean", iso[i])]
              sources.s$"SIG2_array"[modwhich, paste0("SD", iso[i])] <- 
                sources.s$"S_SIG"[modwhich, paste0("SD", iso[i])] ^ 2
            }
          }

          if (sources.s$"data_type" == "raw") {

            mu  <- source$"S_MU"
            colnames(mu) <- paste0("Mean", colnames(mu))
            
            sig <- source$"S_SIG"
            colnames(sig) <- paste0("SD", colnames(sig))

            MU_SIG <- data.frame("source" = source_names, mu, sig)

            ssample <- meansd.nd(data  = MU_SIG[modwhich, ],
                                 n     = nsamples[m],
                                 input = "raw")

            for (i in 1:length(iso)) {

              sources.s$"S_MU"[modwhich, iso[i]]  <- mean(ssample[ , iso[i]])
              sources.s$"S_SIG"[modwhich, iso[i]] <- 
                stats::sd(ssample[ , iso[i]])
              sources.s$"SOURCE_array"[modwhich, i, ] <- NA
              sources.s$"SOURCE_array"[modwhich, i, 1:nrow(ssample)] <- 
                ssample[ , iso[i]]
            }
            
            sources.s$"n.rep"[modwhich] <- nrow(ssample)
          }
        }


        if (type == "all sources") {

          if (sources.s$"data_type" == "means") {

            MU_SIG <- data.frame("source" = source_names, source$"S_MU",
                                 source$"S_SIG")

            for (k in 1:nbsources) {

              ssample <- meansd.nd(data  = MU_SIG[k, ], 
                                   n     = nsamples[m],
                                   input = "means")

              for (i in 1:length(iso)) {

                sources.s$"S_MU"[k, paste0("Mean", iso[i])]     <- 
                  ssample[, paste0("Mean", iso[i])]
                sources.s$"S_SIG"[k, paste0("SD", iso[i])]      <- 
                  ssample[, paste0("SD", iso[i])]
                sources.s$"MU_array"[k, paste0("Mean", iso[i])] <- 
                  sources.s$"S_MU"[k, paste0("Mean", iso[i])]
                sources.s$"SIG2_array"[k, paste0("SD", iso[i])] <- 
                  sources.s$"S_SIG"[k, paste0("SD", iso[i])] ^ 2
                sources.s$"n_array"[k] <- nsamples[m]
              }
            }
          }


          if (sources.s$"data_type" == "raw") {

            mu  <- source$"S_MU"
            colnames(mu) <- paste0("Mean", colnames(mu))
            
            sig <- source$"S_SIG"
            colnames(sig) <- paste0("SD", colnames(sig))

            MU_SIG <- data.frame("source" = source_names, mu, sig)

            for (k in 1:nbsources) {

              ssample <- meansd.nd(data  = MU_SIG[k, ],
                                   n     = nsamples[m],
                                   input = "raw")

              for (i in 1:length(iso)) {

                sources.s$"S_MU"[k, iso[i]]  <- mean(ssample[ , iso[i]])
                sources.s$"S_SIG"[k, iso[i]] <- stats::sd(ssample[ , iso[i]])
                sources.s$"SOURCE_array"[k, i, ] <- NA
                sources.s$"SOURCE_array"[k, i, 1:nrow(ssample)] <- 
                  ssample[ , iso[i]]
              }
              
              sources.s$"n.rep"[k] <- nrow(ssample)
            }
            
            sources.s$"SOURCE_array" <- 
              sources.s$"SOURCE_array"[ , , 1:nsamples[m]]
          }
        }


        if (type == "consumer") {

          tar.mn <- apply(mix$"data_iso", 2, mean)
          tar.sd <- apply(mix$"data_iso", 2, stats::sd)
          
          data.s <- NULL

          for (i in 1:length(iso)) {

            data.s <- cbind(data.s, stats::rnorm(nsamples[m], tar.mn[i], 
                                                 tar.sd[i]))
          }
          
          colnames(data.s) <- colnames(mix$"data_iso")

          mix.s            <- mix
          mix.s$"data"     <- data.s
          mix.s$"data_iso" <- data.s
          mix.s$"N"        <- nrow(data.s)
        }
      }


      ## RUN MODELS ----

      if (package == "siar") {

        if (type %in% c("one source", "all sources")) {

          datasets[ , , n, m] <- as.matrix(sources.s[ , -1])

          if (sum(discr$"mu") == 0){

              res[[n]] <- siar::siarmcmcdirichletv4(as.matrix(mix), 
                                                    sources.s)[[15]]

          } else {

            res[[n]] <- siar::siarmcmcdirichletv4(as.matrix(mix),
                                                  sources.s, corrects)[[15]]
          }

        } else {

          datasets[1:nsamples[m], , n, m] <- mix.s

          if (sum(discr$"mu") == 0){

            res[[n]] <- siar::siarmcmcdirichletv4(as.matrix(mix.s),
                                                  source)[[15]]

          } else {

            res[[n]] <- siar::siarmcmcdirichletv4(as.matrix(mix.s),
                                                  source, corrects)[[15]]
          }
        }
      }

      if (package == "mixsiar") {

        if (type %in% c("one source", "all sources")) {

          datasets[ , , n, m] <- as.matrix(cbind(sources.s$"S_MU", 
                                                 sources.s$"S_SIG"))

          MixSIAR::write_JAGS_model(filename    = "MixSIAR_model.txt",
                                    resid_err   = resid_err,
                                    process_err = process_err,
                                    mix         = mix,
                                    source      = sources.s)

          jags <- MixSIAR::run_model(run            = run,
                                     mix            = mix,
                                     source         = sources.s,
                                     discr          = discr,
                                     model_filename = "MixSIAR_model.txt",
                                     alpha.prior    = alpha.prior,
                                     resid_err      = resid_err,
                                     process_err    = process_err)

          res[[n]] <- jags$"BUGSoutput"[[8]][[2]]
          colnames(res[[n]]) <- source_names

        } else {

          datasets[1:nsamples[m], , n, m] <- mix.s$"data"

          MixSIAR::write_JAGS_model(filename    = "MixSIAR_model.txt",
                                    resid_err   = resid_err,
                                    process_err = process_err,
                                    mix         = mix.s,
                                    source      = sources)

          jags <- MixSIAR::run_model(run            = run,
                                     mix            = mix.s,
                                     source         = source,
                                     discr          = discr,
                                     model_filename = "MixSIAR_model.txt",
                                     alpha.prior    = alpha.prior,
                                     resid_err      = resid_err,
                                     process_err    = process_err)

          res[[n]] <- jags$"BUGSoutput"[[8]][[2]]
          colnames(res[[n]]) <- source_names
          
          file.remove("MixSIAR_model.txt")
        }
      }
    }

    credintervals <- lapply(res, credint, interval = interval)

    for (n in 1:nrep) {

      intervals[ , , n, m] <- credintervals[[n]]
      temp                 <- intervals[2, , n, m] - intervals[1, , n, m]
      widths[n, , m]       <- temp[1:nbsources]
      medians[n, , m]      <- apply(res[[n]][ , 1:nbsources], 2, stats:: median)
    }
    
    rm(list = c("credintervals", "res"))
  }


  ## Save results ----

  saveRDS(intervals, file = file.path(path, name, "intervals.rds"))
  saveRDS(widths,    file = file.path(path, name, "widths.rds"))
  saveRDS(medians,   file = file.path(path, name, "medians.rds"))
  saveRDS(datasets,  file = file.path(path, name, "datasets.rds"))


  ## Write Log ----

  cat("====================================================\n", 
      file = file.path(path, name, "logfile.txt"), sep = "")
  cat("        SampleSim - User-defined parameters\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  cat("====================================================\n\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  
  cat("Simulation name: ", name, "\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  cat("Date: ", as.character(Sys.time()), "\n\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  cat("Type of sampling: ", type, "\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  cat("Package: ", package, "\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)

  if (!is.null(modwhich)) {
    
    cat("Modified source: ", modwhich, "\n", 
        file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
    cat("Modified source name: ", as.character(source_names[modwhich]), "\n", 
        file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  }

  cat("Number of replicates by sample size: ", nrep, "\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  cat("Sample sizes: ", paste(nsamples, collapse = ", "), "\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  cat("Credible intervals width: ", interval, "\n\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)
  
  cat("====================================================\n\n", 
      file = file.path(path, name, "logfile.txt"), sep = "", append = TRUE)


  cat("Results have been stored in:", file.path(path, name), "\n")
  
  invisible(NULL)
}
