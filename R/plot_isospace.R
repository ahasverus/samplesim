plot_isospace <- function(
  mix,
  source,
  discr,
  filename = "isospace",
  plot_save_pdf = FALSE,
  plot_save_png = FALSE
) {



  if (length(mix$FAC) > 0 || length(source$S_factor1) > 0) {

    stop("Your data contain at least one factor. Please use MixSIAR::plot_data() instead.")
  }



  spaces <- combn(1:mix$n.iso, 2)
  nplots <- ncol(spaces)

  for (i in 1:nplots) {

    iso_names <- mix$iso_names[spaces[ , i]]

    if (length(grep("C$", iso_names[1])) == 1) {

      x_label <- expression(paste(delta^13, "C (\u2030)", sep = ""))
    }

    if (length(grep("C$", iso_names[2])) == 1) {

      y_label <- expression(paste(delta^13, "C (\u2030)", sep = ""))
    }

    if (length(grep("N$", iso_names[1])) == 1) {

      x_label <- expression(paste(delta^15, "N (\u2030)", sep = ""))
    }

    if (length(grep("N$", iso_names[2])) == 1) {

      y_label <- expression(paste(delta^15, "N (\u2030)", sep = ""))
    }

    if (length(grep("S$", iso_names[1])) == 1) {
      x_label <- expression(paste(delta^34, "S (\u2030)", sep = ""))
    }

    if (length(grep("S$", iso_names[2])) == 1) {

      y_label <- expression(paste(delta^34, "S (\u2030)", sep = ""))
    }

    if (length(grep("O$", iso_names[1])) == 1) {

      x_label <- expression(paste(delta^18, "O (\u2030)", sep = ""))
    }

    if (length(grep("O$", iso_names[2])) == 1) {

      y_label <- expression(paste(delta^18, "O (\u2030)", sep = ""))
    }

    dat_mix <- data.frame(
      x = mix$data_iso[ , iso_names[1]],
      y = mix$data_iso[ , iso_names[2]]
    )

    mu <- sig <- data.frame(
      rep(0, source$n.sources),
      rep(0, source$n.sources)
    )

    colnames(mu) <- colnames(sig) <- iso_names

    for (iso in iso_names) {
        mu[ , iso]  <- source$S_MU[, paste0("Mean", iso)] + discr$mu[, paste0("Mean", iso)]
        sig[ , iso] <- sqrt(source$S_SIG[, paste0("SD", iso)]^2 + discr$sig2[, paste0("SD", iso)])
    }

    dat_src <- data.frame(
      x      = mu[ , 1],
      y      = mu[ , 2],
      xmin   = mu[ , 1] - sig[ , 1],
      xmax   = mu[ , 1] + sig[ , 1],
      ymin   = mu[ , 2] - sig[ , 2],
      ymax   = mu[ , 2] + sig[ , 2],
      source = source$source_names
    )

    if (i > 1) {

      dev.new()
    }

    gg <- ggplot(
      data = dat_src,
      aes(
        x = x,
        y = y)
      ) +
      geom_point(
        data = dat_mix,
        aes(
          x = x,
          y = y
        ),
        colour = "#888888",
        show.legend = TRUE
      ) +
      geom_errorbar(
        data = dat_src,
        aes(
          ymin = ymin,
          ymax = ymax,
          colour = source
        ),
        width = 0
      ) +
      geom_errorbarh(
        data = dat_src,
        aes(
          xmin = xmin,
          xmax = xmax,
          colour = source
        ),
        height = 0
      ) +
      geom_point(
        data = dat_src,
        aes(
          x = x,
          y = y,
          colour = source
        ),
        shape = 24,
        fill = "white",
        size = 3
      ) +
      labs(
        x = x_label,
        y = y_label
      ) +
      theme_light() +
      theme(
        legend.position = "bottom"
      ) +
      theme(
        legend.title = element_blank()
      )

    print(gg)

    if (plot_save_pdf) {

      cairo_pdf(
        filename = paste0(filename, "-space", i, ".pdf"),
        width = 6,
        height = 6
      )
      print(gg)
      dev.off()
    }

    if (plot_save_png) {

      png(
        file = paste0(filename, "-space", i, ".png")
      )
      print(gg)
      dev.off()
    }
  }
}
