#' Plot samplesim simulation results
#' 
#' @description
#' This plot function is a graphical representation of the effects of sample 
#' size on estimates and precision of stable isotope mixing solutions. Two 
#' plots are currently available: a plot for the width of the credible interval
#' and one for the median of the posterior distribution, both displayed for 
#' each sample size and each source.
#'
#' @inheritParams get_output
#'
#' @details 
#' This plot function automatically loads results data stored by the 
#' [samplesim()] function from the simulation name. It produces two plots. 
#' The first plot presents the width of the credible interval displayed for 
#' each sample size and each source. The second represents the median of the 
#' posterior distribution for each sample size and each source.
#'
#' @return NULL
#' 
#' @seealso [samplesim()], [get_output()], [plot_isospace()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Please visit the vignette available at: 
#' ## https://ahasverus.github.io/samplesim/articles/samplesim.html
#' }

plot_samplesim <- function(name, path = ".", change = FALSE, reference = NULL) {


  tab <- get_output(name, path, change, reference)

  if (change) {

    gg <- ggplot(aes_string(x = "size", y = "value", group = "source"), 
                 data = tab) +
      
      geom_point(aes_string(color = "source"), data = tab, 
                 position = position_dodge(0)) +
      
      geom_line(aes_string(color = "source"), data = tab,
                position = position_dodge(0)) +
      
      labs(x = "Sample size", y = "Change in values (%)", color = "Sources") +
      
      facet_grid(. ~ type) +
      
      theme_light() +
      theme(legend.position = "bottom", legend.title = element_blank(),
            strip.text.x = element_text(face = "bold"))
      
    print(gg)

  } else {

    gg <- ggplot(aes_string(x = "size", y = "value"), data = tab) +
      
      geom_boxplot(aes_string(color = "source"), data = tab, width = 1.0,
                   outlier.shape = NA) +
      
      labs(x = "Sample size", y = "Values", color = "Sources") +
      
      coord_cartesian(ylim = c(0, max(tab$value))) +
      
      facet_grid(. ~ type) +
      
      theme_light() +
      theme(legend.position = "bottom", legend.title = element_blank(),
            strip.text.x = element_text(face = "bold"))
    
    print(gg)
  }

  invisible(NULL)
}
