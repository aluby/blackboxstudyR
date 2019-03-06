library(dplyr)
library(ggplot2)
library(rstan)

#' Plots the difficulty posteriors from IRT analysis
#'
#' @param irt_stan_output Stan object from IRT analysis
#' @examples
#' \dontrun{
#' plot_difficulty_posteriors(im_model)
#' }
#' @export
plot_difficulty_posteriors = function(irt_stan_output) {
  bayesplot::mcmc_intervals_data(as.array(irt_stan_output),
                      regex_pars = 'b',
                      prob_outer = .95) %>%
    ggplot(., aes(
      y = reorder(parameter, m, FUN = median),
      yend = reorder(parameter, m, FUN = median)
    )) +
    geom_segment(aes_(x = ~ ll, xend = ~ hh), size = .8, alpha = .3) +
    geom_segment(aes_(x = ~ l, xend = ~ h), size = 1, alpha = .6) +
    geom_point(aes_(x = ~ m), size = .9) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank()) +
    scale_y_discrete(breaks = NULL) +
    ggtitle('Posterior Intervals for b estimates')
}

#' Plots the proficiency posteriors from IRT analysis
#'
#' @param irt_stan_output Stan object from IRT analysis
#' @examples
#' \dontrun{
#' plot_proficiency_posteriors(im_model)
#' }
#' @export
plot_proficiency_posteriors = function(irt_stan_output) {
  bayesplot::mcmc_intervals_data(as.array(irt_stan_output),
                      regex_pars = 'theta',
                      prob_outer = .95) %>%
    ggplot(., aes(
      y = reorder(parameter, m, FUN = median),
      yend = reorder(parameter, m, FUN = median)
    )) +
    geom_segment(aes_(x = ~ ll, xend = ~ hh), size = .8, alpha = .5) +
    geom_segment(aes_(x = ~ l, xend = ~ h), size = 1, alpha = .8) +
    geom_point(aes_(x = ~ m), size = .9) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank()) +
    scale_y_discrete(breaks = NULL) +
    ggtitle(expression('Posterior Intervals for '~theta~' estimates'))
}

