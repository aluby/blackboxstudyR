#' Fit Rasch Model
#'
#' @param irt_data List formatted as in irt_data_bb()
#' @param iterations Number of MCMC iterations per chain
#' @param n_chains Number of MCMC chains to run
#' @return stan object
#'
#' @importFrom rstan stan
#'
#' @examples
#' \dontrun{
#' fit_rasch()
#' }
#' @export
fit_rasch = function(irt_data, iterations = 600, n_chains = 4){
  if(!any(c("y", "ii", "jj", "N", "J", "I") %in% names(irt_data))){
    stop("Data does not contain necessary elements, try irt_data_bb(..)")
  }
  model = rstan::sampling(stanmodels$rasch, data = irt_data, iter = iterations, chains = n_chains)
  return(model)
}