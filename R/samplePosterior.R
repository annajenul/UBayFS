#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom pbapply pbapply
#' @export

sample.posterior <- function(user.params, ensemble.params, sampling.params){

  # sample from prior
  sample <- sample.prior(user.params, sampling.params)

  # evaluate likelihood
  ## create parallel cluster
  cl <- parallel::makeCluster(parallel::detectCores() - 1)

  ## create acceptance probabilities
  likelihood_vals <- pbapply(sample, 1, likelihood,
                             ensemble.params = ensemble.params,
                             log = TRUE,
                             cl = cl)

  ## stop cluster
  parallel::stopCluster(cl)

  return(list(
    prior_sample = sample,
    likelihood_vals = likelihood_vals))

}
