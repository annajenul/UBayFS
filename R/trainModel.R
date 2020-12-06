#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel pbapply
#' @export

train.UBaymodel <- function(model, n.samples = 1e7){

  # sample from prior
  sample <- sample.prior(model$user.params, n.samples)

  # evaluate likelihood
  ## create parallel cluster
  cl <- parallel::makeCluster(parallel::detectCores() - 1)

  ## create acceptance probabilities
  likelihood_vals <- pbapply(sample, 1, likelihood,
                             ensemble.params = model$ensemble.params,
                             log = TRUE,
                             cl = cl)

  ## stop cluster
  parallel::stopCluster(cl)

  model$output <- list(map = sample[which.max(likelihood_vals),],
                       max_median = sample(which.min(abs(likelihood_vals - median(likelihood_vals)))))

}
