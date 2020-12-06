#' @export

train_model <- function(model){

  posterior <- sample.posterior(model$user.params, model$ensemble.params, model$sampling.params)

  sample <- posterior$prior_sample
  likelihood_vals <- posterior$likelihood_vals

  model$output <- list(map = sample[which.max(likelihood_vals),],
                       median = sample[which.min(abs(likelihood_vals - median(likelihood_vals))),])
  return(model)
}
