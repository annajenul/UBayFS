#' @export

train_model <- function(model){

  posterior <- sample.posterior(model$user.params, model$ensemble.params, model$sampling.params)

  sample <- posterior$sample
  vals <- posterior$vals

  model$output <- list(map = sample[which.max(vals),],
                       median = sample[which.min(abs(vals - median(vals))),])
  return(model)
}
