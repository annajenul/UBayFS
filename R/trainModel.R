#' @export

train_model <- function(model, shiny = FALSE){

  posterior <- sample.posterior(model$user.params, model$ensemble.params, model$sampling.params, shiny = shiny)

  sample <- posterior$sample
  vals <- posterior$vals

  model$output <- list(map = sample[which.max(vals),],
                       median = sample[which.min(abs(vals - median(vals))),])
  return(model)
}
