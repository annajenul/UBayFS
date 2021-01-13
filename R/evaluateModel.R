#' @export

evaluate_model <- function(model){
  x = model$output$success_probs
  return(-sum(x[x>0] * log(x[x>0])) / log(length(x)))
}
