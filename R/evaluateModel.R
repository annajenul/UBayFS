#' @export

evaluate_model <- function(model){
  return(data.frame(
    entropy = sapply(model$output, function(x){return(-sum(x[x>0] * log(x[x>0])))}),
    KL_uniform = sapply(model$output, function(x){return(-sum(x[x>0] * log(x[x>0] * length(x))))})
  ))
}
