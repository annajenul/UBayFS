#' @export

evaluate_model <- function(model){
  return(sapply(model$output, function(x){return(-sum(x[x>0] * log(x[x>0])))}) / log(length(model$output)))

}
