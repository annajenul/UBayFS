#' @export

likelihood <- function(theta, ensemble.params, log = TRUE){

  return(apply(ensemble.params$output$full_counts, 1, dmultinom, prob = theta, log = log))

}
