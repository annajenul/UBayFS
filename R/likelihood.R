#' @export

likelihood <- function(theta, ensemble.params){

  return(apply(ensemble.params$output$full_counts, 1, dmultinom, prob = theta, log = TRUE))

}
