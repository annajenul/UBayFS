#' @export

likelihood <- function(theta, ensemble.params, log = TRUE){

  vals <- apply(ensemble.params$output$full_counts, 1, dmultinom, prob = theta, log = log)

  if(log){
    return(sum(vals))
  }
  else{
    return(prod(vals))
  }

}
