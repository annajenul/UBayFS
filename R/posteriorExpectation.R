posteriorExpectation <- function(model){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  post_scores = model$ensemble.params$output$counts + model$user.params$weights
  post_scores = log(post_scores) - log(sum(post_scores))
  return(post_scores)

}
