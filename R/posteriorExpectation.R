posteriorExpectation <- function(model){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  if(model$prior_model == "dirichlet"){
    # dirichlet expected value
    post_scores = model$ensemble.params$output$counts + model$user.params$weights
    post_scores = log(post_scores) - log(sum(post_scores))
  }
  else if(model$prior_model == "wong"){
    # generalized dirichlet expected value
    counts <- model$ensemble.params$output$counts
    alpha <- model$user.params$weights
    beta <- 0

    N <- length(counts)
    n <- cumsum(counts[N:1])[N:1]
    alpha_n <- alpha + n

    ratio_1 <- (alpha + counts) / alpha_n
    ratio_2 <- n[-1] / alpha_n[-N] #  has length N-1


    post_scores <- ratio_1[1]
    for(i in 2:(N-1)){
      post_scores <- c(post_scores, ratio_1[i] * cumprod(ratio_2[1:i])[i])
    }
    post_scores <- c(post_scores, cumprod(ratio_2)[N-1])
    post_scores <- log(post_scores)
  }
  else{
    # Hankin's hyperdirichlet expected value
    stop("This prior model is not yet implemented")
  }

  return(post_scores)

}
