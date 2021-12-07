#' @import hyper2

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

    ratio_1 <- (alpha + counts) / alpha_n # length N
    ratio_2 <- c(n[-1],0) / alpha_n #  length N

    post_scores <- ratio_1[1]
    for(i in 2:(N)){
      post_scores <- c(post_scores, ratio_1[i] * prod(ratio_2[1:i]))
    }
    post_scores <- log(post_scores) # log-scale
    post_scores <- post_scores - logSumExp(post_scores) # normalize to sum 1
    post_scores <- apply(cbind(post_scores, -10), 1, logSumExp) # add small additive constant to avoid 0
    names(post_scores) <- names(counts)
  }
  else{
    # Hankin's hyperdirichlet expected value
    fs <- apply(model$ensemble.params$output$ensemble_matrix == 1, 1, which) # add feature sets as index lists
    d <- rep(1, length(fs))   # set count 1 for each feature set

    fs <- append(fs, as.list(1:ncol(model$data))) # add each feature individually to define prior
    d <- c(d, model$user.params$weights) # add feature weights as prior

    h <- hyper2(L = fs, d = d)

    samples <- hyper2::rp(500, h)
    post_scores <- colMeans(samples)
    post_scores <- log(post_scores)
    names(post_scores) <- names(model$ensemble.params$output$counts)
  }

  return(post_scores)

}
