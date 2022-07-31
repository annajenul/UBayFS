#' Admissibility function (kappa)
#' @description Evaluate the value of the admissibility function kappa.
#' @param state a binary membership vector describing a feature set
#' @param constraints a list containing a matrix A and a vector b representing the inequality system Ax<=b and a vector rho
#' @param log whether the admissibility should be returned on log scale
#' @return An admissibility value
#' @importFrom matrixStats logSumExp
#' @export

admissibility = function(state, constraints, log = TRUE){

  # parse input
  A = constraints$A
  b = constraints$b
  rho = constraints$rho

  # check input
  if(!is.null(A)){
    if(ncol(A) != length(state) | nrow(A) != length(b) | length(b) != length(rho)){
      stop("Error: dimensions of constraints do not match")
    }
  }

  if(is.null(A)){
    ind_inf = integer(0)
  }
  else{
    ind_inf = (rho == Inf)
  }


  # case 1: rho < Inf (term lprob1)
  if(any(!ind_inf)){
    const_fulfilled <- b[!ind_inf] - A[!ind_inf,] %*% state >= 0
    z = (b[!ind_inf] - A[!ind_inf,] %*% state) * rho[!ind_inf]			# compute exponential term (nom) for each constraint (in log-scale)
    lprob1 = log(2) + z - apply(cbind(z,0), 1, logSumExp) # 2 * log(nom) - log(1 + nom)
    #lprob1 = sum(lprob1)							# product over all constraints
    lprob1 = sum(lprob1[!const_fulfilled])
  }
  else{
    lprob1 = 0
  }

  # case 2: rho = Inf (term lprob2)
  if(any(ind_inf)){
    z = (b[ind_inf] - A[ind_inf,] %*% state) >= 0
    lprob2 = log(prod(z))						# 1, if all constraints are fulfilled, -Inf else
  }
  else{
    lprob2 = 0
  }

  if(log){										# return admissibility in log or original scale
    return(lprob1 + lprob2)
  }
  else{
    return(exp(lprob1 + lprob2))
  }
}

# Block admissibility function (kappa)
# @description Evaluates the value of the admissibility function kappa for block constraints.
#' @describeIn admissibility  Evaluates the value of the admissibility function kappa for block constraints.
#' @importFrom matrixStats logSumExp
#' @export
#'
block_admissibility = function(state, constraints, log = TRUE){

  # parse input
  block_matrix = constraints$block_matrix

  if(!is.null(block_matrix)){
    state_block = (block_matrix %*% state) > 0
  }
  else{
    state_block = state
  }
  return(admissibility(state_block, constraints, log))

}




#' Posterior expectation of features
#' @description Compute the posterior score for each feature.
#' @param model UBayFS model
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
