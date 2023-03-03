#' UBayFS feature selection
#' @description Genetic algorithm to train UBayFS feature selection model.
#' @param x a `UBaymodel` created by \link{build.UBaymodel}
#' @param verbose if TRUE: GA optimization output is printed to the console
#' @return a `UBaymodel` with an additional list element output containing the optimized solution, see \link{build.UBaymodel}
#' @importFrom GA ga
#' @importFrom DirichletReg ddirichlet
#' @importFrom matrixStats logSumExp
#' @importFrom methods is
#' @export train

train <- function(x, verbose=FALSE){
  UseMethod('train')
}

#' @method train UBaymodel
#' @export

train.UBaymodel = function(x, verbose=FALSE){

  if(!is(x, 'UBaymodel')){
    stop('Wrong class of model')
  }
  if(is.null(x$constraint.params)){
    stop('At least a max-size constraint must be defined')
  }

  # detect max-size constraint
  feat_const <- which(sapply(x$constraint.params, function(y){return(identical(y$block_matrix, diag(nrow = ncol(x$data))))}))
  ms = x$constraint.params[[feat_const]]$b[which(apply(x$constraint.params[[feat_const]]$A == 1, 1, all))]
  if((!is.numeric(ms)) || (length(ms) == 0)){
    stop('No max-size constraint among constraints')
  }
  else if (ms > ncol(x$data)){
    stop('No max-size constraint among constraints')
  }

  # define posterior expected parameter
  theta = posteriorExpectation(x)

  if(x$optim.params$method == 'GA'){
    message('Running Genetic Algorithm')
    tGA <- train_GA(theta,
                         x$lambda,
                         x$constraint.params,
                         x$optim.params,
                         colnames(x$data),
                         verbose)

    feature_set = tGA[[1]]
    x_start = tGA[[2]]

    # calculate output metrics
    metrics <- apply(feature_set, 1, evaluateFS, model = x)
    NO = nrow(feature_set)
    mut_sim = matrix(1, nrow = NO, ncol = NO)

    if(NO > 1){
      for (i in 1 : (NO - 1)) {
        for (j in (i+1) : NO) {
          mut_sim[i,j] = length(intersect(which(feature_set[i,] == 1), which(feature_set[j,] == 1))) /
            length(union(which(feature_set[i,] == 1), which(feature_set[j,] == 1)))
          mut_sim[j,i] = mut_sim[i,j]
        }
      }
    }

    mut_sim = round(mut_sim, 2)
    rownames(mut_sim) = 1:NO
    colnames(mut_sim) = 1:NO
    x$output <- list(feature_set = feature_set,
                     metrics = metrics,
                     mutual_similarity = mut_sim)
  }
  else{
    stop('Error: method not supported.')
  }

  return(x)
}

neg_loss <- function(state, theta, lambda, constraints){
  return(logSumExp(c(
          theta[state == 1],
          log(lambda) + admissibility(state = state, # log( lambda * admissibility)
                                      constraint_list = constraints,
                                      log = TRUE)
          )))}
getNegLoss <- function(state, model, log = TRUE){
  res <- neg_loss(
    state = state,
    theta = posteriorExpectation(model),
    lambda = model$lambda,
    constraints = model$constraint.params)
  if(!log){
    res <- exp(res)
  }
  return(res)
}


train_GA <- function(theta, lambda, constraints, optim_params, feat_names, verbose){


  target_fct <- function(state){return(neg_loss(state, theta, lambda, constraints))}

  # Greedy algorithm to select starting vectors
  x_start = sampleInitial(post_scores = exp(theta),
                          constraints = constraints,
                          size = optim_params$popsize)


  optim = ga(type = 'binary',								# use GA for optimization
             fitness = target_fct,
             lower = 0,
             upper = 1,
             nBits = length(theta),
             maxiter = optim_params$maxiter,
             popSize = optim_params$popsize,
             suggestions = x_start,
             monitor = verbose
  )
  x_optim = optim@solution									# extract solution
  if(is.vector(x_optim)){
    x_optim <- as.data.frame(t(x_optim))
  }
  else{
    x_optim <- as.data.frame(x_optim)
  }
  colnames(x_optim) <- feat_names

  return(list(x_optim, x_start))
}

#' Initial feature set sampling using probabilistic Greedy algorithm
#' @description Sample initial solutions using a probabilistic version of Greedy algorithm.
#' @param post_scores a vector of posterior scores (prior scores + likelihood) for each feature
#' @param constraints a list containing feature-wise constraints
#' @param size initial number of samples to be created. The output sample size can be lower, since duplicates are removed.
#' @return a matrix containing initial feature sets as rows

sampleInitial <- function(post_scores, constraints, size){

  n = length(post_scores) # number of features
  num_constraints_per_block = sapply(constraints, function(x){return(length(x$rho))})
  cum_num_constraints_per_block = c(0, cumsum(num_constraints_per_block))
  rho = unlist(sapply(constraints, function(x){return(x$rho)}))
  rho = 1 / (1 + rho)

  full_admissibility <- function(state, constraints, constraint_dropout, log = TRUE){
    active_constraints <- which(constraint_dropout == 1)

    res = ifelse(log, 0, 1)
    for(i in 1:length(constraints)){
      active_constraints_in_block = active_constraints[active_constraints %in% ((cum_num_constraints_per_block[i] + 1) : cum_num_constraints_per_block[i+1])] -
        cum_num_constraints_per_block[i]
      if(length(active_constraints_in_block) > 0){
        a = group_admissibility(state,
                          list( A = constraints[[i]]$A[active_constraints_in_block,, drop = FALSE],
                                b = constraints[[i]]$b[active_constraints_in_block],
                                rho = rep(Inf, length(active_constraints_in_block)),
                                block_matrix = constraints[[i]]$block_matrix),
                          log = log)
        res = ifelse(log, res + a, res * a)
      }
    }
    return(res)
  }

  # shuffle features n times (sampling without replacement), with probability proportional to post_scores
  feature_orders = t(
    replicate(size-1,
              sample.int(
                n = n,
                size = n,
                replace = FALSE,
                prob = post_scores) # add a small constant to account for unobserved elements
    )
  )

  x_start = t(apply(feature_orders, 1, function(order){
    constraint_dropout <- apply(cbind(rho, 1-rho),
                                1,
                                sample,
                                x = c(0,1),
                                size = 1,
                                replace = TRUE
    )
    i = 1														# iterate over features in descending order
    x = rep(0, n)
    for(i in 1:n){
      x_new = x
      x_new[order[i]] = 1						# try to add feature
      if(full_admissibility(x_new,
                            constraints,
                            constraint_dropout,
                            log = FALSE) == 1){# verify if constraints are still satisfied
        x = x_new										# if yes, accept feature
      }
      i = i+1
    }
    return(x)
  }))

  #always add feature set with best scores
  feat_const <- which(sapply(constraints, function(y){return(identical(y$block_matrix, diag(nrow = n)))}))
  ms = constraints[[feat_const]]$b[which(apply(constraints[[feat_const]]$A == 1, 1, all))]
  if(is.numeric(ms) && (length(ms) == 1)){
    ms_sel <- order(post_scores, decreasing = TRUE)[1:ms]
    x_start <- rbind(x_start, 1:n %in% ms_sel)
  }
  else{
    stop('ERROR: no max-size constraint')
  }

  return(x_start)
}
