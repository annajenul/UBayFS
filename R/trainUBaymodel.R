#' UBayFS feature selection
#' @description Genetic algorithm to train UBayFS feature selection model.
#' @param x a `UBaymodel` created by \link{build.UBaymodel}
#' @return a `UBaymodel` with an additional list element output containing the optimized solution.
#' @importFrom GA ga
#' @importFrom DirichletReg ddirichlet
#' @importFrom matrixStats logSumExp
#' @importFrom methods is
#' @export train

train <- function(x){
  UseMethod("train")
}

#' @method train UBaymodel
#' @export

train.UBaymodel = function(x){

  if(!is(x, "UBaymodel")){
    stop("Wrong class of model")
  }
  if(is.null(x$constraint.params$constraints)){
    stop("At least a max-size constraint must be defined")
  }
  ms = x$constraint.params$constraints$b[which(apply(x$constraint.params$constraints$A == 1, 1, all))]
  if((!is.numeric(ms)) || (length(ms) == 0)){
    stop("No max-size constraint among constraints")
  }
  else if (ms > ncol(x$constraint.params$constraints$A)){
    stop("No max-size constraint among constraints")
  }

  # define posterior expected parameter
  theta = posteriorExpectation(x)

  if(x$optim.params$method == "GA"){
    print("Running Genetic Algorithm")
    tGA <- train_GA(theta,
                         x$lambda,
                         x$constraint.params$constraints,
                         x$constraint.params$block_constraints,
                         x$optim.params,
                         colnames(x$data))

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
    stop("Error: method not supported.")
  }

  return(x)
}

neg_loss <- function(state, theta, lambda, constraints, block_constraints){
  return(logSumExp(c(
          theta[state == 1],
          log(lambda) + admissibility(state = state, # log( lambda * admissibility * block_admissibility )
                                      constraints = constraints,
                                      log = TRUE)
                      + block_admissibility(state = state,
                                      constraints = block_constraints,
                                      log = TRUE)
          )))}
getNegLoss <- function(state, model, log = TRUE){
  res <- neg_loss(
    state = state,
    theta = posteriorExpectation(model),
    lambda = model$lambda,
    constraints = model$constraint.params$constraints,
    block_constraints = model$constraint.params$block_constraints)
  if(!log){
    res <- exp(res)
  }
  return(res)
}


train_GA <- function(theta, lambda, constraints, block_constraints, optim_params, feat_names){


  target_fct <- function(state){return(neg_loss(state, theta, lambda, constraints, block_constraints))}

  # Greedy algorithm to select starting vectors
  x_start = sampleInitial(post_scores = exp(theta),
                          constraints = constraints,
                          block_constraints = block_constraints,
                          size = optim_params$popsize)


  optim = ga(type = "binary",								# use GA for optimization
             fitness = target_fct,
             lower = 0,
             upper = 1,
             nBits = length(theta),
             maxiter = optim_params$maxiter,
             popSize = optim_params$popsize,
             suggestions = x_start
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
#' @param block_constraints a list containing block-wise constraints
#' @param size initial number of samples to be created. The output sample size can be lower, since duplicates are removed.
#' @return A matrix containing initial feature sets as rows

sampleInitial <- function(post_scores, constraints, block_constraints, size){

  n = length(post_scores) # number of features
  num_feat_constraints = ifelse(is.null(constraints$A), 0, nrow(constraints$A))
  num_constraints = num_feat_constraints +
    ifelse(is.null(block_constraints$A), 0, nrow(block_constraints$A))
  rho = c(constraints$rho, block_constraints$rho)
  rho = 1 / (1 + rho)

  full_admissibility <- function(state, constraints, block_constraints, constraint_dropout, log = TRUE){
    active_constraints <- which(constraint_dropout == 1)
    active_feat_constraints <- active_constraints[active_constraints <= num_feat_constraints]
    active_block_constraints <- active_constraints[active_constraints > num_feat_constraints] - num_feat_constraints

    res = ifelse(log, 0, 1)
    if(!is.null(constraints) & (length(active_feat_constraints) > 0)){

      a = admissibility(state,
                        list( A = constraints$A[active_feat_constraints,, drop = FALSE],
                              b = constraints$b[active_feat_constraints],
                              rho = rep(Inf, length(active_feat_constraints))),
                        log = log)
      res = ifelse(log, res + a, res * a)
    }
    if(!is.null(block_constraints) & length(active_block_constraints) > 0){
      a = block_admissibility(state,
                              list( A = block_constraints$A[active_block_constraints,, drop = FALSE],
                                    b = block_constraints$b[active_block_constraints],
                                    rho = rep(Inf, length(active_block_constraints)),
                                    block_matrix = block_constraints$block_matrix),
                              log = log)
      res = ifelse(log, res + a, res * a)
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
                                # prob = c(constraint_dropout_rate, 1-constraint_dropout_rate)
    )
    i = 1														# iterate over features in descending order
    x = rep(0, n)
    for(i in 1:n){
      x_new = x
      x_new[order[i]] = 1						# try to add feature
      if(full_admissibility(x_new,
                            constraints,
                            block_constraints,
                            constraint_dropout,
                            log = FALSE) == 1){# verify if constraints are still satisfied
        x = x_new										# if yes, accept feature
      }
      i = i+1
    }
    return(x)
  }))

  #always add feature set with best scores
  ms <- constraints$b[which(apply(constraints$A == 1, 1, all))]
  if((is.numeric(ms)) && (length(ms) != 0)){
    ms_sel <- order(post_scores, decreasing = TRUE)[1:ms]
    x_start <- rbind(x_start, 1:n %in% ms_sel)
  }
  else{
    stop("ERROR: no max-size constraint")
  }


  return(x_start)
}
