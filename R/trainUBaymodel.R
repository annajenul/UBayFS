#' UBayFS feature selection
#' @description genetic algorithm to train UBayFS feature selection model
#' @param x a UBaymodel created by build.UBaymodel
#' @return a UBaymodel with an additional list element output containing the maximum a-posteriori estimate (map)
#' @importFrom GA ga
#' @importFrom DirichletReg ddirichlet
#' @importFrom matrixStats logSumExp
#' @export train

train <- function(x){
  UseMethod("train")
}

#' @method train UBaymodel
#' @export

train.UBaymodel = function(x){

  if(class(x) != "UBaymodel"){
    stop("Wrong class of model")
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

