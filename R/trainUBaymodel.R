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
  theta = posteriorExpectation(model)

  if(x$optim.params$method == "GA"){
    print("Running Genetic Algorithm")
    feature_set <- train_GA(theta,
                         x$lambda,
                         x$constraint.params$constraints,
                         x$constraint.params$block_constraints,
                         x$optim.params,
                         colnames(x$data))

    # calculate output metrics
    metrics <- apply(feature_set, 1, evaluateFS, model = x)

    if(nrow(feature_set) > 1){
      NO = nrow(feature_set)
      mut_sim = matrix(1, nrow = length(NO), ncol = length(NO))
      for (i in NO) {
        for (j in NO[NO > i]) {
          mut_sim[i,j] = length(intersect(which(feature_set[i,] == 1), which(feature_set[j,] == 1))) /
            length(union(which(feature_set[i,] == 1), which(feature_set[j,] == 1)))
          mut_sim[j,i] = mut_sim[i,j]
        }
      }
      mut_sim = round(mut_sim, 2)
      rownames(mut_sim) = NO
      colnames(mut_sim) = NO
    }
    else{
      mut_sim <- 1
    }
    x$output <- list(feature_set = feature_set,
                     metrics = metrics,
                     mutual_similarity = mut_sim)
  }
  else{
    stop("Error: method not supported.")
  }

  return(x)
}

loss <- function(state, theta, lambda, constraints, block_constraints){
  return( - logSumExp(c(theta[state == 0],
          log(lambda) + admissibility(state = state,
                                      constraints = constraints,
                                      log = TRUE),
          log(lambda) + block_admissibility(state = state,
                                      constraints = block_constraints,
                                      log = TRUE))))}
getLoss <- function(state, model){
  return(loss(
          state = state,
          theta = posteriorExpectation(model),
          lambda = model$lambda,
          constraints = model$constraint.params$constraints,
          block_constraints = model$constraint.params$block_constraints))
}


train_GA <- function(theta, lambda, constraints, block_constraints, optim_params, feat_names){


  target_fct <- function(state){return(loss(state, theta, lambda, constraints, block_constraints))}

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

  return(x_optim)
}

