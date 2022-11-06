#' Build an ensemble for UBayFS
#' @description Build a data structure for UBayFS and train an ensemble of elementary feature selectors.
#' @details The function aggregates input parameters for UBayFS - including data, parameters defining ensemble and user knowledge and parameters specifying the optimization procedure - and trains the ensemble model.
#' @param data a matrix of input data
#' @param target a vector (factor) of input labels
#' @param M the number of elementary models to be trained in the ensemble
#' @param tt_split the ratio of samples drawn for building an elementary model (train-test-split)
#' @param nr_features number of features to select in each elementary model
#' @param method a vector denoting the method(s) used as elementary models; options: `mRMR`, `laplace` (Laplacian score)
#' @param prior_model a string denoting the prior model to use; options: `dirichlet`, `wong`, `hankin`; `hankin` is the most general prior model, but also the most time consuming
#' @param weights the vector of user-defined prior weights for each feature
#' @param lambda a positive scalar denoting the overall strength of the constraints
#' @param constraints a list containing a relaxed system `Ax<=b` of user constraints, given as matrix `A`, vector `b` and vector or scalar `rho` (relaxation parameter). At least one max-size constraint must be contained. For details, see \link{buildConstraints}.
#' @param block_constraints a list containing a relaxed system `Ax<=b` of user constraints on feature blocks, given as matrix `A`, vector `b` and vector or scalar `rho` (relaxation parameter). For details, see \link{buildConstraints}.
#' @param optim_method the method to evaluate the posterior distribution. Currently, only the option `GA` (genetic algorithm) is supported.
#' @param popsize size of the initial population of the genetic algorithm for model optimization
#' @param maxiter maximum number of iterations of the genetic algorithm for model optimization
#' @param shiny TRUE indicates that the function is called from Shiny dashboard
#' @return a `UBaymodel` object containing the following list elements:
#' \itemize{
#'   \item `data` - the input dataset
#'   \item `target` - the input target
#'   \item `lambda` - the input lambda value (constraint strength)
#'   \item `prior_model` - the chosen prior model
#'   \item `ensemble.params` -  information about input and output of ensemble feature selection
#'   \item `constraint.params` -  parameters representing the constraints
#'   \item `user.params` - parameters representing the user's prior knowledge
#'   \item `optim.params` - gptimization parameters
#' }
#' @examples
#' # build a UBayFS model using Breast Cancer Wisconsin dataset
#' data(bcw) # dataset
#' c <- buildConstraints(constraint_types = "max_size",
#'                       constraint_vars = list(10),
#'                       num_elements = ncol(bcw$data),
#'                       rho = 1) # prior constraints
#' w <- rep(1, ncol(bcw$data)) # weights
#' model <- build.UBaymodel(
#'                      data = bcw$data,
#'                      target = bcw$labels,
#'                      constraints = c,
#'                      weights = w
#' )
#'
#' # include block-constraints
#' c_block <- buildConstraints(constraint_types = "max_size",
#'                             constraint_vars = list(2),
#'                             num_elements = length(bcw$blocks),
#'                             rho = 10,
#'                             block_list = bcw$blocks)
#' model <- build.UBaymodel(
#'                      data = bcw$data,
#'                      target = bcw$labels,
#'                      constraints = c,
#'                      block_constraints = c_block,
#'                      weights = w
#' )
#' @import Rdimtools
#' @import glmnet
#' @import mRMRe
#' @import shiny
#' @import caret
#' @importFrom GSelection feature.selection
#' @export

build.UBaymodel = function(data,
                           target,
                           M = 100,
                           tt_split = 0.75,
                           nr_features = 10,
                           method = "mRMR",
                           prior_model = "dirichlet",
                           weights = 1,
                           constraints = NULL,
                           block_constraints = NULL,
                           lambda = 1,
                           optim_method = "GA",
                           popsize = 50,
                           maxiter = 100,
                           shiny = FALSE){

  # check input
  if(!is.matrix(data)){
    data = as.matrix(data)
  }
  if(nrow(data) != length(target)){
    stop("Error: number of labels must match number of data rows")
  }
  if(M %% 1 != 0 | M <= 0){
    stop("Error: M must be a positive integer")
  }
  if(tt_split < 0 | tt_split > 1){
    stop("Error: tt_split must be between 0 and 1")
  }
  else if(tt_split < 0.5 | tt_split > 0.99){
    warning("Warning: tt_split should not be outside [0.5,0.99]")
  }
  if(!all(method %in% c("mRMR", "mrmr", "Laplacian score", "laplace", "lasso", "LASSO", "fisher", "Fisher", "RFE", "rfe", "hsic", "HSIC",
                        "dtree", "DTREE"))){
    stop("Error: unknown method")
  }
  if(!is.numeric(lambda) | lambda <=0){
    stop("Error: lambda must be a scalar greater than 0")
  }
  if(!(prior_model %in% c("dirichlet", "wong", "hankin"))){
    stop("Error: unknown prior_model")
  }

  # initialize matrix
  ensemble_matrix = c()
  family = ifelse(is.factor(target), "binomial", "gaussian")
  if(any(method %in% c("lasso", "LASSO"))){cv.lasso <- cv.glmnet(as.matrix(data), target, intercept = FALSE, alpha = 1, family = family, nfolds=3)}

  for(i in 1:M){																				# perform M runs

    # stratified train-test-split
    train_index = caret::createDataPartition(target,
                                             p = tt_split,
                                             list = FALSE)
    test_index = setdiff(1:length(target),
                         train_index)

    train_index = build_train_set(target, tt_split)
    test_index = setdiff(1:length(target), train_index)

    # data preprocessing
    nconst_cols = which(apply(data[train_index,], 2, 											# identify columns with constant features
                              function(x){return(length(unique(x)))}) > 1)
    train_data = scale(data[train_index,nconst_cols])											# scale data
    train_labels = target[train_index]															# prepare labels

    # generate elementary FS
    for(f in method){
      if(f %in% c("laplace", "Laplacian score")){												# type: Laplacian score
        ranks = do.lscore(train_data,ndim = nr_features)$featidx									# use do.lscore function (package Rdimtools)
      }
      else if(f %in% c("fisher", "Fisher")){
        if(is.numeric(train_labels)){stop("Fisher score cannot be used for regression!")}
        ranks = do.fscore(X = train_data, label = train_labels, ndim = nr_features)$featidx

      }
      else if(f %in% c("mrmr", "mRMR")){														# type: mRMR
        dat = data.frame(train_data, "class" = train_labels)									# change data format to data.frame
        if(is.factor(train_labels)){
        dat$class = factor(dat$class, 															# change label format to ordered factor
                           ordered = TRUE)}
        rs = mRMR.classic(data = mRMR.data(dat), 												# use mRMR.classic function (package mRMRe)
                          target_indices = ncol(dat),
                          feature_count = nr_features)

        ranks = unlist(rs@filters)[																# extract selected features
          order(unlist(rs@scores),
                decreasing = TRUE)]

      }

      else if(f %in% c("lasso", "LASSO")){
        model <- glmnet(train_data, train_labels, intercept = FALSE, alpha = 1, family = family,
                        lambda = cv.lasso$lambda.min)
        ranks = which(as.vector(model$beta) != 0)
      }

      else if(f %in% c("RFE", "rfe")){
        if(is.factor(train_labels)){
          control <- rfeControl(functions=rfFuncs, method = "cv", number = 2)
        }
        else{
          control <- rfeControl(functions=lmFuncs, method = "cv", number = 2)
        }
        results <- rfe(train_data, train_labels, sizes = nr_features, rfeControl=control)
        ranks = which(colnames(train_data) %in% results$optVariables)
      }
      else if(f %in% c("hsic", "HSIC")){
        ifelse(is.factor(train_labels), {tl = as.numeric(as.integer(train_labels)-1)}, {tl = train_labels})
        hsic_try <- try({
        results = feature.selection(train_data, tl, nr_features)
        ranks = results$hsic_selected_feature_index
        })
      }
      else if(f %in% c("dtree", "DTREE")){
        rf_data = as.data.frame(cbind(train_labels, train_data))
        #colnames(rf_data) <- make.names(colnames(rf_data))
        #tree = rpart::rpart(train_labels~., data = rf_data)
        #
        if(is.factor(train_labels)){
          tree = caret::train(as.factor(train_labels) ~ .,
                data=rf_data,
                method="rpart")
        }
        else{
          tree = caret::train(train_labels ~ .,
                              data=rf_data,
                              method="rpart")
        }
        ranks = which(colnames(train_data) %in% names(tree$finalModel$variable.importance)[1:nr_features])
      }

      else{
        stop(paste0("Error: unknown method", f))												# catch unknown methods
      }

      # remove unknown or duplicated features from feature set
      vec = rep(0, ncol(data))
      if (!f %in% c("hsic", "HSIC") || is(hsic_try, "try-error")){
      vec[
        nconst_cols[unique(ranks)[unique(ranks) <= ncol(train_data)]]
      ] = 1
      }
      # generate matrix of selected features
      ensemble_matrix = rbind(ensemble_matrix, vec)
      rownames(ensemble_matrix)[nrow(ensemble_matrix)] = paste(f, i)
    }

    if(shiny){
      shiny::incProgress(amount = 1/M)
    }
  }

  # structure results
  counts = colSums(ensemble_matrix)
  names(counts) = colnames(data)

  # define return object
  obj = list(
    data = data,
    target = target,
    lambda = lambda,
    prior_model = prior_model,
    ensemble.params = list(
      input = list( tt_split = tt_split,
                    M = M,
                    method = method,
                    nr_features = nr_features),
      output = list(counts = counts,
                    ensemble_matrix = ensemble_matrix)
    )
  )
  class(obj) = "UBaymodel"

  obj = setConstraints(obj, constraints)
  obj = setBlockConstraints(obj, block_constraints)
  obj = setWeights(obj, weights)
  obj = setOptim(obj,
            method = optim_method,
            popsize = popsize,
            maxiter = maxiter
        )

  return(obj)
}


#' Check whether an object is a UBaymodel
#' @description Perform consistency checks of a UBaymodel.
#' @param x an object to be checked for class consistency
#' @export

is.UBaymodel <- function(x){
  return(class(x) == "UBaymodel")
}


#' Set optimization parameters in a UBaymodel object
#' @description Set the optimization parameters in a UBaymodel object.
#' @param model a UBaymodel object created using build.UBaymodel
#' @param method the method to evaluate the posterior distribution; currently only"GA" (genetic algorithm) is supported
#' @param popsize size of the initial population of the genetic algorithm for model optimization
#' @param maxiter maximum number of iterations of the genetic algorithm for model optimization
#' @return A UBaymodel object with updated optimization parameters
#' @seealso build.UBaymodel
#' @importFrom methods is
#' @export

setOptim = function(model, method = "GA", popsize, maxiter){

  if(!is(model, "UBaymodel")){
    stop("Wrong class of model")
  }

  if(is.null(method) | !(method %in% c("GA"))){
    stop("Error: method not supported")
  }

  if(popsize < 10 | maxiter < 10){
    stop("Error: popsize or maxiter < 10 does not make sense")
  }

  model$optim.params <- list(method = method,
                             popsize = popsize,
                             maxiter = maxiter)

  return(model)
}

#' Set weights in UBaymodel object
#' @description Set the prior weights in a UBaymodel object.
#' @param model a UBaymodel object created using build.UBaymodel
#' @param weights the vector of user-defined prior weights for each feature
#' @param block_list the list of feature indices for each block; only required, if block-wise weights are specified and block_matrix is NULL
#' @param block_matrix the matrix containing affiliations of features to each block; only required, if block-wise weights are specified and block_list is NULL
#' @return A UBaymodel object with updated prior weights
#' @seealso build.UBaymodel
#' @importFrom methods is
#' @export

setWeights = function(model, weights, block_list = NULL, block_matrix = NULL){

  if(!is(model, "UBaymodel")){
    stop("Wrong class of model")
  }

  if(is.null(weights)){
    stop("Error: weights cannot be empty")
  }


  if(!is.null(block_matrix) | !is.null(block_list)){
    if(is.null(block_matrix)){
      block_matrix = matrix(0, nrow = length(block_list), ncol = max(unlist(block_list)))
      for(i in 1:length(block_list)){
        block_matrix[i,block_list[[i]]] <- 1
      }
    }
    if(nrow(block_matrix) != length(weights)){
      stop("Error: wrong length of weights vector: must match number of blocks, if block_matrix or block_list are provided")
    }
    weights = as.vector(t(block_matrix) %*% weights)
  }

  if((length(weights) > 1) & (length(weights) != ncol(model$data))){
    stop("Error: length of prior weights does not match data matrix")
  }
  else if(length(weights) == 1){
    weights = rep(weights, ncol(model$data))
  }

  if(any(weights <= 0)){
    stop("Error: weights must be positive")
  }

  model$user.params$weights = weights

  return(model)
}


#' Perform stratified data partition.
#' @description Sample indices for training from the data.
#' @param y a column, often the target, by which the data shall be partitioned.
#' @param tt_split the percentage of data used for training in each ensemble model.
#' @return Data indices for training ensembles

build_train_set <- function(y, tt_split){
  n = length(unique(y))
  if(n == 1){stop("Error: Target must have more than one unique value!")}
  else if(n > 2){
    return(sample(1:length(y), floor(tt_split*length(y))))
  }
  else{
    uv = unique(y)
    s = c()
    for (i in 1:length(uv)) {
      group = which(y == uv[i])
      s = c(s, sample(group, floor(tt_split*length(group))))
    }
    return(s)
  }
}

