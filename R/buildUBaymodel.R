#' Build an ensemble for UBayFS
#' @description builds a data structure for UBayFS and trains an ensemble of elementary feature selectors
#' @details The function aggregates input parameters for UBayFS - including data, parameters defining ensemble and user knowledge and parameters specifying the optimization procedure - and trains the ensemble model
#' @param data a matrix of input data
#' @param target a vector (factor) of labels for data
#' @param M the number of elementary models in the ensemble
#' @param tt_split the ratio of samples drawn for building an elementary model (train-test-split)
#' @param nr_features number of features to select in each elementary model
#' @param method a vector denoting the method(s) used as elementary models; options: "mRMR", "Laplacian score"
#' @param weights the vector of user-defined prior weights for each feature
#' @param lambda a positive scalar denoting the overall strength of the constraints
#' @param constraints a list containing a relaxed system Ax<=b of user constraints, given as matrix A, vector b and vector or scalar rho (relaxation parameters); see buildConstraints function
#' @param block_constraints a list containing a relaxed system Ax<=b of user constraints on feature blocks, given as matrix A, vector b and vector or scalar rho (relaxation parameters); see buildConstraints function
#' @param optim_method the method to evaluate the posterior distribution. Options "GA" (genetic algorithm) and "MH" (Metropolis-Hastrings MCMC) are supported.
#' @param popsize size of the initial population of the genetic algorithm for model optimization
#' @param maxiter maximum number of iterations of the genetic algorithm for model optimization
#' @param shiny TRUE indicates that the function is called from Shiny dashboard
#' @return a UBaymodel object containing the following list elements: data, target, user.params (parameters representing user knowledge), ensemble.params (parameters representing the likelihood) and optim.params (parameters for genetic algorithm)
#' @examples
#' # build a UBayFS model using Wisconsin breast cancer dataset
#' data(wbc) # dataset
#' c <- buildConstraints(constraint_types = "max_size",
#'                       constraint_vars = list(10),
#'                       num_elements = ncol(wbc$data),
#'                       rho = 1) # prior constraints
#' w <- rep(1, ncol(wbc$data)) # weights
#' model <- build.UBaymodel(
#'                      data = wbc$data,
#'                      target = wbc$labels,
#'                      constraints = c,
#'                      weights = w
#' )
#'
#' # include block-constraints
#' c_block <- buildConstraints(constraint_types = "max_size",
#'                             constraint_vars = list(2),
#'                             num_elements = length(wbc$blocks),
#'                             rho = 10,
#'                             block_list = wbc$blocks)
#' model <- build.UBaymodel(
#'                      data = wbc$data,
#'                      target = wbc$labels,
#'                      constraints = c,
#'                      block_constraints = c_block,
#'                      weights = w
#' )
#' @import Rdimtools
#' @import caret
#' @import glmnet
#' @import mRMRe
#' @import shiny
#' @export

build.UBaymodel = function(data, target, 															# data + labels
                       M = 100, tt_split = 0.75, 												# number of train-test-splits, split ratio
                       nr_features = 10,														# number of features to select by elementary FS
                       method = "mRMR",
                       weights = 1, 														# user weights
                       constraints = NULL,
                       block_constraints = NULL,
                       lambda = 1, 														# constraint strength
                       optim_method = "GA",
                       popsize = 50, 														# number of initial candidates (total)
                       maxiter = 100,
                       shiny = FALSE){														# elementary FS to use

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
  if(!all(method %in% c("mRMR", "mrmr", "Laplacian score", "laplace", "lasso", "LASSO", "fisher", "Fisher", "RFE", "rfe"))){
    stop("Error: unknown method")
  }
  if(!is.numeric(lambda) | lambda <=0){
    stop("Error: lambda must be a scalar greater than 0")
  }

  # initialize matrix
  ensemble_matrix = c()

  for(i in 1:M){																				# perform M runs

    # stratified train-test-split
    train_index = caret::createDataPartition(target,
                                             p = tt_split,
                                             list = FALSE)
    test_index = setdiff(1:length(target),
                         train_index)

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
        ranks = do.fscore(X = train_data, label = train_labels, ndim = nr_features)$featidx
      }
      else if(f %in% c("mrmr", "mRMR")){														# type: mRMR
        dat = data.frame(train_data, "class" = train_labels)									# change data format to data.frame
        dat$class = factor(dat$class, 															# change label format to ordered factor
                           ordered = TRUE)
        rs = mRMR.classic(data = mRMR.data(dat), 												# use mRMR.classic function (package mRMRe)
                          target_indices = ncol(dat),
                          feature_count = nr_features)
        #cat("rs filters", unlist(rs@filters), "\n")
        ranks = unlist(rs@filters)[																# extract selected features
          order(unlist(rs@scores),
                decreasing = TRUE)]
        #cat("mrmr ranks", ranks, "\n")
      }

      else if(f %in% c("lasso", "LASSO")){
        cv.lasso <- cv.glmnet(train_data, train_labels, intercept = FALSE, alpha = 1, family = "binomial")
        model <- glmnet(train_data, train_labels, intercept = FALSE, alpha = 1, family = "binomial",
                        lambda = cv.lasso$lambda.min)
        ranks = which(as.vector(model$beta) != 0)
      }

      else if(f %in% c("RFE", "rfe")){
        control <- rfeControl(functions=rfFuncs, method="cv", number=5)
        results <- rfe(train_data, train_labels, sizes = nr_features, rfeControl=control)
        ranks = which(colnames(train_data) %in% results$optVariables)
      }

      else{
        stop(paste0("Error: unknown method", f))												# catch unknown methods
      }

      # remove unknown or duplicated features from feature set
      vec = rep(0, ncol(data))
      vec[
        nconst_cols[unique(ranks)[unique(ranks) <= ncol(train_data)]]
      ] = 1

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
    ensemble.params = list(
      input = list( tt_split = tt_split,
                    M = M,
                    method = method,
                    nr_features = nr_features),
      output = list(counts = counts)
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
