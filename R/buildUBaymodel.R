#' Build an ensemble for UBayFS
#' @description builds a data structure for UBayFS and trains an ensemble of elementary feature selectors
#' @details The function aggregates input parameters for UBayFS - including data, parameters defining ensemble and user knowledge and parameters specifying the optimization procedure - and trains the ensemble model
#' @param data a matrix of input data
#' @param target a vector (factor) of labels for data
#' @param M the number of elementary models in the ensemble
#' @param tt_split the ratio of samples drawn for building an elementary model (train-test-split)
#' @param nr_features number of features to select in each elementary model
#' @param method a vector denoting the method(s) used as elementary models; options: "mRMR", "Laplacian score"
#' @param A the matrix defining the constraint system Ax<=b
#' @param b the vector defining the constraint system Ax<=b
#' @param rho the vector of relaxation parameters for the constraint system Ax<=b
#' @param weights the vector of user-defined prior weights for each feature
#' @param popsize size of the initial population of the genetic algorithm for model optimization
#' @param maxiter maximum number of iterations of the genetic algorithm for model optimization
#' @param shiny TRUE indicates that the function is called from Shiny dashboard
#' @return a UBaymodel object containing the following list elements: data, target, user.params (parameters representing user knowledge), ensemble.params (parameters representing the likelihood) and optim.params (parameters for genetic algorithm)
#' @examples
#' # build a UBayFS model using Wisconsin breast cancer dataset
#' d <- loadWisconsin() # dataset
#' c <- buildConstraints("max_size", list(10), ncol(d$data), rho = 1) # prior constraints
#' w <- rep(1, ncol(d$data)) # weights
#' model <- build.UBaymodel(
#'                      data = d$data,
#'                      target = d$labels,
#'                      A = c$A,
#'                      b = c$b,
#'                      rho = c$rho,
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
                       method = "mrmr",
                       A = NULL, b = NULL, rho = NULL,											# user constraints
                       weights = NULL, 														# user weights
                       popsize = 50,
                       maxiter = 100,
                       shiny = FALSE){														# elementary FS to use

  # check input
  if(!is.matrix(data)){
    data = as.matrix(data)
  }
  if(nrow(data) != length(target)){
    stop("Error: number of labels must match number of data rows")
  }
  if(M%%1 != 0 | M <= 0){
    stop("Error: M must be a positive integer")
  }
  if(tt_split < 0 | tt_split > 1){
    stop("Error: tt_split must be between 0 and 1")
  }
  else if(tt_split < 0.5 | tt_split > 0.99){
    warning("Warning: tt_split should not be outside [0.5,0.99]")
  }
  if(!all(method %in% c("mRMR", "mrmr", "Laplacian score", "laplace"))){
    stop("Error: unknown method")
  }
  if(!is.null(A)){
    if(ncol(A) != ncol(data) | nrow(A) != length(b) | length(b) != length(rho)){
      stop("Error: dimensions of constraints do not match")
    }
  }
  if(!is.null(weights)){
    if(length(weights) != ncol(data)){
      stop("Error: length of prior weights does not match data matrix")
    }
  }
  else{
    weights = rep(1, ncol(data))
  }
  if(popsize < 10 | maxiter < 10){
    stop("Error: popsize or maxiter < 10 does not make sense")
  }

  # theoretical maximum count that can be obtained by a single feature in ensemble (= number of elementary FS)
  max_counts = length(method) * M

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
      else if(f %in% c("mrmr", "mRMR")){														# type: mRMR
        dat = data.frame(train_data, "class" = train_labels)									# change data format to data.frame
        dat$class = factor(dat$class, 															# change label format to ordered factor
                           ordered = TRUE)
        rs = mRMR.classic(data = mRMR.data(dat), 												# use mRMR.classic function (package mRMRe)
                          target_indices = ncol(dat),
                          feature_count = nr_features)
        ranks = unlist(rs@filters)[																# extract selected features
          order(unlist(rs@scores),
                decreasing = TRUE)]
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
    user.params = list(
      constraints = list(A = A,
                         b = b,
                         rho = rho),
      weights = weights
    ),
    ensemble.params = list(
      input = list( tt_split = tt_split,
                    M = M,
                    method = method,
                    nr_features = nr_features),
      output = list(counts = counts,
                    max_counts = max_counts)
    ),
    optim.params = list(
      maxiter = maxiter,
      popsize = popsize
    )
  )
  class(obj) = "UBaymodel"
  return(obj)
}
