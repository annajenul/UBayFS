#' @import Rdimtools
#' @import caret
#' @import glmnet
#' @import mRMRe
#' @import rpart
#' @import shiny
#' @export
# function to build elementary models
build.model = function(data, target, 															# data + labels
                       M = 100, tt_split = 0.75, 												# number of train-test-splits, split ratio
                       A = NULL, b = NULL, rho = NULL,											# user constraints
                       weights = NULL, 														# user weights
                       nr_features = 10,														# number of features to select by elementary FS
                       method = "mrmr",
                       shiny = FALSE){														# elementary FS to use

  # check if data is in matrix format
  if(!is.matrix(data)){
    data = as.matrix(data)
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
                    method = method),
      output = list(counts = counts,
                    max_counts = max_counts)
    )
  )
  class(obj) = "UBaymodel"
  return(obj)
}
