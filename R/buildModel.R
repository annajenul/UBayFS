#' @import Rdimtools
#' @import caret
#' @import glmnet
#' @import mRMRe
#' @import rpart
#' @export


build.model <- function(data, target, reg.param = NULL, K = 100,
                        trainsize = 0.75, A = NULL, b = NULL, rho = NULL,
                        weights = NULL, verbose = TRUE,
                        nr_features = 10, ranking = FALSE,
                        method = c("laplace", "fisher", "mrmr", "RENT"),
                        t_max = 1e3, t_bi = 1e2){

  if(!is.matrix(data)){
    data <- as.matrix(data)
  }
  data = scale(data)
  if(is.null(reg.param)){
    print("auto-selecting regularization parameters")
    reg.param <- select.reg.param(data, target, model.type)
  }

  rank_matrix = NULL

  max_counts = ifelse(ranking, length(method) * K * nr_features, length(method) * K)
  print(paste0("Running ",K, " elementary models"))
  for(i in 1:K){
    train_index <- createDataPartition(target, p = trainsize, list = FALSE)
    test_index <- setdiff(1:length(target), train_index)

    train_data = data[train_index,]
    train_labels = target[train_index]
    for(f in method){
      if(f %in% c("laplace", "Laplacian score")){
        ranks = do.lscore(train_data,ndim=nr_features, preprocess = 'cscale')$featidx
      }
      else if(f %in% c("fisher", "Fisher score")){
        ranks = do.fscore(X = train_data, label = train_labels, ndim = nr_features, preprocess = 'cscale')$featidx
      }
      else if(f %in% c("mrmr", "mRMR")){
        dat = data.frame(train_data, "class"=train_labels)
        dat$class = factor(dat$class, ordered=TRUE)
        rs = mRMR.classic(data=mRMR.data(dat), target_indices = ncol(dat), feature_count = nr_features)
        ranks = unlist(rs@filters)[order(unlist(rs@scores), decreasing = TRUE)]
      }
      else if(f %in% c("RENT", "enet", "elastic net")){
        mod <- glmnet(
                      x = train_data,
                      y = train_labels,
                      family = 'binomial',
                      lambda = 1 / (reg.param$lambda * nrow(train_data)),
                      alpha = reg.param$alpha)

        ranks=order(abs(as.vector(mod$beta)), decreasing = TRUE)[1:nr_features]
      }
      else if(f %in% c("tree", "classification_tree")){
        rf_data = cbind(train_labels, train_data)
        fit = rpart(train_labels~., data= as.data.frame(rf_data), method = "class")
        ranks = which(colnames(train_data) %in% names(fit$variable.importance))
      }
      else{
        stop(paste0("Error: unknown method", f))
      }
      vec <- rep(0, ncol(train_data))
      if(ranking){
        vec[unique(ranks)[unique(ranks) <= ncol(train_data)]] <- nr_features : 1
      }
      else{
        vec[unique(ranks)[unique(ranks) <= ncol(train_data)]] <- 1
      }

      rank_matrix <- rbind(rank_matrix, vec)
      rownames(rank_matrix)[nrow(rank_matrix)] <- paste(f, i)
    }
  }

  full_counts = rank_matrix
  colnames(full_counts) <- colnames(data)
  counts = colSums(rank_matrix)

  obj <- list(
    data = data,
    target = target,
    user.params = list(
      constraints = list(A = A,
                         b = b,
                         rho = rho),
      weights = weights
    ),
    ensemble.params = list(
      input = list( trainsize=trainsize,
                    K=K,
                    reg.param=reg.param,
                    method=method),
      output = list(full_counts = full_counts,
                    counts = counts,
                    max_counts = max_counts)
    ),
    sampling.params = list(
      t_max = t_max,
      t_bi = t_bi
    ),
    verbose=verbose
  )
  class(obj) <- "UBaymodel"
  return(obj)
}
