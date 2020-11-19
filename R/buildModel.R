#' @import Rdimtools
#' @import caret
#' @import glmnet
#' @import mRMRe
#' @export


build.model <- function(data, target, reg.param = NULL, K = 100,
                        testsize = 0.25, A = NULL, b = NULL, rho = NULL, verbose = TRUE,
                        alpha0 = c(5,1), beta0 = c(1,5), nr_features = 10, ranking = TRUE,
                        method = c("laplace", "fisher", "mrmr", "RENT")){

  if(!is.matrix(data)){
    data <- as.matrix(data)
  }
  if(is.null(reg.param)){
    print("auto-selecting regularization paramters")
    reg.param <- select.reg.param(data, target, model.type)
  }

  rank_matrix = c()

  max_counts = ifelse(ranking, length(method) * K * nr_features, length(method) * K)

  for(i in 1:K){
    train_index <- createDataPartition(target, p = .75, list = FALSE)
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
        dat = data.frame("class"=train_labels, train_data)
        dat$class = factor(dat$class, ordered=TRUE)
        rs = mRMR.classic(data=mRMR.data(dat), target_indices = c(1), feature_count = nr_features)
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
      else{
        stop(paste0("Error: unknown method", f))
      }
      vec <- rep(0, ncol(train_data))
      if(ranking){
        vec[ranks] <- nr_features : 1
      }
      else{
        vec[ranks] <- 1
      }
      rank_matrix <- rbind(rank_matrix, vec)
      rownames(rank_matrix)[nrow(rank_matrix)] <- paste(f, i)
    }
  }

  full_counts = rank_matrix
  counts = colSums(rank_matrix)

  obj <- list(
    data=data,
    target=target,
    prior.params = list(A=A,
                        b=b,
                        rho=rho),
    likelihood.params = list(testsize=testsize,
                             K=K,
                             reg.param=reg.param,
                             full_counts = full_counts,
                             counts = counts,
                             max_counts = max_counts,
                             alpha0=alpha0,
                             beta0=beta0,
                             method=method),
    verbose=verbose
  )
  class(obj) <- "UBaymodel"
  return(obj)
}
