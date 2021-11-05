#' @export

buildDecorrConstraints = function(data, level = 0.5, method = "kendall"){

  corr_matrix = cor(data, method = method)
  corr_matrix <- abs(corr_matrix)
  num_features = ncol(corr_matrix)

  A_corr <- matrix(0, nrow = choose(num_features, 2), ncol = num_features)
  A_corr[cbind(rep(1:choose(num_features,2), each = 2), as.vector(combn(num_features,2)))] <- 1
  b_corr <- rep(1, choose(num_features, 2))
  rho_corr <- corr_matrix[lower.tri(corr_matrix)]

  #restrict to positive rho
  pos_corr <- rho_corr > level
  A_corr <- A_corr[pos_corr,]
  b_corr <- b_corr[pos_corr]
  rho_corr <- rho_corr[pos_corr]
  rho_corr <- log( rho_corr / (1-rho_corr)) # logit function # TODO: transform for level != 0.5!

  const <- list(A = A_corr,
                b = b_corr,
                rho = rho_corr,
                block_matrix = NULL)

  return(
    const
  )
}
