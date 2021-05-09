#' feature selection results
#' @description displays and summarizes the results of UBayFS after feature selection.
#' @param model a UBaymodel object created using build.UBaymodel after training
#' @importFrom knitr kable
#' @importFrom DirichletReg ddirichlet
#' @export

getResults <- function(model){

  map = model$output$map
  rownames(map) = NULL
  NO = 1:nrow(map)
  post_param = model$user.params$weights + model$ensemble.params$output$counts

  feature_sets = apply(map, 1, function(x){
    paste0(which(x == 1), collapse = ",")
  })

  feature_size = apply(map, 1, sum)
  log_posterior = round(apply(map,
                              1,
                              getPosterior,
                              model = model,
                              log = TRUE), 2)

  log_admissibility = round(apply(map,
                                  1,
                                  admissibility,
                                  constraints = model$user.params$constraints,
                                  weights_sum = sum(post_param),
                                  log=TRUE), 2)

  log_block_admissibility = round(apply(map,
                                        1,
                                        block_admissibility,
                                        constraints = model$user.params$block_constraints,
                                        weights_sum = sum(post_param) / nrow(model$user.params$block_constraints$block_matrix),
                                        log=TRUE), 2)

  log_dirichlet = round(ddirichlet(as.matrix(map + 0.01),
                                   alpha=post_param,
                                   log=TRUE), 2)

  df = cbind(NO, feature_sets,
             feature_size,
             log_posterior,
             log_admissibility,
             log_block_admissibility,
             log_dirichlet)
  print("MAP feature sets")
  print(kable(df))

  if(length(NO) > 1){
    similarity_matrix = matrix(1, nrow = length(NO), ncol = length(NO))
    for (i in NO) {
      for (j in NO[NO > i]) {
        similarity_matrix[i,j] = length(intersect(which(map[i,] == 1), which(map[j,] ==1))) /
                                 length(union(which(map[i,] == 1), which(map[j,] == 1)))
        similarity_matrix[j,i] = similarity_matrix[i,j]
      }
    }
    similarity_matrix = round(similarity_matrix, 2)
    rownames(similarity_matrix) = NO
    colnames(similarity_matrix) = NO
    print("MAP feature set similarities")
    print(similarity_matrix)
  }
}
