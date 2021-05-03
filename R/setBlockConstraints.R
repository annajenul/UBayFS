#' set block constraints in UBaymodel object
#' @description sets the block constraints in a UBaymodel object
#' @param model a UBaymodel object created using build.UBaymodel
#' @param constraints a list containing a relaxed system Ax<=b of user constraints on feature blocks, given as matrix A, vector b and vector or scalar rho (relaxation parameters); see buildConstraints function
#' @return a UBaymodel object with updated constraint parameters
#' @seealso build.UBaymodel
#' @export

setBlockConstraints = function(model, constraints){

  if(class(model) != "UBaymodel"){
    stop("Error: wrong class of model")
  }

  if(!checkConstraints(constraints)){
    stop("Error: inconsistent constraints provided")
  }

  if(!is.null(constraints)){
    if(ncol(model$data) != ncol(constraints$block_matrix)){
      stop("Error: inconsistent constraints provided")
    }
    if(nrow(constraints$block_matrix) != ncol(constraints$A)){
      stop("Error: inconsistent constraints provided")
    }
  }

  model$user.params$block_constraints = constraints

  return(model)
}
