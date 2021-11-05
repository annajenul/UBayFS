#' set constraints in UBaymodel object
#' @description sets the constraints in a UBaymodel object
#' @param model a UBaymodel object created using build.UBaymodel
#' @param constraints a list containing a relaxed system Ax<=b of user constraints, given as matrix A, vector b and vector or scalar rho (relaxation parameters); see buildConstraints function
#' @return a UBaymodel object with updated constraint parameters
#' @seealso build.UBaymodel
#' @export

setConstraints = function(model, constraints, append = FALSE){

  if(class(model) != "UBaymodel"){
    stop("Error: wrong class of model")
  }

  if(!checkConstraints(constraints)){
    stop("Error: inconsistent constraints provided")
  }

  if(!is.null(constraints)){
    if(ncol(model$data) != ncol(constraints$A)){
      stop("Error: inconsistent constraints provided")
    }
  }

  if(append){
    const <- model$user.params$constraints
    constraints$A <- rbind(const$A, constraints$A)
    constraints$b <- c(const$b, constraints$b)
    constraints$rho <- c(const$rho, constraints$rho)
    constraints$block_matrix <- rbind(const$block_matrix, constraints$block_matrix)
  }
  model$user.params$constraints = constraints

  return(model)
}
