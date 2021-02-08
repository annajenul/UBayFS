#' set constraints in UBaymodel object
#' @description sets the constraints in a UBaymodel object
#' @param model a UBaymodel object created using build.UBaymodel
#' @param constraints a list containing a relaxed system Ax<=b of user constraints, given as matrix A, vector b and vector or scalar rho (relaxation parameters); see buildConstraints function
#' @return a UBaymodel object with updated constraint parameters
#' @seealso build.UBaymodel
#' @export

setConstraints = function(model, constraints){

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

  model$user.params$constraints = constraints

  return(model)
}
