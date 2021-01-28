#' set constraints in UBaymodel object
#' @description sets the constraints in a UBaymodel object
#' @param model a UBaymodel object created using build.model
#' @param A the matrix defining the constraint system Ax<=b
#' @param b the vector defining the constraint system Ax<=b
#' @param rho the vector of relaxation parameters for the constraint system Ax<=b
#' @return a UBaymodel object with updated constraint parameters
#' @seealso build.model
#' @export

set_constraint_params = function(model, A, b, rho){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  if(ncol(A) != ncol(model$data) | nrow(A) != length(b) | length(b) != length(rho)){
    stop("Error: dimensions of constraints do not match")
  }

  model$user.params$constraints = list(A = A,
                                       b = b,
                                       rho = rho)

  return(model)
}
